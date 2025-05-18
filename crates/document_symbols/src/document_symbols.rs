use std::ops::Range;
use std::{
    cmp::{self, Reverse},
    sync::Arc,
};

use editor::RowHighlightOptions;
use editor::{Anchor, AnchorRangeExt, Editor, scroll::Autoscroll};
use fuzzy::StringMatch;
use gpui::{
    App, Context, DismissEvent, Entity, EventEmitter, FocusHandle, Focusable, HighlightStyle,
    ParentElement, Point, Render, Styled, StyledText, Task, TextStyle, WeakEntity, Window, div,
    rems,
};
use language::{Outline, OutlineItem};
use ordered_float::OrderedFloat;
use picker::{Picker, PickerDelegate};
use settings::Settings;
use theme::{ActiveTheme, ThemeSettings};
use ui::{ListItem, ListItemSpacing, prelude::*};
use util::ResultExt;
use workspace::{DismissDecision, ModalView, Workspace};
use zed_actions::document_symbols::Toggle;

pub fn init(cx: &mut App) {
    cx.observe_new(DocumentSymbolView::register).detach();
}

pub struct DocumentSymbolView {
    picker: Entity<Picker<DocumentSymbolViewDelegate>>,
}

impl Focusable for DocumentSymbolView {
    fn focus_handle(&self, cx: &App) -> FocusHandle {
        self.picker.focus_handle(cx)
    }
}

impl EventEmitter<DismissEvent> for DocumentSymbolView {}
impl ModalView for DocumentSymbolView {
    fn on_before_dismiss(
        &mut self,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> DismissDecision {
        self.picker.update(cx, |picker, cx| {
            picker.delegate.restore_active_editor(window, cx)
        });
        DismissDecision::Dismiss(true)
    }
}

impl Render for DocumentSymbolView {
    fn render(&mut self, _window: &mut Window, _cx: &mut Context<Self>) -> impl IntoElement {
        v_flex().w(rems(34.)).child(self.picker.clone())
    }
}

impl DocumentSymbolView {
    fn register(editor: &mut Editor, _: Option<&mut Window>, cx: &mut Context<Editor>) {
        if editor.mode().is_full() {
            let handle = cx.entity().downgrade();
            editor
                .register_action(move |action, window, cx| {
                    if let Some(editor) = handle.upgrade() {
                        Self::open(editor, action, window, cx);
                    }
                })
                .detach();
        }
    }

    fn open(
        workspace: &mut Workspace,
        editor: Entity<Editor>,
        window: &mut Window,
        cx: &mut App,
    ) -> Task<()> {
        let buffer = editor.read(cx).buffer().read(cx).as_singleton()?;
        let document_symbols = editor
            .read(cx)
            .semantics_provider()
            .as_ref()?
            .document_symbols(&buffer, cx)?;

        cx.spawn_in(window, async move |workspace, cx| {
            let document_symbols = document_symbols.await.ok()?;
            workspace.update_in(cx, |workspace, window, cx| {
                workspace
                    .toggle_modal(window, cx, |window, cx| {
                        DocumentSymbolView::new(document_symbols, editor, window, cx)
                    })
                    .ok();
            })
        });
    }

    fn new(
        outline: Outline<Anchor>,
        editor: Entity<Editor>,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> DocumentSymbolView {
        let delegate =
            DocumentSymbolViewDelegate::new(cx.entity().downgrade(), outline, editor, cx);
        let picker = cx.new(|cx| {
            Picker::uniform_list(delegate, window, cx).max_height(Some(vh(0.75, window)))
        });
        DocumentSymbolView { picker }
    }
}

struct DocumentSymbolViewDelegate {
    outline_view: WeakEntity<DocumentSymbolView>,
    active_editor: Entity<Editor>,
    outline: Outline<Anchor>,
    selected_match_index: usize,
    prev_scroll_position: Option<Point<f32>>,
    matches: Vec<StringMatch>,
    last_query: String,
}

enum OutlineRowHighlights {}

impl DocumentSymbolViewDelegate {
    fn new(
        outline_view: WeakEntity<DocumentSymbolView>,
        outline: Outline<Anchor>,
        editor: Entity<Editor>,

        cx: &mut Context<DocumentSymbolView>,
    ) -> Self {
        Self {
            outline_view,
            last_query: Default::default(),
            matches: Default::default(),
            selected_match_index: 0,
            prev_scroll_position: Some(editor.update(cx, |editor, cx| editor.scroll_position(cx))),
            active_editor: editor,
            outline,
        }
    }

    fn restore_active_editor(&mut self, window: &mut Window, cx: &mut App) {
        self.active_editor.update(cx, |editor, cx| {
            editor.clear_row_highlights::<OutlineRowHighlights>();
            if let Some(scroll_position) = self.prev_scroll_position {
                editor.set_scroll_position(scroll_position, window, cx);
            }
        })
    }

    fn set_selected_index(
        &mut self,
        ix: usize,
        navigate: bool,

        cx: &mut Context<Picker<DocumentSymbolViewDelegate>>,
    ) {
        self.selected_match_index = ix;

        if navigate && !self.matches.is_empty() {
            let selected_match = &self.matches[self.selected_match_index];
            let outline_item = &self.outline.items[selected_match.candidate_id];

            self.active_editor.update(cx, |active_editor, cx| {
                active_editor.clear_row_highlights::<OutlineRowHighlights>();
                active_editor.highlight_rows::<OutlineRowHighlights>(
                    outline_item.range.start..outline_item.range.end,
                    cx.theme().colors().editor_highlighted_line_background,
                    RowHighlightOptions {
                        autoscroll: true,
                        ..Default::default()
                    },
                    cx,
                );
                active_editor.request_autoscroll(Autoscroll::center(), cx);
            });
        }
    }
}

impl PickerDelegate for DocumentSymbolViewDelegate {
    type ListItem = ListItem;

    fn placeholder_text(&self, _window: &mut Window, _cx: &mut App) -> Arc<str> {
        "Search buffer symbols...".into()
    }

    fn match_count(&self) -> usize {
        self.matches.len()
    }

    fn selected_index(&self) -> usize {
        self.selected_match_index
    }

    fn set_selected_index(
        &mut self,
        ix: usize,
        _: &mut Window,
        cx: &mut Context<Picker<DocumentSymbolViewDelegate>>,
    ) {
        self.set_selected_index(ix, true, cx);
    }

    fn update_matches(
        &mut self,
        query: String,
        window: &mut Window,
        cx: &mut Context<Picker<DocumentSymbolViewDelegate>>,
    ) -> Task<()> {
        let selected_index;
        if query.is_empty() {
            self.restore_active_editor(window, cx);
            self.matches = self
                .outline
                .items
                .iter()
                .enumerate()
                .map(|(index, _)| StringMatch {
                    candidate_id: index,
                    score: Default::default(),
                    positions: Default::default(),
                    string: Default::default(),
                })
                .collect();

            let (buffer, cursor_offset) = self.active_editor.update(cx, |editor, cx| {
                let buffer = editor.buffer().read(cx).snapshot(cx);
                let cursor_offset = editor.selections.newest::<usize>(cx).head();
                (buffer, cursor_offset)
            });
            selected_index = self
                .outline
                .items
                .iter()
                .enumerate()
                .map(|(ix, item)| {
                    let range = item.range.to_offset(&buffer);
                    let distance_to_closest_endpoint = cmp::min(
                        (range.start as isize - cursor_offset as isize).abs(),
                        (range.end as isize - cursor_offset as isize).abs(),
                    );
                    let depth = if range.contains(&cursor_offset) {
                        Some(item.depth)
                    } else {
                        None
                    };
                    (ix, depth, distance_to_closest_endpoint)
                })
                .max_by_key(|(_, depth, distance)| (*depth, Reverse(*distance)))
                .map(|(ix, _, _)| ix)
                .unwrap_or(0);
        } else {
            self.matches = smol::block_on(
                self.outline
                    .search(&query, cx.background_executor().clone()),
            );
            selected_index = self
                .matches
                .iter()
                .enumerate()
                .max_by_key(|(_, m)| OrderedFloat(m.score))
                .map(|(ix, _)| ix)
                .unwrap_or(0);
        }
        self.last_query = query;
        self.set_selected_index(selected_index, !self.last_query.is_empty(), cx);
        Task::ready(())
    }

    fn confirm(
        &mut self,
        _: bool,
        window: &mut Window,
        cx: &mut Context<Picker<DocumentSymbolViewDelegate>>,
    ) {
        self.prev_scroll_position.take();
        self.set_selected_index(self.selected_match_index, true, cx);

        self.active_editor.update(cx, |active_editor, cx| {
            let highlight = active_editor
                .highlighted_rows::<OutlineRowHighlights>()
                .next();
            if let Some((rows, _)) = highlight {
                active_editor.change_selections(Some(Autoscroll::center()), window, cx, |s| {
                    s.select_ranges([rows.start..rows.start])
                });
                active_editor.clear_row_highlights::<OutlineRowHighlights>();
                window.focus(&active_editor.focus_handle(cx));
            }
        });

        self.dismissed(window, cx);
    }

    fn dismissed(
        &mut self,
        window: &mut Window,
        cx: &mut Context<Picker<DocumentSymbolViewDelegate>>,
    ) {
        self.outline_view
            .update(cx, |_, cx| cx.emit(DismissEvent))
            .log_err();
        self.restore_active_editor(window, cx);
    }

    fn render_match(
        &self,
        ix: usize,
        selected: bool,
        _: &mut Window,
        cx: &mut Context<Picker<Self>>,
    ) -> Option<Self::ListItem> {
        let mat = self.matches.get(ix)?;
        let outline_item = self.outline.items.get(mat.candidate_id)?;

        Some(
            ListItem::new(ix)
                .inset(true)
                .spacing(ListItemSpacing::Sparse)
                .toggle_state(selected)
                .child(
                    div()
                        .text_ui(cx)
                        .pl(rems(outline_item.depth as f32))
                        .child(render_item(outline_item, mat.ranges(), cx)),
                ),
        )
    }
}

pub fn render_item<T>(
    outline_item: &OutlineItem<T>,
    match_ranges: impl IntoIterator<Item = Range<usize>>,
    cx: &App,
) -> StyledText {
    let highlight_style = HighlightStyle {
        background_color: Some(cx.theme().colors().text_accent.alpha(0.3)),
        ..Default::default()
    };
    let custom_highlights = match_ranges
        .into_iter()
        .map(|range| (range, highlight_style));

    let settings = ThemeSettings::get_global(cx);

    // TODO: We probably shouldn't need to build a whole new text style here
    // but I'm not sure how to get the current one and modify it.
    // Before this change TextStyle::default() was used here, which was giving us the wrong font and text color.
    let text_style = TextStyle {
        color: cx.theme().colors().text,
        font_family: settings.buffer_font.family.clone(),
        font_features: settings.buffer_font.features.clone(),
        font_fallbacks: settings.buffer_font.fallbacks.clone(),
        font_size: settings.buffer_font_size(cx).into(),
        font_weight: settings.buffer_font.weight,
        line_height: relative(1.),
        ..Default::default()
    };
    let highlights = gpui::combine_highlights(
        custom_highlights,
        outline_item.highlight_ranges.iter().cloned(),
    );

    StyledText::new(outline_item.text.clone()).with_default_highlights(&text_style, highlights)
}
