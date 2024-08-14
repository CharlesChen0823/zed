mod popover;
mod scrollbar;
use editor::Editor;
use gpui::{
    impl_actions, AppContext, Element, EventEmitter, IntoElement, ParentElement, Render,
    StyledText, Subscription, ViewContext,
};
use itertools::Itertools;
use popover::*;
use serde::Deserialize;
use std::path::PathBuf;
use theme::ActiveTheme;
use ui::{prelude::*, ButtonLike, ButtonStyle, Label, ToggleButton, Tooltip};
use workspace::{
    item::{BreadcrumbItem, ItemEvent, ItemHandle},
    ToolbarItemEvent, ToolbarItemLocation, ToolbarItemView,
};

#[derive(PartialEq, Clone, Deserialize, Default)]
struct TogglePopover {
    pub(crate) target_path: PathBuf,
}

impl_actions!(breadcrumbs, [TogglePopover]);

pub struct Breadcrumbs {
    pane_focused: bool,
    active_item: Option<Box<dyn ItemHandle>>,
    subscription: Option<Subscription>,
}

pub fn init(cx: &mut AppContext) {
    let _ = cx
        .observe_new_views(|workspace, cx| {
            Popover::register(workspace, cx);
        })
        .detach();
}

impl Breadcrumbs {
    pub fn new() -> Self {
        Self {
            pane_focused: false,
            active_item: Default::default(),
            subscription: Default::default(),
        }
    }
}

impl EventEmitter<ToolbarItemEvent> for Breadcrumbs {}

impl Render for Breadcrumbs {
    fn render(&mut self, cx: &mut ViewContext<Self>) -> impl IntoElement {
        let element = h_flex().text_ui(cx);
        let Some(active_item) = self.active_item.as_ref() else {
            return element;
        };
        let Some(segments) = active_item.breadcrumbs(cx.theme(), cx) else {
            return element;
        };

        let mut path_click_button = Vec::new();
        let mut outline_button = Vec::new();

        for segment in segments.into_iter() {
            match segment {
                BreadcrumbItem::FileItem { path, root, .. } => {
                    let mut pre = PathBuf::from("");
                    let mut id = 1;
                    let root_path = PathBuf::from(root.clone());
                    for path in &path.split('/').collect_vec() {
                        let root_path = root_path.clone();
                        let root = PathBuf::from(root_path.clone())
                            .join(pre.clone())
                            .join(path);
                        pre = pre.join(path);
                        let button = ToggleButton::new(id, path.to_string())
                            .on_click(move |_, cx| {
                                cx.dispatch_action(Box::new(TogglePopover {
                                    target_path: root.clone(),
                                }))
                            })
                            .into_any_element();
                        path_click_button.push(button);
                        id += 1;
                    }
                }
                BreadcrumbItem::OutLineItem {
                    text,
                    highlights,
                    font,
                } => {
                    let mut text_style = cx.text_style();
                    text_style.font_family = font.family;
                    text_style.font_features = font.features;
                    text_style.font_style = font.style;
                    text_style.font_weight = font.weight;
                    text_style.color = Color::Muted.color(cx);
                    let style_text = StyledText::new(text.replace('\n', "␤"))
                        .with_highlights(&text_style, highlights)
                        .into_any();
                    outline_button.push(style_text);
                }
            }
        }

        path_click_button.extend(outline_button);
        let highlighted_segments = path_click_button.into_iter();
        let breadcrumbs = Itertools::intersperse_with(highlighted_segments, || {
            Label::new("›").color(Color::Placeholder).into_any_element()
        });

        let breadcrumbs_stack = h_flex().gap_1().children(breadcrumbs);
        match active_item
            .downcast::<Editor>()
            .map(|editor| editor.downgrade())
        {
            Some(editor) => element.child(
                ButtonLike::new("toggle outline view")
                    .child(breadcrumbs_stack)
                    .style(ButtonStyle::Transparent)
                    .on_click(move |_, cx| {
                        if let Some(editor) = editor.upgrade() {
                            outline::toggle(editor, &editor::actions::ToggleOutline, cx)
                        }
                    })
                    .tooltip(|cx| {
                        Tooltip::for_action(
                            "Show symbol outline",
                            &editor::actions::ToggleOutline,
                            cx,
                        )
                    }),
            ),
            None => element
                // Match the height of the `ButtonLike` in the other arm.
                .h(rems_from_px(22.))
                .child(breadcrumbs_stack),
        }
    }
}

impl ToolbarItemView for Breadcrumbs {
    fn set_active_pane_item(
        &mut self,
        active_pane_item: Option<&dyn ItemHandle>,
        cx: &mut ViewContext<Self>,
    ) -> ToolbarItemLocation {
        cx.notify();
        self.active_item = None;

        let Some(item) = active_pane_item else {
            return ToolbarItemLocation::Hidden;
        };

        let this = cx.view().downgrade();
        self.subscription = Some(item.subscribe_to_item_events(
            cx,
            Box::new(move |event, cx| {
                if let ItemEvent::UpdateBreadcrumbs = event {
                    this.update(cx, |this, cx| {
                        cx.notify();
                        if let Some(active_item) = this.active_item.as_ref() {
                            cx.emit(ToolbarItemEvent::ChangeLocation(
                                active_item.breadcrumb_location(cx),
                            ))
                        }
                    })
                    .ok();
                }
            }),
        ));
        self.active_item = Some(item.boxed_clone());
        item.breadcrumb_location(cx)
    }

    fn pane_focus_update(&mut self, pane_focused: bool, _: &mut ViewContext<Self>) {
        self.pane_focused = pane_focused;
    }
}
