use gpui::{
    App, Context, DismissEvent, Entity, EventEmitter, FocusHandle, Focusable,
    InteractiveElement as _, IntoElement, Render, WeakEntity, Window,
};
use picker::{Picker, PickerDelegate};
use std::{path::PathBuf, sync::Arc};
use ui::{ListItem, ListItemSpacing, prelude::*, v_flex};
use util::{ResultExt, maybe};
use workspace::{ModalView, Workspace};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Operation {
    Save,
    Renamed,
    Moved,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FileEntry {
    name: PathBuf,
    op_source: Operation,
    timestamp: u64,
}

impl ModalView for FileHistory {}

pub struct FileHistory {
    picker: Entity<Picker<FileHistoryDelegate>>,
}

impl FileHistory {
    fn new(query: &str, window: &mut Window, cx: &mut Context<Self>) -> Self {
        let matches = Vec::new();
        let delegate = FileHistoryDelegate::new(cx.entity().downgrade(), matches);
        let picker = cx.new(|cx| {
            let picker = Picker::uniform_list(delegate, window, cx);
            picker
        });
        Self { picker }
    }
}

impl EventEmitter<DismissEvent> for FileHistory {}

impl Focusable for FileHistory {
    fn focus_handle(&self, cx: &App) -> FocusHandle {
        self.picker.focus_handle(cx)
    }
}

impl Render for FileHistory {
    fn render(&mut self, _: &mut Window, _: &mut Context<Self>) -> impl IntoElement {
        v_flex()
            .key_context("FileHistory")
            .w(rems(34.0))
            .child(self.picker.clone())
    }
}

pub struct FileHistoryDelegate {
    file_history: WeakEntity<FileHistory>,
    workspace: WeakEntity<Workspace>,
    project: WeakEntity<Project>,
    file_resource_url: PathBuf,
    matches: Vec<FileEntry>,
    selected_ix: usize,
    same_current_project: bool,
}

impl FileHistoryDelegate {
    fn new(
        file_history: WeakEntity<FileHistory>,
        workspace: WeakEntity<Workspace>,
        project: WeakEntity<Project>,
        file_base_path: PathBuf,
        matches: Vec<FileEntry>,
        same_current_project: bool,
    ) -> Self {
        Self {
            file_history,
            workspace,
            project,
            file_resource_url: file_base_path,
            matches,
            selected_ix: 0,
            same_current_project,
        }
    }
}

impl PickerDelegate for FileHistoryDelegate {
    type ListItem = ListItem;

    fn placeholder_text(&self, _window: &mut Window, _cx: &mut App) -> Arc<str> {
        "Select file to restore ...".into()
    }

    fn match_count(&self) -> usize {
        self.matches.len()
    }

    fn selected_index(&self) -> usize {
        self.selected_ix
    }

    fn set_selected_index(
        &mut self,
        ix: usize,
        _window: &mut Window,
        cx: &mut Context<Picker<Self>>,
    ) {
        self.selected_ix = ix;
        cx.notify();
    }

    fn update_matches(
        &mut self,
        mut query: String,
        window: &mut Window,
        cx: &mut Context<Picker<Self>>,
    ) -> gpui::Task<()> {
        todo!()
    }

    fn dismissed(&mut self, _window: &mut Window, cx: &mut Context<Picker<Self>>) {
        self.file_history
            .update(cx, |_, cx| cx.emit(DismissEvent))
            .log_err();
    }

    fn confirm(&mut self, _: bool, window: &mut Window, cx: &mut Context<Picker<Self>>) {
        if self.same_current_project {
            // open with buff diff
        } else {
            // directly open file with read-only mode
        }
    }

    fn render_match(
        &self,
        ix: usize,
        selected: bool,
        window: &mut Window,
        cx: &mut Context<Picker<Self>>,
    ) -> Option<Self::ListItem> {
        let file_icon = maybe!({
            let file_name = self.file_resource_url.file_name()?;
            let icon = FileIcons::get_icon(file_name.as_ref(), cx)?;
            Some(Icon::from_path(icon).color(Color::Muted));
        });
        Some(
            ListItem::new(ix)
                .spacing(ListItemSpacing::Sparse)
                .start_slot::<Icon>(file_icon)
                .end_slot::<AnyElement>(history_icon)
                .inset(true)
                .toggle_state(selected)
                .child(
                    h_flex().gap_2().py_px(), // .child(file_name_label)
                                              // .child(full_path_label),
                ),
        )
    }
}
