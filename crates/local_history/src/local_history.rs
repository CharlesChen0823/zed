use std::sync::Arc;

use gpui::{
    App, Context, DismissEvent, Entity, EventEmitter, Focusable, IntoElement, Render, WeakEntity,
    Window,
};
use picker::{Picker, PickerDelegate};
use ui::{ListItem, prelude::*, v_flex};
use util::ResultExt;
use workspace::ModalView;

use crate::file_history::FileHistory;

mod file_history;

pub struct HistoryEntry {}

// todo!()
// 1.
pub struct LocalHistory {
    picker: Entity<Picker<LocalHistoryDelegate>>,
}

impl ModalView for LocalHistory {}

impl EventEmitter<DismissEvent> for LocalHistory {}

impl Focusable for LocalHistory {
    fn focus_handle(&self, cx: &App) -> gpui::FocusHandle {
        self.picker.focus_handle(cx)
    }
}

impl Render for LocalHistory {
    fn render(&mut self, _: &mut Window, _: &mut Context<Self>) -> impl IntoElement {
        v_flex()
            .key_context("LocalHistory")
            .w(rems(34.0))
            .child(self.picker.clone())
    }
}

// todo
// 1. list all local history entries
// 2. render history entry with (file_icon, file_name, file_directory)
// 3. select history entry then open file_history_delegate
pub struct LocalHistoryDelegate {
    local_history: WeakEntity<LocalHistory>,
    matches: Vec<HistoryEntry>,
    selected_ix: usize,
}

impl PickerDelegate for LocalHistoryDelegate {
    type ListItem = ListItem;

    fn placeholder_text(&self, _window: &mut Window, _cx: &mut App) -> Arc<str> {
        "Local History...".into()
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
        self.local_history
            .update(cx, |_, cx| cx.emit(DismissEvent))
            .log_err();
    }

    fn confirm(&mut self, _: bool, window: &mut Window, cx: &mut Context<Picker<Self>>) {
        // open file_history_delegate
        FileHistory::open(self.local_history.clone(), window, cx);
    }

    fn render_match(
        &self,
        ix: usize,
        selected: bool,
        window: &mut Window,
        cx: &mut Context<Picker<Self>>,
    ) -> Option<Self::ListItem> {
        todo!()
    }
}
