use crate::{scrollbar::Scrollbar, TogglePopover};

use editor::scroll::ScrollbarAutoHide;
use file_icons::FileIcons;

use client::{ErrorCode, ErrorExt};
use gpui::{
    div, px, uniform_list, AnyElement, AppContext, DismissEvent, Div, EventEmitter, FocusHandle,
    FocusableView, InteractiveElement, ListSizingBehavior, Model, MouseButton, ParentElement,
    Render, Stateful, Styled, Task, UniformListScrollHandle, ViewContext, WeakView,
};
use menu::{Confirm, SelectFirst, SelectLast, SelectNext, SelectPrev};
use project::{Entry, EntryKind, Project, ProjectEntryId, ProjectPath, WorktreeId};
use std::{
    cell::Cell,
    collections::HashSet,
    ffi::OsStr,
    ops::Range,
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
    time::Duration,
};
use ui::{prelude::*, Icon, ListItem, Tooltip};
use util::ResultExt;
use workspace::{notifications::DetachAndPromptErr, ModalView, Workspace};

pub struct Popover {
    workspace: WeakView<Workspace>,
    project: Model<Project>,
    scroll_handle: UniformListScrollHandle,
    focus_handle: FocusHandle,
    worktree_id: WorktreeId,
    visible_entries: Vec<Entry>,
    expanded_dir_ids: Vec<ProjectEntryId>,
    current_entry: ProjectEntryId,
    selection: Option<ProjectEntryId>,
    show_scrollbar: bool,
    scrollbar_drag_thumb_offset: Rc<Cell<Option<f32>>>,
    hide_scrollbar_task: Option<Task<()>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EntryDetails {
    filename: String,
    icon: Option<SharedString>,
    path: Arc<Path>,
    depth: usize,
    kind: EntryKind,
    is_expanded: bool,
    is_selected: bool,
    canonical_path: Option<Box<Path>>,
}

#[derive(Debug)]
pub enum Event {
    Focus,
}

impl EventEmitter<Event> for Popover {}

impl EventEmitter<DismissEvent> for Popover {}

impl FocusableView for Popover {
    fn focus_handle(&self, _cx: &AppContext) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl ModalView for Popover {}

impl Popover {
    pub fn register(workspace: &mut Workspace, cx: &mut ViewContext<Workspace>) {
        let _handle = cx.view().downgrade();
        workspace.register_action(move |workspace, _: &TogglePopover, cx| {
            let project = workspace.project().clone();
            cx.spawn(|workspace, mut cx| async move {
                workspace.update(&mut cx, |workspace, cx| {
                    let weak_workspace = cx.view().downgrade();
                    workspace.toggle_modal(cx, move |cx| {
                        Popover::new(
                            weak_workspace,
                            project,
                            "/home/charles/program/python/django/django/db/transaction.py".into(),
                            cx,
                        )
                    });
                })?;
                anyhow::Ok(())
            })
            .detach_and_log_err(cx);
        });
    }
    fn new(
        workspace: WeakView<Workspace>,
        project: Model<Project>,
        target_path: PathBuf,
        cx: &mut ViewContext<Self>,
    ) -> Self {
        let (worktree, path) = project.read(cx).find_worktree(&target_path, cx).unwrap();
        let entry = worktree.read(cx).entry_for_path(path).unwrap().clone();
        let focus_handle = cx.focus_handle();
        cx.on_focus(&focus_handle, Self::focus_in).detach();

        cx.observe_global::<FileIcons>(|_, cx| {
            cx.notify();
        })
        .detach();

        let mut this = Self {
            workspace,
            project: project.clone(),
            scroll_handle: UniformListScrollHandle::new(),
            focus_handle,
            worktree_id: worktree.read(cx).id(),
            visible_entries: Default::default(),
            expanded_dir_ids: Default::default(),
            current_entry: entry.id,
            selection: None,
            show_scrollbar: !Self::should_autohide_scrollbar(cx),
            hide_scrollbar_task: None,
            scrollbar_drag_thumb_offset: Default::default(),
        };
        this.update_visible_entries(None, cx);

        this
    }

    fn focus_in(&mut self, cx: &mut ViewContext<Self>) {
        if !self.focus_handle.contains_focused(cx) {
            cx.emit(Event::Focus);
        }
    }

    fn should_autohide_scrollbar(cx: &AppContext) -> bool {
        cx.try_global::<ScrollbarAutoHide>()
            .map_or_else(|| cx.should_auto_hide_scrollbars(), |autohide| autohide.0)
    }

    fn hide_scrollbar(&mut self, cx: &mut ViewContext<Self>) {
        const SCROLLBAR_SHOW_INTERVAL: Duration = Duration::from_secs(1);
        if !Self::should_autohide_scrollbar(cx) {
            return;
        }
        self.hide_scrollbar_task = Some(cx.spawn(|panel, mut cx| async move {
            cx.background_executor()
                .timer(SCROLLBAR_SHOW_INTERVAL)
                .await;
            panel
                .update(&mut cx, |panel, cx| {
                    panel.show_scrollbar = false;
                    cx.notify();
                })
                .log_err();
        }))
    }

    fn toggle_expanded(&mut self, entry_id: ProjectEntryId, cx: &mut ViewContext<Self>) {
        match self.expanded_dir_ids.binary_search(&entry_id) {
            Ok(ix) => {
                self.expanded_dir_ids.remove(ix);
            }
            Err(ix) => {
                self.expanded_dir_ids.insert(ix, entry_id);
            }
        }
        self.update_visible_entries(Some(entry_id), cx);
        cx.focus(&self.focus_handle);
        cx.notify();
    }

    fn select_prev(&mut self, _: &SelectPrev, cx: &mut ViewContext<Self>) {
        if let Some(selection) = self.selection {
            let mut entry_ix = self.index_for_selection(selection).unwrap_or_default();
            if entry_ix > 0 {
                entry_ix -= 1;
            } else {
                return;
            }

            let entry_id = &self.visible_entries[entry_ix];
            self.selection = Some(entry_id.id);
            self.autoscroll(cx);
            cx.notify();
        }
    }

    fn cancel(&mut self, _: &menu::Cancel, cx: &mut ViewContext<Self>) {
        cx.emit(DismissEvent);
    }

    fn open_entry(&mut self, entry_id: ProjectEntryId, cx: &mut ViewContext<Self>) {
        if let Some(worktree) = self.project.read(cx).worktree_for_entry(entry_id, cx) {
            if let Some(entry) = worktree.read(cx).entry_for_id(entry_id) {
                let worktree_id = worktree.read(cx).id();
                let file_path = entry.path.clone();
                if let Some(workspace) = self.workspace.upgrade() {
                    workspace.update(cx, |workspace, cx| {
                    workspace.open_path_preview(
                        ProjectPath {
                            worktree_id,
                            path: file_path.clone(),
                        },
                        None,
                        true,
                        true,
                        cx,
                    )
                    .detach_and_prompt_err("Failed to open file", cx, move |e, _| {
                        match e.error_code() {
                            ErrorCode::Disconnected => Some("Disconnected from remote project".to_string()),
                            ErrorCode::UnsharedItem => Some(format!(
                                "{} is not shared by the host. This could be because it has been marked as `private`",
                                file_path.display()
                            )),
                            _ => None,
                        }
                    });
                });
                }
            }
        }
        cx.emit(DismissEvent);
    }

    fn split_entry(&mut self, entry_id: ProjectEntryId, cx: &mut ViewContext<Self>) {
        if let Some(worktree) = self.project.read(cx).worktree_for_entry(entry_id, cx) {
            if let Some(entry) = worktree.read(cx).entry_for_id(entry_id) {
                let worktree_id = worktree.read(cx).id();
                let file_path = entry.path.clone();
                if let Some(workspace) = self.workspace.upgrade() {
                    workspace.update(cx, |workspace, cx| {
                        workspace
                            .split_path(
                                ProjectPath {
                                    worktree_id,
                                    path: file_path.clone(),
                                },
                                cx,
                            )
                            .detach_and_log_err(cx);
                    });
                }
            }
        }
        cx.emit(DismissEvent);
    }

    fn select_next(&mut self, _: &SelectNext, cx: &mut ViewContext<Self>) {
        if let Some(selection) = self.selection {
            let mut entry_ix = self.index_for_selection(selection).unwrap_or_default();
            if entry_ix + 1 < self.visible_entries.len() {
                entry_ix += 1;
            } else {
                return;
            }
            let entry = &self.visible_entries[entry_ix];
            self.selection = Some(entry.id);
            self.autoscroll(cx);
            cx.notify();
        }
    }

    fn select_first(&mut self, _: &SelectFirst, cx: &mut ViewContext<Self>) {
        if let Some(entry) = self.visible_entries.first() {
            self.selection = Some(entry.id);
            self.autoscroll(cx);
            cx.notify();
        }
    }

    fn select_last(&mut self, _: &SelectLast, cx: &mut ViewContext<Self>) {
        if let Some(entry) = self.visible_entries.last() {
            self.selection = Some(entry.id);
            self.autoscroll(cx);
            cx.notify();
        }
    }

    fn autoscroll(&mut self, cx: &mut ViewContext<Self>) {
        if let Some(index) = self.selection.and_then(|s| self.index_for_selection(s)) {
            self.scroll_handle.scroll_to_item(index);
            cx.notify();
        }
    }

    fn index_for_selection(&self, selection: ProjectEntryId) -> Option<usize> {
        for (idx, entry) in self.visible_entries.iter().enumerate() {
            if entry.id == selection {
                return Some(idx);
            }
        }
        None
    }

    fn selected_entry_handle<'a>(&self, cx: &'a AppContext) -> Option<&'a project::Entry> {
        let selection = self.selection?;
        let project = self.project.read(cx);
        let worktree = project.worktree_for_id(self.worktree_id, cx)?;
        let entry = worktree.read(cx).entry_for_id(selection)?;
        Some(entry)
    }

    fn update_visible_entries(
        &mut self,
        new_selected_entry: Option<ProjectEntryId>,
        cx: &mut ViewContext<Self>,
    ) {
        self.visible_entries.clear();
        let worktree = self
            .project
            .read(cx)
            .worktree_for_id(self.worktree_id, cx)
            .unwrap();
        let snapshot = worktree.read(cx).snapshot();

        let mut visible_worktree_entries = Vec::new();
        let entry = worktree.read(cx).entry_for_id(self.current_entry).unwrap();
        let parent = entry.path.parent().unwrap();
        let mut entry_iter = snapshot.traverse_from_path(true, true, true, &parent);
        while let Some(entry) = entry_iter.entry() {
            visible_worktree_entries.push(entry.clone());
            if self.expanded_dir_ids.binary_search(&entry.id).is_err()
                && entry_iter.advance_to_sibling()
            {
                continue;
            }
            entry_iter.advance();
        }

        project::sort_worktree_entries(&mut visible_worktree_entries);
        self.visible_entries = visible_worktree_entries;
        self.selection = new_selected_entry;
    }

    fn confirm(&mut self, _: &Confirm, cx: &mut ViewContext<Self>) {
        if let Some(entry) = self.selected_entry_handle(cx) {
            if entry.is_dir() {
                self.toggle_expanded(entry.id, cx);
            } else {
                self.open_entry(entry.id, cx);
                cx.emit(DismissEvent);
            }
        }
    }

    fn for_each_visible_entry(
        &self,
        range: Range<usize>,
        cx: &mut ViewContext<Popover>,
        mut callback: impl FnMut(ProjectEntryId, EntryDetails, &mut ViewContext<Popover>),
    ) {
        if let Some(worktree) = self.project.read(cx).worktree_for_id(self.worktree_id, cx) {
            let snapshot = worktree.read(cx).snapshot();
            let root_name = OsStr::new(snapshot.root_name());
            for (index, entry) in self.visible_entries.iter().enumerate() {
                if index > range.end || index < range.start {
                    return;
                }

                let is_expanded = self.expanded_dir_ids.binary_search(&entry.id).is_ok();
                let icon = match entry.kind {
                    EntryKind::File(_) => FileIcons::get_icon(&entry.path, cx),
                    _ => FileIcons::get_chevron_icon(is_expanded, cx),
                };

                let entries = self
                    .visible_entries
                    .iter()
                    .map(|e| e.path.clone())
                    .collect();

                let (depth, difference) = Popover::calculate_depth_and_difference(entry, &entries);

                let filename = match difference {
                    diff if diff > 1 => entry
                        .path
                        .iter()
                        .skip(entry.path.components().count() - diff)
                        .collect::<PathBuf>()
                        .to_str()
                        .unwrap_or_default()
                        .to_string(),
                    _ => entry
                        .path
                        .file_name()
                        .map(|name| name.to_string_lossy().into_owned())
                        .unwrap_or_else(|| root_name.to_string_lossy().to_string()),
                };
                let details = EntryDetails {
                    filename,
                    icon,
                    path: entry.path.clone(),
                    depth,
                    kind: entry.kind,
                    is_expanded,
                    is_selected: self.selection == Some(entry.id),
                    canonical_path: entry.canonical_path.clone(),
                };

                callback(entry.id, details, cx);
            }
        }
    }

    fn calculate_depth_and_difference(
        entry: &Entry,
        visible_worktree_entries: &HashSet<Arc<Path>>,
    ) -> (usize, usize) {
        let (depth, difference) = entry
            .path
            .ancestors()
            .skip(1) // Skip the entry itself
            .find_map(|ancestor| {
                if let Some(parent_entry) = visible_worktree_entries.get(ancestor) {
                    let entry_path_components_count = entry.path.components().count();
                    let parent_path_components_count = parent_entry.components().count();
                    let difference = entry_path_components_count - parent_path_components_count;
                    let depth = parent_entry
                        .ancestors()
                        .skip(1)
                        .filter(|ancestor| visible_worktree_entries.contains(*ancestor))
                        .count();
                    Some((depth + 1, difference))
                } else {
                    None
                }
            })
            .unwrap_or((0, 0));

        (depth, difference)
    }

    fn render_entry(
        &self,
        entry_id: ProjectEntryId,
        details: EntryDetails,
        cx: &mut ViewContext<Self>,
    ) -> Stateful<Div> {
        let kind = details.kind;
        let is_active = self
            .selection
            .map_or(false, |selection| selection == entry_id);
        let icon = details.icon.clone();
        let file_name = details.filename.clone();

        let canonical_path = details
            .canonical_path
            .as_ref()
            .map(|f| f.to_string_lossy().to_string());

        let depth = details.depth;

        div()
            .elevation_3(cx)
            .id(entry_id.to_proto() as usize)
            .w_full()
            .child(
                ListItem::new(entry_id.to_proto() as usize)
                    .indent_level(depth)
                    .indent_step_size(px(1.0))
                    .selected(is_active)
                    .when_some(canonical_path, |this, path| {
                        this.end_slot::<AnyElement>(
                            div()
                                .id("symlink_icon")
                                .pr_3()
                                .tooltip(move |cx| {
                                    Tooltip::text(format!("{path} • Symbolic Link"), cx)
                                })
                                .into_any_element(),
                        )
                    })
                    .child(if let Some(icon) = &icon {
                        h_flex().child(Icon::from_path(icon.to_string()))
                    } else {
                        h_flex()
                            .size(IconSize::default().rems())
                            .invisible()
                            .flex_none()
                    })
                    .child(h_flex().h_6().child(Label::new(file_name).single_line()))
                    .on_click(cx.listener(move |this, event: &gpui::ClickEvent, cx| {
                        if event.down.button == MouseButton::Right || event.down.first_mouse {
                            return;
                        }
                        cx.stop_propagation();

                        if event.down.modifiers.secondary() {
                            this.split_entry(entry_id, cx);
                        } else if kind.is_dir() {
                            this.toggle_expanded(entry_id, cx);
                        } else {
                            this.open_entry(entry_id, cx);
                        }
                    })),
            )
            .border_1()
            .border_r_2()
            .rounded_none()
            .hover(|style| {
                if is_active {
                    style
                } else {
                    let hover_color = cx.theme().colors().ghost_element_hover;
                    style.bg(hover_color).border_color(hover_color)
                }
            })
            .when(
                is_active && self.focus_handle.contains_focused(cx),
                |this| this.border_color(Color::Selected.color(cx)),
            )
    }

    fn render_scrollbar(
        &self,
        items_count: usize,
        cx: &mut ViewContext<Self>,
    ) -> Option<Stateful<Div>> {
        let scroll_handle = self.scroll_handle.0.borrow();

        let height = scroll_handle
            .last_item_height
            .filter(|_| self.show_scrollbar || self.scrollbar_drag_thumb_offset.get().is_some())?;

        let total_list_length = height.0 as f64 * items_count as f64;
        let current_offset = scroll_handle.base_handle.offset().y.0.min(0.).abs() as f64;
        let mut percentage = current_offset / total_list_length;
        let end_offset = (current_offset + scroll_handle.base_handle.bounds().size.height.0 as f64)
            / total_list_length;
        // Uniform scroll handle might briefly report an offset greater than the length of a list;
        // in such case we'll adjust the starting offset as well to keep the scrollbar thumb length stable.
        let overshoot = (end_offset - 1.).clamp(0., 1.);
        if overshoot > 0. {
            percentage -= overshoot;
        }
        const MINIMUM_SCROLLBAR_PERCENTAGE_HEIGHT: f64 = 0.005;
        if percentage + MINIMUM_SCROLLBAR_PERCENTAGE_HEIGHT > 1.0 || end_offset > total_list_length
        {
            return None;
        }
        if total_list_length < scroll_handle.base_handle.bounds().size.height.0 as f64 {
            return None;
        }
        let end_offset = end_offset.clamp(percentage + MINIMUM_SCROLLBAR_PERCENTAGE_HEIGHT, 1.);
        Some(
            div()
                .absolute()
                .elevation_3(cx)
                .occlude()
                .id("breadcrumbs-popover-scroll")
                .on_mouse_move(cx.listener(|_, _, cx| {
                    cx.notify();
                    cx.stop_propagation()
                }))
                .on_hover(|_, cx| {
                    cx.stop_propagation();
                })
                .on_any_mouse_down(|_, cx| {
                    cx.stop_propagation();
                })
                .on_mouse_up(
                    MouseButton::Left,
                    cx.listener(|this, _, cx| {
                        if this.scrollbar_drag_thumb_offset.get().is_none()
                            && !this.focus_handle.contains_focused(cx)
                        {
                            this.hide_scrollbar(cx);
                            cx.notify();
                        }

                        cx.stop_propagation();
                    }),
                )
                .on_scroll_wheel(cx.listener(|_, _, cx| {
                    cx.notify();
                }))
                .h_full()
                .right_0()
                .top_0()
                .bottom_0()
                .w(px(12.))
                .cursor_default()
                .child(Scrollbar::new(
                    percentage as f32..end_offset as f32,
                    self.scroll_handle.clone(),
                    self.scrollbar_drag_thumb_offset.clone(),
                    cx.view().clone().into(),
                    items_count,
                )),
        )
    }
}

impl Render for Popover {
    fn render(&mut self, cx: &mut ViewContext<Self>) -> impl IntoElement {
        let items_count = self.visible_entries.len();

        v_flex()
            .elevation_3(cx)
            .id("breadcrumbs-popover")
            .group("breadcrumbs-popover")
            .key_context("breadcrumbs")
            .overflow_y_scroll()
            .w(px(400.0))
            .max_h(px(200.0))
            .on_hover(cx.listener(|this, hovered, cx| {
                if *hovered {
                    this.show_scrollbar = true;
                    this.hide_scrollbar_task.take();
                    cx.notify();
                } else if !this.focus_handle.contains_focused(cx) {
                    this.hide_scrollbar(cx);
                }
            }))
            .on_action(cx.listener(Self::select_next))
            .on_action(cx.listener(Self::select_prev))
            .on_action(cx.listener(Self::select_first))
            .on_action(cx.listener(Self::select_last))
            .on_action(cx.listener(Self::confirm))
            .on_action(cx.listener(Self::cancel))
            .track_focus(&self.focus_handle)
            .child(
                uniform_list(cx.view().clone(), "entries", items_count, {
                    |this, range, cx| {
                        let mut items = Vec::new();
                        this.for_each_visible_entry(range, cx, |id, details, cx| {
                            items.push(this.render_entry(id, details, cx));
                        });
                        items
                    }
                })
                // .elevation_3(cx)
                .size_full()
                .with_sizing_behavior(ListSizingBehavior::Infer)
                .track_scroll(self.scroll_handle.clone()),
            )
            .children(self.render_scrollbar(items_count, cx))
    }
}