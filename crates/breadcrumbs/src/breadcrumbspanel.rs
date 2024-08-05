use crate::breadcrumbsscrollbar::BreadCrumbsScrollbar;

use file_icons::FileIcons;

use client::{ErrorCode, ErrorExt};
use collections::{hash_map, HashMap};
use gpui::{
    div, px, uniform_list, AnyElement, AppContext, Div, EventEmitter, FocusHandle, FocusableView,
    InteractiveElement, ListSizingBehavior, Model, MouseButton, ParentElement, Render, Stateful,
    Styled, UniformListScrollHandle, View, ViewContext, VisualContext as _, WeakView,
};
use menu::{Confirm, SelectFirst, SelectLast, SelectNext, SelectPrev};
use project::{Entry, EntryKind, Project, ProjectEntryId, ProjectPath, Worktree, WorktreeId};
use std::{
    cell::{Cell, OnceCell},
    collections::HashSet,
    ffi::OsStr,
    ops::Range,
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
};
use ui::{prelude::*, Icon, ListItem, Tooltip};
use workspace::{notifications::DetachAndPromptErr, SelectedEntry, Workspace};

pub struct BreadCrumbsPanel {
    project: Model<Project>,
    scroll_handle: UniformListScrollHandle,
    focus_handle: FocusHandle,
    visible_entries: Vec<(WorktreeId, Vec<Entry>, OnceCell<HashSet<Arc<Path>>>)>,
    expanded_dir_ids: HashMap<WorktreeId, Vec<ProjectEntryId>>,
    // Currently selected entry in a file tree
    current_entry: Option<(WorktreeId, ProjectEntryId)>,
    selection: Option<SelectedEntry>,
    workspace: WeakView<Workspace>,
    scrollbar_drag_thumb_offset: Rc<Cell<Option<f32>>>,
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
    worktree_id: WorktreeId,
    canonical_path: Option<Box<Path>>,
}

#[derive(Debug)]
pub enum Event {
    OpenedEntry {
        entry_id: ProjectEntryId,
        focus_opened_item: bool,
        allow_preview: bool,
    },
    SplitEntry {
        entry_id: ProjectEntryId,
    },
    Focus,
}

impl BreadCrumbsPanel {
    fn new(
        workspace: &mut Workspace,
        current_entry: Option<(WorktreeId, ProjectEntryId)>,
        cx: &mut ViewContext<Workspace>,
    ) -> View<Self> {
        let project = workspace.project().clone();
        let breadcrumbs_panel = cx.new_view(|cx: &mut ViewContext<Self>| {
            let focus_handle = cx.focus_handle();
            cx.on_focus(&focus_handle, Self::focus_in).detach();
            cx.on_focus_out(&focus_handle, |this, _, cx| {
                // TODO should dismiss
            })
            .detach();

            cx.observe_global::<FileIcons>(|_, cx| {
                cx.notify();
            })
            .detach();

            let mut this = Self {
                project: project.clone(),
                scroll_handle: UniformListScrollHandle::new(),
                focus_handle,
                visible_entries: Default::default(),
                expanded_dir_ids: Default::default(),
                current_entry,
                selection: None,
                workspace: workspace.weak_handle(),
                scrollbar_drag_thumb_offset: Default::default(),
            };
            this.update_visible_entries(None, cx);

            this
        });

        cx.subscribe(&breadcrumbs_panel, {
            move |workspace, _, event, cx| match event {
                &Event::OpenedEntry {
                    entry_id,
                    focus_opened_item,
                    ..
                } => {
                    if let Some(worktree) = project.read(cx).worktree_for_entry(entry_id, cx) {
                        if let Some(entry) = worktree.read(cx).entry_for_id(entry_id) {
                            let worktree_id = worktree.read(cx).id();
                            let file_path = entry.path.clone();

                            workspace
                                .open_path_preview(
                                    ProjectPath {
                                        worktree_id,
                                        path: file_path.clone(),
                                    },
                                    None,
                                    focus_opened_item,
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
                        }
                    }
                }
                &Event::SplitEntry { entry_id } => {
                    if let Some(worktree) = project.read(cx).worktree_for_entry(entry_id, cx) {
                        if let Some(entry) = worktree.read(cx).entry_for_id(entry_id) {
                            workspace
                                .split_path(
                                    ProjectPath {
                                        worktree_id: worktree.read(cx).id(),
                                        path: entry.path.clone(),
                                    },
                                    cx,
                                )
                                .detach_and_log_err(cx);
                        }
                    }
                }
                _ => {}
            }
        })
        .detach();

        breadcrumbs_panel
    }

    fn focus_in(&mut self, cx: &mut ViewContext<Self>) {
        if !self.focus_handle.contains_focused(cx) {
            cx.emit(Event::Focus);
        }
    }

    fn toggle_expanded(&mut self, entry_id: ProjectEntryId, cx: &mut ViewContext<Self>) {
        if let Some(worktree_id) = self.project.read(cx).worktree_id_for_entry(entry_id, cx) {
            if let Some(expanded_dir_ids) = self.expanded_dir_ids.get_mut(&worktree_id) {
                match expanded_dir_ids.binary_search(&entry_id) {
                    Ok(ix) => {
                        expanded_dir_ids.remove(ix);
                    }
                    Err(ix) => {
                        expanded_dir_ids.insert(ix, entry_id);
                    }
                }
                self.update_visible_entries(Some((worktree_id, entry_id)), cx);
                cx.focus(&self.focus_handle);
                cx.notify();
            }
        }
    }

    fn select_prev(&mut self, _: &SelectPrev, cx: &mut ViewContext<Self>) {
        if let Some(selection) = self.selection {
            let (mut worktree_ix, mut entry_ix, _) =
                self.index_for_selection(selection).unwrap_or_default();
            if entry_ix > 0 {
                entry_ix -= 1;
            } else if worktree_ix > 0 {
                worktree_ix -= 1;
                entry_ix = self.visible_entries[worktree_ix].1.len() - 1;
            } else {
                return;
            }

            let (worktree_id, worktree_entries, _) = &self.visible_entries[worktree_ix];
            let selection = SelectedEntry {
                worktree_id: *worktree_id,
                entry_id: worktree_entries[entry_ix].id,
            };
            self.selection = Some(selection);
            self.autoscroll(cx);
            cx.notify();
        } else {
            self.select_first(&SelectFirst {}, cx);
        }
    }

    fn cancel(&mut self, _: &menu::Cancel, cx: &mut ViewContext<Self>) {
        self.update_visible_entries(None, cx);
        cx.focus(&self.focus_handle);
        cx.notify();
    }

    fn open_entry(
        &mut self,
        entry_id: ProjectEntryId,
        focus_opened_item: bool,
        allow_preview: bool,
        cx: &mut ViewContext<Self>,
    ) {
        cx.emit(Event::OpenedEntry {
            entry_id,
            focus_opened_item,
            allow_preview,
        });
    }

    fn split_entry(&mut self, entry_id: ProjectEntryId, cx: &mut ViewContext<Self>) {
        cx.emit(Event::SplitEntry { entry_id });
    }

    fn select_next(&mut self, _: &SelectNext, cx: &mut ViewContext<Self>) {
        if let Some(selection) = self.selection {
            let (mut worktree_ix, mut entry_ix, _) =
                self.index_for_selection(selection).unwrap_or_default();
            if let Some((_, worktree_entries, _)) = self.visible_entries.get(worktree_ix) {
                if entry_ix + 1 < worktree_entries.len() {
                    entry_ix += 1;
                } else {
                    worktree_ix += 1;
                    entry_ix = 0;
                }
            }

            if let Some((worktree_id, worktree_entries, _)) = self.visible_entries.get(worktree_ix)
            {
                if let Some(entry) = worktree_entries.get(entry_ix) {
                    let selection = SelectedEntry {
                        worktree_id: *worktree_id,
                        entry_id: entry.id,
                    };
                    self.selection = Some(selection);

                    self.autoscroll(cx);
                    cx.notify();
                }
            }
        } else {
            self.select_first(&SelectFirst {}, cx);
        }
    }

    fn select_first(&mut self, _: &SelectFirst, cx: &mut ViewContext<Self>) {
        let worktree = self
            .visible_entries
            .first()
            .and_then(|(worktree_id, _, _)| {
                self.project.read(cx).worktree_for_id(*worktree_id, cx)
            });
        if let Some(worktree) = worktree {
            let worktree = worktree.read(cx);
            let worktree_id = worktree.id();
            if let Some(root_entry) = worktree.root_entry() {
                let selection = SelectedEntry {
                    worktree_id,
                    entry_id: root_entry.id,
                };
                self.selection = Some(selection);
                self.autoscroll(cx);
                cx.notify();
            }
        }
    }

    fn select_last(&mut self, _: &SelectLast, cx: &mut ViewContext<Self>) {
        let worktree = self.visible_entries.last().and_then(|(worktree_id, _, _)| {
            self.project.read(cx).worktree_for_id(*worktree_id, cx)
        });
        if let Some(worktree) = worktree {
            let worktree = worktree.read(cx);
            let worktree_id = worktree.id();
            if let Some(last_entry) = worktree.entries(true, 0).last() {
                self.selection = Some(SelectedEntry {
                    worktree_id,
                    entry_id: last_entry.id,
                });
                self.autoscroll(cx);
                cx.notify();
            }
        }
    }

    fn autoscroll(&mut self, cx: &mut ViewContext<Self>) {
        if let Some((_, _, index)) = self.selection.and_then(|s| self.index_for_selection(s)) {
            self.scroll_handle.scroll_to_item(index);
            cx.notify();
        }
    }

    fn index_for_selection(&self, selection: SelectedEntry) -> Option<(usize, usize, usize)> {
        let mut entry_index = 0;
        let mut visible_entries_index = 0;
        for (worktree_index, (worktree_id, worktree_entries, _)) in
            self.visible_entries.iter().enumerate()
        {
            if *worktree_id == selection.worktree_id {
                for entry in worktree_entries {
                    if entry.id == selection.entry_id {
                        return Some((worktree_index, entry_index, visible_entries_index));
                    } else {
                        visible_entries_index += 1;
                        entry_index += 1;
                    }
                }
                break;
            } else {
                visible_entries_index += worktree_entries.len();
            }
        }
        None
    }

    pub fn selected_entry<'a>(
        &self,
        cx: &'a AppContext,
    ) -> Option<(&'a Worktree, &'a project::Entry)> {
        let (worktree, entry) = self.selected_entry_handle(cx)?;
        Some((worktree.read(cx), entry))
    }

    fn selected_entry_handle<'a>(
        &self,
        cx: &'a AppContext,
    ) -> Option<(Model<Worktree>, &'a project::Entry)> {
        let selection = self.selection?;
        let project = self.project.read(cx);
        let worktree = project.worktree_for_id(selection.worktree_id, cx)?;
        let entry = worktree.read(cx).entry_for_id(selection.entry_id)?;
        Some((worktree, entry))
    }

    fn expand_to_selection(&mut self, cx: &mut ViewContext<Self>) -> Option<()> {
        let (worktree, entry) = self.selected_entry(cx)?;
        let expanded_dir_ids = self.expanded_dir_ids.entry(worktree.id()).or_default();

        for path in entry.path.ancestors() {
            let Some(entry) = worktree.entry_for_path(path) else {
                continue;
            };
            if entry.is_dir() {
                if let Err(idx) = expanded_dir_ids.binary_search(&entry.id) {
                    expanded_dir_ids.insert(idx, entry.id);
                }
            }
        }

        Some(())
    }

    fn update_visible_entries(
        &mut self,
        new_selected_entry: Option<(WorktreeId, ProjectEntryId)>,
        cx: &mut ViewContext<Self>,
    ) {
        let project = self.project.read(cx);

        self.visible_entries.clear();
        let (worktree, entry_id) = if let Some((worktree_id, entry_id)) = self.current_entry {
            (project.worktree_for_id(worktree_id, cx).unwrap(), entry_id)
        } else {
            return;
        };
        let snapshot = worktree.read(cx).snapshot();
        let worktree_id = snapshot.id();

        let expanded_dir_ids = match self.expanded_dir_ids.entry(worktree_id) {
            hash_map::Entry::Occupied(e) => e.into_mut(),
            hash_map::Entry::Vacant(e) => {
                // The first time a worktree's root entry becomes available,
                // mark that root entry as expanded.
                if let Some(entry) = snapshot.root_entry() {
                    e.insert(vec![entry.id]).as_slice()
                } else {
                    &[]
                }
            }
        };

        let mut visible_worktree_entries = Vec::new();
        let entry = worktree.read(cx).entry_for_id(entry_id).unwrap();
        let path = entry.path.parent().unwrap_or(worktree.abs_path());
        let mut entries_iter = snapshot.child_entries(path);
        while let Some(entry) = entry_iter.entry() {
            visible_worktree_entries.push(entry.clone());
            if expanded_dir_ids.binary_search(&entry.id).is_err() && entry_iter.advance_to_sibling()
            {
                continue;
            }
            entry_iter.advance();
        }

        project::sort_worktree_entries(&mut visible_worktree_entries);
        self.visible_entries
            .push((worktree_id, visible_worktree_entries, OnceCell::new()));
        if let Some((worktree_id, entry_id)) = new_selected_entry {
            self.selection = Some(SelectedEntry {
                worktree_id,
                entry_id,
            });
        }
    }

    fn confirm(&mut self, _: &Confirm, cx: &mut ViewContext<Self>) {
        if let Some((_, ref entry)) = self.selected_entry_handle(cx) {
            if entry.is_dir() {
                self.toggle_expanded(entry.id, cx);
            } else {
                self.open_entry(entry.id, true, false, cx);
            }
        }
    }

    fn expand_entry(
        &mut self,
        worktree_id: WorktreeId,
        entry_id: ProjectEntryId,
        cx: &mut ViewContext<Self>,
    ) {
        self.project.update(cx, |project, cx| {
            if let Some((worktree, expanded_dir_ids)) = project
                .worktree_for_id(worktree_id, cx)
                .zip(self.expanded_dir_ids.get_mut(&worktree_id))
            {
                let worktree = worktree.read(cx);

                if let Some(mut entry) = worktree.entry_for_id(entry_id) {
                    loop {
                        if let Err(ix) = expanded_dir_ids.binary_search(&entry.id) {
                            expanded_dir_ids.insert(ix, entry.id);
                        }

                        if let Some(parent_entry) =
                            entry.path.parent().and_then(|p| worktree.entry_for_path(p))
                        {
                            entry = parent_entry;
                        } else {
                            break;
                        }
                    }
                }
            }
        });
    }

    fn for_each_visible_entry(
        &self,
        range: Range<usize>,
        cx: &mut ViewContext<BreadCrumbsPanel>,
        mut callback: impl FnMut(ProjectEntryId, EntryDetails, &mut ViewContext<BreadCrumbsPanel>),
    ) {
        let mut ix = 0;
        for (worktree_id, visible_worktree_entries, entries_paths) in &self.visible_entries {
            if ix >= range.end {
                return;
            }

            if ix + visible_worktree_entries.len() <= range.start {
                ix += visible_worktree_entries.len();
                continue;
            }

            let end_ix = range.end.min(ix + visible_worktree_entries.len());
            let show_file_icons = true;
            if let Some(worktree) = self.project.read(cx).worktree_for_id(*worktree_id, cx) {
                let snapshot = worktree.read(cx).snapshot();
                let root_name = OsStr::new(snapshot.root_name());
                let expanded_entry_ids = self
                    .expanded_dir_ids
                    .get(&snapshot.id())
                    .map(Vec::as_slice)
                    .unwrap_or(&[]);

                let entry_range = range.start.saturating_sub(ix)..end_ix - ix;
                let entries = entries_paths.get_or_init(|| {
                    visible_worktree_entries
                        .iter()
                        .map(|e| (e.path.clone()))
                        .collect()
                });
                for entry in visible_worktree_entries[entry_range].iter() {
                    let is_expanded = expanded_entry_ids.binary_search(&entry.id).is_ok();
                    let icon = match entry.kind {
                        EntryKind::File(_) => {
                            if show_file_icons {
                                FileIcons::get_icon(&entry.path, cx)
                            } else {
                                None
                            }
                        }
                        _ => FileIcons::get_chevron_icon(is_expanded, cx),
                    };

                    let (depth, difference) =
                        BreadCrumbsPanel::calculate_depth_and_difference(entry, &entries);

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
                    let selection = SelectedEntry {
                        worktree_id: snapshot.id(),
                        entry_id: entry.id,
                    };
                    let details = EntryDetails {
                        filename,
                        icon,
                        path: entry.path.clone(),
                        depth,
                        kind: entry.kind,
                        is_expanded,
                        is_selected: self.selection == Some(selection),
                        worktree_id: *worktree_id,
                        canonical_path: entry.canonical_path.clone(),
                    };

                    callback(entry.id, details, cx);
                }
            }
            ix = end_ix;
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
            .map_or(false, |selection| selection.entry_id == entry_id);
        let icon = details.icon.clone();

        let canonical_path = details
            .canonical_path
            .as_ref()
            .map(|f| f.to_string_lossy().to_string());

        let depth = details.depth;

        div()
            .id(entry_id.to_proto() as usize)
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
                            this.open_entry(entry_id, true, false, cx);
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
            .filter(|_| self.scrollbar_drag_thumb_offset.get().is_some())?;

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
                .occlude()
                .id("breadcrumbs-panel-scroll")
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
                .on_scroll_wheel(cx.listener(|_, _, cx| {
                    cx.notify();
                }))
                .h_full()
                .absolute()
                .right_0()
                .top_0()
                .bottom_0()
                .w(px(12.))
                .cursor_default()
                .child(BreadCrumbsScrollbar::new(
                    percentage as f32..end_offset as f32,
                    self.scroll_handle.clone(),
                    self.scrollbar_drag_thumb_offset.clone(),
                    cx.view().clone().into(),
                    items_count,
                )),
        )
    }
}

impl Render for BreadCrumbsPanel {
    fn render(&mut self, cx: &mut gpui::ViewContext<Self>) -> impl IntoElement {
        let items_count = self
            .visible_entries
            .iter()
            .map(|(_, worktree_entries, _)| worktree_entries.len())
            .sum();

        h_flex()
            .id("breadcrumbs-panel")
            .group("breadcrumbs-panel")
            .elevation_2(cx)
            .overflow_y_scroll()
            .max_w(px(100.0))
            .max_h(px(100.0))
            .on_action(cx.listener(Self::select_next))
            .on_action(cx.listener(Self::select_prev))
            .on_action(cx.listener(Self::select_first))
            .on_action(cx.listener(Self::select_last))
            .on_action(cx.listener(Self::confirm))
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
                .size_full()
                .with_sizing_behavior(ListSizingBehavior::Infer)
                .track_scroll(self.scroll_handle.clone()),
            )
            .children(self.render_scrollbar(items_count, cx))
    }
}

impl EventEmitter<Event> for BreadCrumbsPanel {}

impl FocusableView for BreadCrumbsPanel {
    fn focus_handle(&self, _cx: &AppContext) -> FocusHandle {
        self.focus_handle.clone()
    }
}
