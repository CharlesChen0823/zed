use gpui::{Model, ModelContext};
use itertools::Itertools;
use language::Buffer;
use settings::WorktreeId;
use std::{collections::BTreeMap, path::Path, sync::Arc};
use text::{Anchor, BufferId};

use crate::worktree_store::{WorktreeStore, WorktreeStoreEvent};
use crate::ProjectItem;

#[derive(Debug, Clone)]
pub struct Bookmark {
    pub id: usize,
    pub buffer: Model<Buffer>,
    pub anchor: Anchor,
    pub annotation: Option<String>,
}

#[derive(Debug)]
pub struct BookmarkStore {
    bookmark_ordered: Vec<usize>,
    current_id: usize,
    bookmark_map: BTreeMap<usize, Bookmark>,
    next_bookmark_id: usize,
}

impl BookmarkStore {
    pub fn new(worktree_store: Model<WorktreeStore>, cx: &mut ModelContext<Self>) -> Self {
        cx.subscribe(&worktree_store, Self::on_worktree_store_event)
            .detach();
        Self {
            bookmark_ordered: vec![],
            current_id: 0,
            bookmark_map: BTreeMap::default(),
            next_bookmark_id: 0,
        }
    }

    pub fn update_current_id(&mut self, current_id: usize, _cx: &mut ModelContext<Self>) {
        if self.bookmark_ordered.contains(&current_id) {
            self.current_id = current_id;
        }
    }

    fn on_worktree_store_event(
        &mut self,
        _: Model<WorktreeStore>,
        event: &WorktreeStoreEvent,
        cx: &mut ModelContext<Self>,
    ) {
        match event {
            WorktreeStoreEvent::WorktreeAdded(worktree) => cx
                .subscribe(worktree, |this, worktree, event, cx| match event {
                    worktree::Event::DeletedEntry(id) => {
                        // TODO, need better method
                        let worktree_id = worktree.read(cx).id();
                        if let Some(entry) = worktree.read(cx).entry_for_id(id.clone()) {
                            let path = entry.path.clone();
                            this.clear_by_project_entry_id(worktree_id, path, cx);
                        }
                    }
                    _ => {}
                })
                .detach(),
            WorktreeStoreEvent::WorktreeRemoved(_, id) => self.clear_by_worktree_id(*id, cx),
            _ => {}
        }
    }

    pub fn toggle_bookmark(
        &mut self,
        buffer: Model<Buffer>,
        anchor: Anchor,
        annotation: Option<String>,
    ) {
        let id = self
            .bookmark_map
            .iter()
            .find(|(_, bm)| bm.buffer == buffer && bm.anchor == anchor)
            .map(|(id, _)| *id);
        if let Some(id) = id {
            self.bookmark_map.remove(&id);
            self.bookmark_ordered.retain(|i| i != &id);
            if self.current_id == id {
                self.current_id = self.find_nearest_id(self.current_id);
            }
        } else {
            self.next_bookmark_id += 1;
            let bookmark_id = self.next_bookmark_id;
            let bookmark = Bookmark {
                id: bookmark_id,
                buffer,
                anchor,
                annotation,
            };

            self.bookmark_map.insert(bookmark_id, bookmark);
            self.bookmark_ordered.push(bookmark_id);
            self.current_id = bookmark_id;
        }
    }

    fn find_nearest_id(&mut self, target_id: usize) -> usize {
        // first find previous id
        if let Some(id) = self.find_previous_id(target_id) {
            return id;
        }
        // find next id
        if let Some(id) = self.find_next_id(target_id) {
            return id;
        }
        // empty
        0
    }

    fn find_previous_id(&mut self, target_id: usize) -> Option<usize> {
        let mut previous = None;
        for id in self.bookmark_ordered.iter() {
            if id >= &target_id {
                break;
            }
            previous = Some(*id);
        }
        previous
    }

    fn find_next_id(&mut self, target_id: usize) -> Option<usize> {
        let mut next = None;
        for id in self.bookmark_ordered.iter() {
            if id > &target_id {
                next = Some(*id);
                break;
            }
        }
        next
    }

    pub fn prev_bookmark(&mut self) -> Option<Bookmark> {
        if let Some(id) = self.find_previous_id(self.current_id) {
            self.current_id = id;
            return self.bookmark_map.get(&id).cloned();
        }
        None
    }

    pub fn next_bookmark(&mut self) -> Option<Bookmark> {
        if let Some(id) = self.find_next_id(self.current_id) {
            self.current_id = id;
            return self.bookmark_map.get(&id).cloned();
        }
        None
    }

    fn clear_by_project_entry_id(
        &mut self,
        worktree_id: WorktreeId,
        path: Arc<Path>,
        cx: &mut ModelContext<Self>,
    ) {
        let ids_will_remove = self
            .bookmark_map
            .iter()
            .filter_map(|(id, bm)| {
                let file = bm.buffer.read(cx).file()?;
                if file.worktree_id(cx) == worktree_id && file.path() == &path {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect_vec();
        self.bookmark_ordered
            .retain(|id| !ids_will_remove.contains(id));
        self.bookmark_map
            .retain(|id, _| !ids_will_remove.contains(id));
        if ids_will_remove.contains(&self.current_id) {
            self.current_id = self.find_nearest_id(self.current_id);
        }
    }

    pub fn clear_by_buffer_id(&mut self, buffer_id: BufferId, cx: &mut ModelContext<Self>) {
        let ids_will_remove = self
            .bookmark_map
            .iter()
            .filter_map(|(id, bm)| {
                if bm.buffer.read(cx).remote_id() == buffer_id {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect_vec();

        self.bookmark_ordered
            .retain(|id| !ids_will_remove.contains(id));
        self.bookmark_map
            .retain(|id, _| !ids_will_remove.contains(id));
        if ids_will_remove.contains(&self.current_id) {
            self.current_id = self.find_nearest_id(self.current_id);
        }
    }

    pub fn clear_by_worktree_id(&mut self, worktree_id: WorktreeId, cx: &mut ModelContext<Self>) {
        let ids_will_remove = self
            .bookmark_map
            .iter()
            .filter_map(|(id, bm)| {
                if let Some(project_path) = bm.buffer.read(cx).project_path(cx) {
                    if project_path.worktree_id == worktree_id {
                        Some(*id)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect_vec();

        self.bookmark_ordered
            .retain(|id| !ids_will_remove.contains(id));
        self.bookmark_map
            .retain(|id, _| !ids_will_remove.contains(id));
        if ids_will_remove.contains(&self.current_id) {
            self.current_id = self.find_nearest_id(self.current_id);
        }
    }

    pub fn clear_all(&mut self) {
        self.current_id = 0;
        self.next_bookmark_id = 0;
        self.bookmark_ordered.clear();
        self.bookmark_map.clear();
    }

    pub fn get_bookmark_by_buffer_id(
        &self,
        buffer_id: BufferId,
        cx: &mut ModelContext<Self>,
    ) -> Vec<Bookmark> {
        self.bookmark_map
            .values()
            .filter_map(|bm| {
                if bm.buffer.read(cx).remote_id() == buffer_id {
                    Some(bm.clone())
                } else {
                    None
                }
            })
            .collect_vec()
    }

    pub fn get_current_worktree(
        &self,
        worktree_id: WorktreeId,
        cx: &mut ModelContext<Self>,
    ) -> Vec<Bookmark> {
        self.bookmark_map
            .values()
            .filter_map(|bm| {
                if bm.buffer.read(cx).project_path(cx).unwrap().worktree_id == worktree_id {
                    Some(bm.clone())
                } else {
                    None
                }
            })
            .collect_vec()
    }

    pub fn get_bookmark_all(&self) -> Vec<Bookmark> {
        self.bookmark_map
            .values()
            .map(|bm| bm.clone())
            .collect_vec()
    }
}
