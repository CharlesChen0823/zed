use std::{path::PathBuf, sync::Arc};

use anyhow::Result;
use fs::Fs;
use futures::StreamExt;
use gpui::{App, AppContext, AsyncApp, Context, Entity, Task, WeakEntity};
use rpc::{
    AnyProtoClient, TypedEnvelope,
    proto::{self, FromProto, ToProto},
};
use url::Url;

use crate::buffer_store::BufferStore;
use paths::local_history_dir;
use serde::{Deserialize, Serialize};
use util::ResultExt;

enum LocalHistoryState {
    Local {
        fs: Arc<dyn Fs>,
    },
    Remote {
        upstream_client: AnyProtoClient,
        upstream_project_id: u64,
    },
}

pub struct Inventory {
    fs: Arc<dyn Fs>,
    history_entries: Vec<HistoryEntry>,
    initialed: bool,
}

impl Inventory {
    pub fn new(fs: Arc<dyn Fs>, cx: &mut App) -> Entity<Self> {
        cx.new(|_| Self {
            fs,
            history_entries: Vec::new(),
            initialed: false,
        })
    }

    pub fn list_entries(&self, cx: &mut App) {
        // if self.initialed {
        //     return Task::ready(self.history_entries.clone());
        // }
        //
        let fs = self.fs.clone();
        let mut history_entries = Vec::new();

        cx.background_spawn(async move {
            if let Ok(mut entries) = fs.read_dir(&local_history_dir()).await {
                while let Some(Ok(file_path)) = entries.next().await {
                    if file_path.is_dir() {
                        let file = file_path.join("entry.json");
                        if let Ok(content) = fs.load(&file).await {
                            if let Some(entry) =
                                serde_json::from_str::<HistoryEntry>(&content).log_err()
                            {
                                history_entries.push(entry);
                            }
                        }
                    }
                }
            }
        })
        .detach();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Entry {
    id: String,
    timestamp: u64,
    source: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct HistoryEntry {
    version: u64,
    url: Url,
    entries: Vec<Entry>,
}

pub struct LocalHistoryStore {
    state: LocalHistoryState,
    downstream_client: Option<(AnyProtoClient, u64)>,
    history_entity: Vec<HistoryEntry>,
    buffer_store: WeakEntity<BufferStore>,
    // Implementation details
}

impl LocalHistoryStore {
    pub fn init(client: &AnyProtoClient) {
        client.add_entity_request_handler(Self::handle_copy_local_history);
        client.add_entity_request_handler(Self::handle_delete_local_history);
        client.add_entity_request_handler(Self::handle_delete_all_local_history);
    }

    pub fn local(
        fs: Arc<dyn Fs>,
        buffer_store: WeakEntity<BufferStore>,
        cx: &mut Context<Self>,
    ) -> Self {
        Self {
            downstream_client: None,
            state: LocalHistoryState::Local { fs },
            history_entity: Vec::new(),
            buffer_store,
        }
    }

    pub fn remote(
        fs: Arc<dyn Fs>,
        buffer_store: WeakEntity<BufferStore>,
        upstream_client: AnyProtoClient,
        project_id: u64,
        cx: &mut Context<Self>,
    ) -> Self {
        Self {
            downstream_client: None,
            state: LocalHistoryState::Remote {
                upstream_client,
                upstream_project_id: project_id,
            },
            history_entity: Vec::new(),
            buffer_store,
        }
    }

    // todo!()
    // 1. should subscribe the file delete
    // 2. should subscribe the buffer save
    // 3. should subscribe the file copy
    // 4. should subscribe the file create
    // 5. should subscribe the file rename
    // 6. should subscribe the app quit, and serialize local_history to local

    pub fn list_entries(&self) -> Vec<HistoryEntry> {
        self.history_entity.clone()
    }

    pub fn create_entry(&mut self, entry: HistoryEntry) {
        self.history_entity.push(entry);
        // todo!() should create entry.json and copy to local_history
    }

    pub fn shared(
        &mut self,
        remote_id: u64,
        downstream_client: AnyProtoClient,
        cx: &mut Context<Self>,
    ) {
        self.downstream_client = Some((downstream_client, remote_id));
        // when shared, should update local_history? might not need
        // self.send_project_updates(cx);
    }

    pub fn unshared(&mut self, _: &mut Context<Self>) {
        self.downstream_client.take();
    }

    pub async fn handle_delete_all_local_history(
        this: Entity<Self>,
        envelope: TypedEnvelope<proto::DeleteAllLocalHistoryRequest>,
        mut cx: AsyncApp,
    ) -> Result<proto::LocalHistoryResponse> {
    }
    pub async fn handle_delete_local_history(
        this: Entity<Self>,
        envelope: TypedEnvelope<proto::DeleteLocalHistoryRequest>,
        mut cx: AsyncApp,
    ) -> Result<proto::LocalHistoryResponse> {
    }
    pub async fn handle_copy_local_history(
        this: Entity<Self>,
        envelope: TypedEnvelope<proto::DeleteLocalHistoryRequest>,
        mut cx: AsyncApp,
    ) -> Result<proto::LocalHistoryResponse> {
    }
}
