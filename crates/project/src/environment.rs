use futures::{
    FutureExt,
    future::{Shared, WeakShared},
};
use std::{path::Path, sync::Arc};
use util::ResultExt;

use collections::HashMap;
use gpui::{App, AppContext as _, Context, Entity, EventEmitter, Task};
use settings::Settings as _;

use crate::{
    project_settings::{DirenvSettings, ProjectSettings},
    worktree_store::{WorktreeStore, WorktreeStoreEvent},
};

pub struct ProjectEnvironment {
    cli_environment: Option<HashMap<String, String>>,
    environments: HashMap<Arc<Path>, WeakShared<Task<Option<HashMap<String, String>>>>>,
    environment_error_messages: HashMap<Arc<Path>, EnvironmentErrorMessage>,
}

pub enum ProjectEnvironmentEvent {
    ErrorsUpdated,
}

impl EventEmitter<ProjectEnvironmentEvent> for ProjectEnvironment {}

impl ProjectEnvironment {
    pub fn new(
        worktree_store: &Entity<WorktreeStore>,
        cli_environment: Option<HashMap<String, String>>,
        cx: &mut App,
    ) -> Entity<Self> {
        cx.new(|cx| {
            cx.subscribe(worktree_store, |this: &mut Self, _, event, _| {
                if let WorktreeStoreEvent::WorktreeRemoved(_, _) = event {
                    this.environments.retain(|_, weak| weak.upgrade().is_some());
                    this.environment_error_messages
                        .retain(|abs_path, _| this.environments.contains_key(abs_path));
                }
            })
            .detach();

            Self {
                cli_environment,
                environments: Default::default(),
                environment_error_messages: Default::default(),
            }
        })
    }

    /// Returns the inherited CLI environment, if this project was opened from the Zed CLI.
    pub(crate) fn get_cli_environment(&self) -> Option<HashMap<String, String>> {
        if let Some(mut env) = self.cli_environment.clone() {
            set_origin_marker(&mut env, EnvironmentOrigin::Cli);
            Some(env)
        } else {
            None
        }
    }

    /// Returns an iterator over all pairs `(worktree_id, error_message)` of
    /// environment errors associated with this project environment.
    pub(crate) fn environment_errors(
        &self,
    ) -> impl Iterator<Item = (&Arc<Path>, &EnvironmentErrorMessage)> {
        self.environment_error_messages.iter()
    }

    pub(crate) fn remove_environment_error(&mut self, abs_path: &Path, cx: &mut Context<Self>) {
        self.environment_error_messages.remove(abs_path);
        cx.emit(ProjectEnvironmentEvent::ErrorsUpdated);
    }

    /// Returns the project environment, if possible.
    /// If the project was opened from the CLI, then the inherited CLI environment is returned.
    /// If it wasn't opened from the CLI, and an absolute path is given, then a shell is spawned in
    /// that directory, to get environment variables as if the user has `cd`'d there.
    pub(crate) fn get_environment(
        &mut self,
        abs_path: Option<Arc<Path>>,
        cx: &Context<Self>,
    ) -> Shared<Task<Option<HashMap<String, String>>>> {
        if cfg!(any(test, feature = "test-support")) {
            return Task::ready(Some(HashMap::default())).shared();
        }

        if let Some(cli_environment) = self.get_cli_environment() {
            return cx
                .spawn(async move |_, _| {
                    let path = cli_environment
                        .get("PATH")
                        .map(|path| path.as_str())
                        .unwrap_or_default();
                    log::info!(
                        "using project environment variables from CLI. PATH={:?}",
                        path
                    );
                    Some(cli_environment)
                })
                .shared();
        }

        let Some(abs_path) = abs_path else {
            return Task::ready(None).shared();
        };

        if let Some(existing) = self
            .environments
            .get(&abs_path)
            .and_then(|weak| weak.upgrade())
        {
            existing
        } else {
            let env = get_directory_env(abs_path.clone(), cx).shared();
            self.environments.insert(
                abs_path.clone(),
                env.downgrade()
                    .expect("environment task has not been polled yet"),
            );
            env
        }
    }
}

fn set_origin_marker(env: &mut HashMap<String, String>, origin: EnvironmentOrigin) {
    env.insert(ZED_ENVIRONMENT_ORIGIN_MARKER.to_string(), origin.into());
}

const ZED_ENVIRONMENT_ORIGIN_MARKER: &str = "ZED_ENVIRONMENT";

enum EnvironmentOrigin {
    Cli,
    WorktreeShell,
}

impl From<EnvironmentOrigin> for String {
    fn from(val: EnvironmentOrigin) -> Self {
        match val {
            EnvironmentOrigin::Cli => "cli".into(),
            EnvironmentOrigin::WorktreeShell => "worktree-shell".into(),
        }
    }
}

pub struct EnvironmentErrorMessage(pub String);

impl EnvironmentErrorMessage {
    #[allow(dead_code)]
    fn from_str(s: &str) -> Self {
        Self(String::from(s))
    }
}

async fn load_directory_shell_environment(
    abs_path: &Path,
    load_direnv: &DirenvSettings,
) -> (
    Option<HashMap<String, String>>,
    Option<EnvironmentErrorMessage>,
) {
    match smol::fs::metadata(abs_path).await {
        Ok(meta) => {
            let dir = if meta.is_dir() {
                abs_path
            } else if let Some(parent) = abs_path.parent() {
                parent
            } else {
                return (
                    None,
                    Some(EnvironmentErrorMessage(format!(
                        "Failed to load shell environment in {}: not a directory",
                        abs_path.display()
                    ))),
                );
            };

            load_shell_environment(&dir, load_direnv).await
        }
        Err(err) => (
            None,
            Some(EnvironmentErrorMessage(format!(
                "Failed to load shell environment in {}: {}",
                abs_path.display(),
                err
            ))),
        ),
    }
}

#[cfg(any(test, feature = "test-support"))]
async fn load_shell_environment(
    _dir: &Path,
    _load_direnv: &DirenvSettings,
) -> (
    Option<HashMap<String, String>>,
    Option<EnvironmentErrorMessage>,
) {
    let fake_env = [("ZED_FAKE_TEST_ENV".into(), "true".into())]
        .into_iter()
        .collect();
    (Some(fake_env), None)
}

#[cfg(all(target_os = "windows", not(any(test, feature = "test-support"))))]
async fn load_shell_environment(
    _dir: &Path,
    _load_direnv: &DirenvSettings,
) -> (
    Option<HashMap<String, String>>,
    Option<EnvironmentErrorMessage>,
) {
    // TODO the current code works with Unix $SHELL only, implement environment loading on windows
    (None, None)
}

#[cfg(not(any(target_os = "windows", test, feature = "test-support")))]
async fn load_shell_environment(
    dir: &Path,
    load_direnv: &DirenvSettings,
) -> (
    Option<HashMap<String, String>>,
    Option<EnvironmentErrorMessage>,
) {
    use crate::direnv::{DirenvError, load_direnv_environment};
    use std::path::PathBuf;
    use util::parse_env_output;

    fn message<T>(with: &str) -> (Option<T>, Option<EnvironmentErrorMessage>) {
        let message = EnvironmentErrorMessage::from_str(with);
        (None, Some(message))
    }

    const MARKER: &str = "ZED_SHELL_START";
    let Some(shell) = std::env::var("SHELL").log_err() else {
        return message("Failed to get login environment. SHELL environment variable is not set");
    };
    let shell_path = PathBuf::from(&shell);
    let shell_name = shell_path.file_name().and_then(|f| f.to_str());

    // What we're doing here is to spawn a shell and then `cd` into
    // the project directory to get the env in there as if the user
    // `cd`'d into it. We do that because tools like direnv, asdf, ...
    // hook into `cd` and only set up the env after that.
    //
    // If the user selects `Direct` for direnv, it would set an environment
    // variable that later uses to know that it should not run the hook.
    // We would include in `.envs` call so it is okay to run the hook
    // even if direnv direct mode is enabled.
    //
    // In certain shells we need to execute additional_command in order to
    // trigger the behavior of direnv, etc.
    //
    //
    // The `exit 0` is the result of hours of debugging, trying to find out
    // why running this command here, without `exit 0`, would mess
    // up signal process for our process so that `ctrl-c` doesn't work
    // anymore.
    //
    // We still don't know why `$SHELL -l -i -c '/usr/bin/env -0'`  would
    // do that, but it does, and `exit 0` helps.

    let command = match shell_name {
        Some("fish") => format!(
            "cd '{}'; emit fish_prompt; printf '%s' {MARKER}; /usr/bin/env; exit 0;",
            dir.display()
        ),
        _ => format!(
            "cd '{}'; printf '%s' {MARKER}; /usr/bin/env; exit 0;",
            dir.display()
        ),
    };

    // csh/tcsh only supports `-l` if it's the only flag. So this won't be a login shell.
    // Users must rely on vars from `~/.tcshrc` or `~/.cshrc` and not `.login` as a result.
    let args = match shell_name {
        Some("tcsh") | Some("csh") => vec!["-i", "-c", &command],
        _ => vec!["-l", "-i", "-c", &command],
    };

    let Some(output) = smol::process::Command::new(&shell)
        .args(&args)
        .output()
        .await
        .log_err()
    else {
        return message(
            "Failed to spawn login shell to source login environment variables. See logs for details",
        );
    };

    if !output.status.success() {
        log::error!("login shell exited with {}", output.status);
        return message("Login shell exited with nonzero exit code. See logs for details");
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let Some(env_output_start) = stdout.find(MARKER) else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        log::error!(
            "failed to parse output of `env` command in login shell. stdout: {:?}, stderr: {:?}",
            stdout,
            stderr
        );
        return message("Failed to parse stdout of env command. See logs for the output");
    };

    let mut parsed_env = HashMap::default();
    let env_output = &stdout[env_output_start + MARKER.len()..];

    parse_env_output(env_output, |key, value| {
        parsed_env.insert(key, value);
    });

    let (direnv_environment, direnv_error) = match load_direnv {
        DirenvSettings::ShellHook => (None, None),
        DirenvSettings::Direct => match load_direnv_environment(&parsed_env, dir).await {
            Ok(env) => (Some(env), None),
            Err(err) => (
                None,
                <Option<EnvironmentErrorMessage> as From<DirenvError>>::from(err),
            ),
        },
    };

    for (key, value) in direnv_environment.unwrap_or(HashMap::default()) {
        parsed_env.insert(key, value);
    }

    (Some(parsed_env), direnv_error)
}

fn get_directory_env(
    abs_path: Arc<Path>,
    cx: &Context<ProjectEnvironment>,
) -> Task<Option<HashMap<String, String>>> {
    let load_direnv = ProjectSettings::get_global(cx).load_direnv.clone();

    cx.spawn(async move |this, cx| {
        let (mut shell_env, error_message) = cx
            .background_spawn({
                let abs_path = abs_path.clone();
                async move { load_directory_shell_environment(&abs_path, &load_direnv).await }
            })
            .await;

        if let Some(shell_env) = shell_env.as_mut() {
            let path = shell_env
                .get("PATH")
                .map(|path| path.as_str())
                .unwrap_or_default();
            log::info!(
                "using project environment variables shell launched in {:?}. PATH={:?}",
                abs_path,
                path
            );

            set_origin_marker(shell_env, EnvironmentOrigin::WorktreeShell);
        }

        if let Some(error) = error_message {
            this.update(cx, |this, cx| {
                this.environment_error_messages.insert(abs_path, error);
                cx.emit(ProjectEnvironmentEvent::ErrorsUpdated)
            })
            .log_err();
        }

        shell_env
    })
}
