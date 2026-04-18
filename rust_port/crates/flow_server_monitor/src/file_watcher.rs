/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::os::fd::RawFd;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::time::Duration;

use flow_common_vcs::git;
use flow_common_vcs::hg;
use flow_common_vcs::vcs::Vcs;
use flow_common_vcs::vcs::{self};
use flow_common_vcs::vcs_utils;
use flow_server_env::monitor_prot;
use flow_server_env::monitor_prot::FileWatcherMetadata;

use crate::flow_server_monitor_options;
use crate::runtime;
use crate::status_stream;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExitReason {
    WatcherStopped,
    WatcherDied,
    WatcherMissedChanges,
}

pub trait Watcher: Send {
    fn name(&self) -> &str;
    fn debug(&self) -> bool;
    fn start_init(&mut self);
    fn wait_for_init(
        &mut self,
        timeout: Option<f64>,
    ) -> impl std::future::Future<Output = Result<(), String>> + Send;
    fn get_and_clear_changed_files(
        &mut self,
    ) -> impl std::future::Future<Output = (BTreeSet<String>, Option<FileWatcherMetadata>, bool)> + Send;
    fn wait_for_changed_files(&mut self) -> impl std::future::Future<Output = ()> + Send;
    fn stop(&mut self) -> impl std::future::Future<Output = ()> + Send;
    fn waitpid(&self) -> impl std::future::Future<Output = ExitReason> + Send;
    fn getpid(&self) -> Option<i32>;
}

pub struct Dummy;

impl Dummy {
    pub fn new() -> Self {
        Dummy
    }
}

impl Watcher for Dummy {
    fn name(&self) -> &str {
        "dummy"
    }
    fn debug(&self) -> bool {
        false
    }
    fn start_init(&mut self) {}
    async fn wait_for_init(&mut self, _timeout: Option<f64>) -> Result<(), String> {
        Ok(())
    }
    async fn get_and_clear_changed_files(
        &mut self,
    ) -> (BTreeSet<String>, Option<FileWatcherMetadata>, bool) {
        (BTreeSet::new(), None, false)
    }
    async fn wait_for_changed_files(&mut self) {}
    async fn stop(&mut self) {}
    async fn waitpid(&self) -> ExitReason {
        std::future::pending().await
    }
    fn getpid(&self) -> Option<i32> {
        None
    }
}
pub fn changes_since_mergebase(mergebase_with: &str, watch_paths: &[PathBuf]) -> BTreeSet<String> {
    fn fold_relative_paths(
        mut acc: BTreeSet<String>,
        root: &std::path::Path,
        paths: &[String],
    ) -> BTreeSet<String> {
        for change in paths {
            let path = root.join(change).to_string_lossy().to_string();
            acc.insert(path);
        }
        acc
    }

    async fn files_changed_since_mergebase_with(
        vcs: Vcs,
        root: &std::path::Path,
        mergebase_with: &str,
    ) -> Result<(String, Vec<String>), vcs_utils::ErrorStatus> {
        let root_str = root.to_string_lossy().to_string();
        match vcs {
            Vcs::Git => {
                git::files_changed_since_mergebase_with(Some(&root_str), mergebase_with).await
            }
            Vcs::Hg => {
                hg::files_changed_since_mergebase_with(Some(&root_str), mergebase_with).await
            }
        }
    }

    async fn fold_files_changed_since_mergebase_with(
        acc: BTreeSet<String>,
        vcs: Vcs,
        root: &std::path::Path,
        mergebase_with: &str,
    ) -> BTreeSet<String> {
        let vcs_name = vcs::name(vcs);
        match files_changed_since_mergebase_with(vcs, root, mergebase_with).await {
            Err(_) => {
                log::error!(
                    "Not checking changes since mergebase! {} failed to determine the initial mergebase.",
                    vcs_name
                );
                acc
            }
            Ok((mergebase, changes)) => {
                log::info!(
                    "{} reports the initial mergebase as {:?}, and {} changes",
                    vcs_name,
                    mergebase,
                    changes.len()
                );
                fold_relative_paths(acc, root, &changes)
            }
        }
    }

    async fn helper(
        mergebase_with: &str,
        mut seen_roots: BTreeSet<String>,
        mut acc: BTreeSet<String>,
        paths: &[PathBuf],
    ) -> BTreeSet<String> {
        for path in paths {
            match vcs::find_root(None, path) {
                Some((vcs, root)) => {
                    let root_str = root.to_string_lossy().to_string();
                    if seen_roots.contains(&root_str) {
                    } else {
                        seen_roots.insert(root_str);
                        acc = fold_files_changed_since_mergebase_with(
                            acc,
                            vcs,
                            &root,
                            mergebase_with,
                        )
                        .await;
                    }
                }
                None => {}
            }
        }
        acc
    }

    runtime::handle().block_on(async move {
        helper(
            mergebase_with,
            BTreeSet::new(),
            BTreeSet::new(),
            watch_paths,
        )
        .await
    })
}

pub fn query_mergebase(root: &std::path::Path, mergebase_with: &str) -> Result<String, String> {
    runtime::handle().block_on(async move {
        match vcs::find_root(None, root) {
            None => Err("no VCS root found".to_string()),
            Some((vcs, vcs_root)) => {
                let cwd = vcs_root.to_string_lossy().to_string();
                let result = match vcs {
                    Vcs::Hg => hg::merge_base(Some(&cwd), ".", mergebase_with).await,
                    Vcs::Git => git::merge_base(Some(&cwd), mergebase_with, "HEAD").await,
                };
                match result {
                    Ok(hash) => Ok(hash),
                    Err(vcs_utils::ErrorStatus::NotInstalled { path }) => {
                        Err(format!("VCS not installed at {}", path))
                    }
                    Err(vcs_utils::ErrorStatus::Errored(msg)) => Err(msg),
                }
            }
        }
    })
}

pub mod watchman_file_watcher {
    use super::*;

    pub struct EnvShared {
        pub files: BTreeSet<String>,
        pub metadata: FileWatcherMetadata,
        pub is_initial: bool,
    }

    pub struct Env {
        pub instance: Arc<tokio::sync::Mutex<Option<flow_watchman::Env>>>,
        pub shared: Arc<std::sync::Mutex<EnvShared>>,
        pub listening_thread: Arc<tokio::sync::Mutex<Option<tokio::task::JoinHandle<ExitReason>>>>,
        pub changes_condition: Arc<(std::sync::Mutex<bool>, Condvar)>,
        pub init_settings: flow_watchman::InitSettings,
    }

    pub mod watchman_listen_loop {
        use super::*;

        pub fn should_pause() -> bool {
            true
        }

        pub fn extract_hg_update_metadata(
            metadata: Option<&serde_json::Value>,
        ) -> (String, String) {
            match metadata {
                None => ("<UNKNOWN>".to_string(), "<UNKNOWN REV>".to_string()),
                Some(metadata) => {
                    let distance = match metadata.get("distance") {
                        Some(serde_json::Value::String(s)) => s.clone(),
                        Some(serde_json::Value::Number(n)) => n.to_string(),
                        _ => "<UNKNOWN>".to_string(),
                    };
                    let rev = match metadata.get("rev") {
                        Some(serde_json::Value::String(s)) => s.clone(),
                        _ => "<UNKNOWN REV>".to_string(),
                    };
                    (distance, rev)
                }
            }
        }

        pub fn log_state_enter(name: &str, metadata: Option<&serde_json::Value>) {
            let data = metadata.map(|m| m.to_string()).unwrap_or_default();
            flow_event_logger::file_watcher_event_started(name, &data);
        }

        pub fn log_state_leave(name: &str, metadata: Option<&serde_json::Value>) {
            let data = metadata.map(|m| m.to_string()).unwrap_or_default();
            flow_event_logger::file_watcher_event_finished(name, &data);
        }

        pub fn broadcast(env: &Env) {
            let shared = env.shared.lock().unwrap();
            if !shared.files.is_empty() || shared.metadata.missed_changes {
                let (lock, cvar) = &*env.changes_condition;
                let mut changed = lock.lock().unwrap();
                *changed = true;
                cvar.notify_all();
            }
        }

        pub async fn main(env: &Env) -> Result<(), flow_watchman::Failure> {
            let instance = {
                let mut guard = env.instance.lock().await;
                guard.take().expect("watchman instance missing")
            };
            let (instance, pushed_changes) = match flow_watchman::get_changes(instance).await {
                Ok((instance, pushed_changes)) => (instance, pushed_changes),
                Err(flow_watchman::Failure::Dead) => return Err(flow_watchman::Failure::Dead),
                Err(flow_watchman::Failure::Restarted) => {
                    status_stream::file_watcher_deferred("Watchman restart".to_string());
                    log::error!(
                        "Watchman get_changes returned Restarted but env was consumed; treating as Dead"
                    );
                    return Err(flow_watchman::Failure::Dead);
                }
            };
            *env.instance.lock().await = Some(instance);
            match pushed_changes {
                flow_watchman::PushedChanges::FilesChanged {
                    changes,
                    changed_mergebase,
                } => {
                    {
                        let mut shared = env.shared.lock().unwrap();
                        shared.files.extend(changes);
                        let metadata = FileWatcherMetadata {
                            changed_mergebase,
                            missed_changes: false,
                        };
                        shared.metadata =
                            flow_monitor_rpc::monitor_prot::merge_file_watcher_metadata(
                                &shared.metadata,
                                &metadata,
                            );
                    }
                    broadcast(env);
                    Ok(())
                }
                flow_watchman::PushedChanges::MissedChanges {
                    prev_mergebase,
                    mergebase,
                    changes_since_mergebase,
                } => {
                    let changed_mergebase = prev_mergebase != mergebase;
                    if changed_mergebase {
                        log::info!(
                            "Watchman missed changes, and the mergebase changed from {:?} to {:?}.",
                            prev_mergebase,
                            mergebase
                        );
                    } else {
                        log::info!("Watchman missed changes, but the mergebase didn't change.");
                    }
                    log::info!(
                        "Watchman reports {} files have changed since the mergebase",
                        changes_since_mergebase.len()
                    );
                    {
                        let mut shared = env.shared.lock().unwrap();
                        shared.files.extend(changes_since_mergebase);
                        let metadata = FileWatcherMetadata {
                            changed_mergebase: Some(changed_mergebase),
                            missed_changes: true,
                        };
                        shared.metadata =
                            flow_monitor_rpc::monitor_prot::merge_file_watcher_metadata(
                                &shared.metadata,
                                &metadata,
                            );
                    }
                    broadcast(env);
                    Ok(())
                }
                flow_watchman::PushedChanges::StateEnter(name, metadata) => {
                    if name == "hg.update" {
                        let (distance, rev) = extract_hg_update_metadata(metadata.as_ref());
                        log_state_enter(&name, metadata.as_ref());
                        log::info!(
                            "Watchman reports an hg.update just started. Moving {} revs from {}",
                            distance,
                            rev
                        );
                    } else if env.init_settings.defer_states.iter().any(|s| s == &name) {
                        log_state_enter(&name, metadata.as_ref());
                        log::info!(
                            "Watchman reports {} just started. Filesystem notifications are paused.",
                            name
                        );
                        status_stream::file_watcher_deferred(name);
                    }
                    Ok(())
                }
                flow_watchman::PushedChanges::StateLeave(name, metadata) => {
                    if name == "hg.update" {
                        let (distance, rev) = extract_hg_update_metadata(metadata.as_ref());
                        log_state_leave(&name, metadata.as_ref());
                        log::info!(
                            "Watchman reports an hg.update just finished. Moved {} revs to {}",
                            distance,
                            rev
                        );
                    } else if env.init_settings.defer_states.iter().any(|s| s == &name) {
                        log_state_leave(&name, metadata.as_ref());
                        log::info!(
                            "Watchman reports {} ended. Filesystem notifications resumed.",
                            name
                        );
                        status_stream::file_watcher_ready();
                    }
                    Ok(())
                }
            }
        }

        pub fn catch(failure: &flow_watchman::Failure) -> flow_watchman::Failure {
            match failure {
                flow_watchman::Failure::Dead | flow_watchman::Failure::Restarted => {
                    log::error!("Watchman unavailable. Exiting...");
                    failure.clone()
                }
            }
        }
    }

    pub async fn listen(env: Arc<Env>) -> ExitReason {
        loop {
            match watchman_listen_loop::main(&env).await {
                Ok(()) => {}
                Err(failure) => {
                    let failure = watchman_listen_loop::catch(&failure);
                    return match failure {
                        flow_watchman::Failure::Dead => ExitReason::WatcherDied,
                        flow_watchman::Failure::Restarted => ExitReason::WatcherMissedChanges,
                    };
                }
            }
        }
    }

    pub struct Watchman {
        env: Option<Arc<Env>>,
        init_thread:
            Option<tokio::task::JoinHandle<Result<(flow_watchman::Env, BTreeSet<String>), String>>>,
        init_settings: Option<flow_watchman::InitSettings>,
        mergebase_with: String,
        server_options: flow_common::options::Options,
        watchman_options: flow_server_monitor_options::WatchmanOptions,
    }

    impl Watchman {
        pub fn new(
            mergebase_with: String,
            server_options: flow_common::options::Options,
            watchman_options: flow_server_monitor_options::WatchmanOptions,
        ) -> Self {
            Watchman {
                env: None,
                init_thread: None,
                init_settings: None,
                mergebase_with,
                server_options,
                watchman_options,
            }
        }

        fn get_env(&self) -> &Arc<Env> {
            match &self.env {
                None => panic!("Watchman was not initialized"),
                Some(env) => env,
            }
        }

        pub fn get_env_for_waitpid(&self) -> Arc<Env> {
            self.get_env().clone()
        }

        fn log_watch_spec(&self) {
            let file_options = &self.server_options.file_options;
            let extensions =
                flow_server_file_watcher_spec::file_watcher_spec::get_suffixes(file_options);
            let file_names = flow_server_file_watcher_spec::file_watcher_spec::get_file_names(
                &self.server_options,
            );
            let include_dirs =
                flow_server_file_watcher_spec::file_watcher_spec::get_include_dirs_absolute(
                    &self.server_options,
                );
            log::info!(
                "Watchman connection established with watch_spec: extensions=[{}], file_names=[{}], include_dirs=[{}], exclude_dirs=[{}]",
                extensions.join(", "),
                file_names.join(", "),
                include_dirs.join(", "),
                flow_server_file_watcher_spec::file_watcher_spec::EXCLUDE_DIRS.join(", ")
            );
        }
    }

    impl Watcher for Watchman {
        fn name(&self) -> &str {
            "watchman"
        }

        fn debug(&self) -> bool {
            self.watchman_options.debug
        }

        fn start_init(&mut self) {
            let flow_server_monitor_options::WatchmanOptions {
                debug,
                defer_states,
                sync_timeout,
            } = &self.watchman_options;
            let _file_options = &self.server_options.file_options;
            let watchman_expression_terms =
                flow_server_watchman_expression_terms::make(&self.server_options);
            let should_track_mergebase = self.server_options.lazy_mode;
            let settings = flow_watchman::InitSettings {
                debug_logging: *debug,
                defer_states: defer_states.clone(),
                expression_terms: watchman_expression_terms,
                mergebase_with: self.mergebase_with.clone(),
                roots: flow_common::files::watched_paths(&self.server_options.file_options),
                should_track_mergebase,
                subscribe_mode: flow_watchman::SubscribeMode::DeferChanges,
                subscription_prefix: "flow_watcher".to_string(),
                sync_timeout: sync_timeout.map(|t| t as i64),
            };
            self.init_settings = Some(settings.clone());
            self.init_thread =
                Some(runtime::handle().spawn(async move { flow_watchman::init(settings).await }));
        }

        async fn wait_for_init(&mut self, timeout: Option<f64>) -> Result<(), String> {
            let init_thread = self.init_thread.take().expect("init_thread should be set");
            let init_settings = self
                .init_settings
                .clone()
                .expect("init_settings should be set");
            let go = async move {
                match init_thread.await {
                    Ok(Ok((watchman, files))) => Ok((watchman, files)),
                    Ok(Err(msg)) => Err(msg),
                    Err(join_err) => {
                        let msg = format!("Failed to initialize watchman: {}", join_err);
                        flow_event_logger::watchman_uncaught_failure(&msg);
                        Err(msg)
                    }
                }
            };
            let result = match timeout {
                Some(timeout) => {
                    match tokio::time::timeout(Duration::from_secs_f64(timeout), go).await {
                        Ok(r) => r,
                        Err(_) => {
                            Err("Failed to initialize watchman: Watchman timed out".to_string())
                        }
                    }
                }
                None => go.await,
            };
            match result {
                Ok((watchman, files)) => {
                    if self.watchman_options.debug {
                        self.log_watch_spec();
                    }
                    let shared = Arc::new(std::sync::Mutex::new(EnvShared {
                        files,
                        metadata: monitor_prot::empty_file_watcher_metadata(),
                        is_initial: true,
                    }));
                    let new_env = Arc::new(Env {
                        instance: Arc::new(tokio::sync::Mutex::new(Some(watchman))),
                        shared,
                        listening_thread: Arc::new(tokio::sync::Mutex::new(None)),
                        changes_condition: Arc::new((std::sync::Mutex::new(false), Condvar::new())),
                        init_settings,
                    });
                    let listen_env = new_env.clone();
                    let listening =
                        runtime::handle().spawn(async move { listen(listen_env).await });
                    *new_env.listening_thread.lock().await = Some(listening);
                    self.env = Some(new_env);
                    Ok(())
                }
                Err(msg) => Err(msg),
            }
        }

        async fn get_and_clear_changed_files(
            &mut self,
        ) -> (BTreeSet<String>, Option<FileWatcherMetadata>, bool) {
            // Should we throw away metadata even if files is empty? glevi thinks that's fine, since we
            // probably don't care about hg updates or mergebase changing if no files were affected
            let env = self.get_env();
            let mut shared = env.shared.lock().unwrap();
            let ret = (
                std::mem::take(&mut shared.files),
                Some(shared.metadata.clone()),
                shared.is_initial,
            );
            shared.metadata = monitor_prot::empty_file_watcher_metadata();
            shared.is_initial = false;
            ret
        }

        async fn wait_for_changed_files(&mut self) {
            let env = self.get_env();
            let cv = env.changes_condition.clone();
            tokio::task::spawn_blocking(move || {
                let (lock, cvar) = &*cv;
                let mut changed = lock.lock().unwrap();
                while !*changed {
                    changed = cvar.wait(changed).unwrap();
                }
                *changed = false;
            })
            .await
            .expect("spawn_blocking for wait_for_changed_files panicked");
        }

        async fn stop(&mut self) {
            // Flow doesn't own the watchman process, so it's not Flow's job to stop the watchman
            // process. What we can do, though, is stop listening to the messages
            let env = self.get_env().clone();
            log::info!("Canceling Watchman listening thread & closing connection");
            if let Some(handle) = env.listening_thread.lock().await.take() {
                handle.abort();
            }
            if let Some(instance) = env.instance.lock().await.as_ref() {
                flow_watchman::close(instance).await;
            }
        }

        async fn waitpid(&self) -> ExitReason {
            // If watchman dies, we can start it back up again and use clockspec to make sure we didn't
            // miss anything. So from the point of view of the FileWatcher abstraction, watchman never
            // dies and this method can just wait forever.
            //
            // However it's possible that something Really Really Bad might happen to watchman. If
            // the watchman listening thread itself dies, then we need to tell the monitor that this
            // file watcher is dead.
            let env = self.get_env().clone();
            let handle_opt = env.listening_thread.lock().await.take();
            match handle_opt {
                Some(handle) => match handle.await {
                    Ok(reason) => reason,
                    Err(_join_err) => ExitReason::WatcherStopped,
                },
                None => ExitReason::WatcherStopped,
            }
        }

        fn getpid(&self) -> Option<i32> {
            None
        }
    }
}

pub use watchman_file_watcher::Watchman;

pub mod edenfs_file_watcher {
    use super::*;

    pub struct EnvShared {
        pub files: BTreeSet<String>,
        pub metadata: FileWatcherMetadata,
        pub is_initial: bool,
        pub mergebase: Option<String>,
    }

    pub struct Env {
        pub instance: Arc<tokio::sync::Mutex<flow_edenfs_watcher::Instance>>,
        pub notification_fd: RawFd,
        pub shared: Arc<std::sync::Mutex<EnvShared>>,
        pub listening_thread: Arc<tokio::sync::Mutex<Option<tokio::task::JoinHandle<ExitReason>>>>,
        pub changes_condition: Arc<(Mutex<bool>, Condvar)>,
        pub root: PathBuf,
        pub mergebase_with: String,
    }

    // Query VCS for the current mergebase and compare with the stored value.
    // Updates [env.mergebase] if it changed. Returns [Some bool] or [None] on error.
    fn check_mergebase(env: &Env) -> Option<bool> {
        match query_mergebase(&env.root, &env.mergebase_with) {
            Err(_) => {
                log::warn!("EdenFS: failed to query mergebase");
                None
            }
            Ok(new_mergebase) => {
                let mut shared = env.shared.lock().unwrap();
                match shared.mergebase.clone() {
                    None => {
                        shared.mergebase = Some(new_mergebase);
                        None
                    }
                    Some(old_mergebase) => {
                        let changed = new_mergebase != old_mergebase;
                        if changed {
                            log::info!(
                                "EdenFS: mergebase changed from {:?} to {:?}",
                                old_mergebase,
                                new_mergebase
                            );
                            shared.mergebase = Some(new_mergebase);
                        } else {
                            log::debug!("EdenFS: mergebase unchanged at {:?}", new_mergebase);
                        }
                        Some(changed)
                    }
                }
            }
        }
    }

    fn convert_changes(
        changes_list: &[flow_edenfs_watcher::Changes],
    ) -> (BTreeSet<String>, FileWatcherMetadata) {
        // Convert EdenFS watcher changes to a set of file paths and metadata updates.
        // Note: The paths from EdenFS watcher are already absolute (the Rust code joins
        // them with root_absolute), so we just add them directly without concatenating
        // with root again.
        fn add_files(mut files: BTreeSet<String>, paths: &[String]) -> BTreeSet<String> {
            for path in paths {
                files.insert(path.clone());
            }
            files
        }
        let mut files: BTreeSet<String> = BTreeSet::new();
        let mut metadata = monitor_prot::empty_file_watcher_metadata();
        for change in changes_list {
            match change {
                flow_edenfs_watcher::Changes::FileChanges(paths) => {
                    files = add_files(files, paths);
                }
                flow_edenfs_watcher::Changes::CommitTransition {
                    from_commit,
                    to_commit,
                    file_changes,
                } => {
                    log::info!(
                        "EdenFS watcher reports commit transition from {} to {} with {} changed files",
                        from_commit,
                        to_commit,
                        file_changes.len()
                    );
                    files = add_files(files, file_changes);
                    metadata = flow_monitor_rpc::monitor_prot::merge_file_watcher_metadata(
                        &metadata,
                        &FileWatcherMetadata {
                            changed_mergebase: Some(true),
                            missed_changes: false,
                        },
                    );
                }
                flow_edenfs_watcher::Changes::StateEnter(_)
                | flow_edenfs_watcher::Changes::StateLeave(_) => {}
            }
        }
        (files, metadata)
    }

    pub mod edenfs_listen_loop {
        use super::*;

        pub fn should_pause() -> bool {
            true
        }

        pub fn log_state_enter(name: &str) {
            flow_event_logger::file_watcher_event_started(name, "");
        }

        pub fn log_state_leave(name: &str) {
            flow_event_logger::file_watcher_event_finished(name, "");
        }

        pub fn broadcast(env: &Env) {
            let shared = env.shared.lock().unwrap();
            if !shared.files.is_empty() || shared.metadata.missed_changes {
                let (lock, cvar) = &*env.changes_condition;
                let mut changed = lock.lock().unwrap();
                *changed = true;
                cvar.notify_all();
            }
        }

        pub fn handle_state_changes(changes_list: &[flow_edenfs_watcher::Changes]) {
            for change in changes_list {
                match change {
                    flow_edenfs_watcher::Changes::StateEnter(name) => {
                        log_state_enter(name);
                        log::info!(
                            "EdenFS reports {} just started. Filesystem notifications are paused.",
                            name
                        );
                        status_stream::file_watcher_deferred(name.clone());
                    }
                    flow_edenfs_watcher::Changes::StateLeave(name) => {
                        log_state_leave(name);
                        log::info!(
                            "EdenFS reports {} ended. Filesystem notifications resumed.",
                            name
                        );
                        status_stream::file_watcher_ready();
                    }
                    flow_edenfs_watcher::Changes::FileChanges(_)
                    | flow_edenfs_watcher::Changes::CommitTransition { .. } => {}
                }
            }
        }

        pub async fn get_changes_async_lwt(
            env: &Env,
        ) -> Result<
            (
                Vec<flow_edenfs_watcher::Changes>,
                flow_edenfs_watcher::Clock,
                Option<serde_json::Value>,
            ),
            flow_edenfs_watcher::EdenfsWatcherError,
        > {
            // Wait for the notification fd to become readable, then get changes.
            // Uses the persistent Lwt_unix.file_descr created once during init, so
            // that Lwt's internal state for this fd stays consistent across calls.
            let fd = env.notification_fd;
            let wait_result =
                match tokio::io::unix::AsyncFd::with_interest(fd, tokio::io::Interest::READABLE) {
                    Ok(async_fd) => match async_fd.readable().await {
                        Ok(mut guard) => {
                            guard.clear_ready();
                            Ok(())
                        }
                        Err(e) if e.raw_os_error() == Some(libc::EBADF) => {
                            Err(flow_edenfs_watcher::EdenfsWatcherError::EdenfsWatcherError(
                                "Notification fd closed".to_string(),
                            ))
                        }
                        Err(e) => Err(flow_edenfs_watcher::EdenfsWatcherError::EdenfsWatcherError(
                            format!("AsyncFd readable error: {}", e),
                        )),
                    },
                    Err(e) if e.raw_os_error() == Some(libc::EBADF) => {
                        Err(flow_edenfs_watcher::EdenfsWatcherError::EdenfsWatcherError(
                            "Notification fd closed".to_string(),
                        ))
                    }
                    Err(e) => Err(flow_edenfs_watcher::EdenfsWatcherError::EdenfsWatcherError(
                        format!("AsyncFd registration error: {}", e),
                    )),
                };
            match wait_result {
                Ok(()) => {
                    let instance = env.instance.lock().await;
                    flow_edenfs_watcher::get_changes_async(&instance)
                }
                Err(err) => Err(err),
            }
        }

        pub async fn main(env: &Env) -> Result<(), flow_edenfs_watcher::EdenfsWatcherError> {
            match get_changes_async_lwt(env).await {
                Err(flow_edenfs_watcher::EdenfsWatcherError::LostChanges(msg)) => {
                    log::error!("EdenFS watcher lost changes: {}", msg);
                    flow_event_logger::edenfs_watcher_lost_changes(&msg, &msg);
                    {
                        let mut shared = env.shared.lock().unwrap();
                        shared.metadata =
                            flow_monitor_rpc::monitor_prot::merge_file_watcher_metadata(
                                &shared.metadata,
                                &FileWatcherMetadata {
                                    changed_mergebase: None,
                                    missed_changes: true,
                                },
                            );
                    }
                    broadcast(env);
                    Ok(())
                }
                Err(_) if flow_edenfs_watcher::is_instance_destroyed() => {
                    log::info!("EdenFS watcher instance was destroyed (shutdown in progress)");
                    Err(flow_edenfs_watcher::EdenfsWatcherError::EdenfsWatcherError(
                        "Canceled".to_string(),
                    ))
                }
                Err(err) => {
                    let msg = format!("{:?}", err);
                    flow_event_logger::edenfs_watcher_error(&msg, "");
                    Err(err)
                }
                Ok((changes_list, _clock, _telemetry)) => {
                    handle_state_changes(&changes_list);
                    let (new_files, mut new_metadata) = convert_changes(&changes_list);
                    if let Some(true) = new_metadata.changed_mergebase {
                        log::debug!(
                            "EdenFS: CommitTransition reported changed_mergebase=true; verifying with VCS"
                        );
                        let actual_changed = check_mergebase(env);
                        let changed_mergebase = match actual_changed {
                            Some(changed) => {
                                if !changed {
                                    log::info!(
                                        "EdenFS: overriding changed_mergebase from true to false (mergebase did not actually change)"
                                    );
                                }
                                Some(changed)
                            }
                            None => new_metadata.changed_mergebase,
                        };
                        new_metadata.changed_mergebase = changed_mergebase;
                    }
                    {
                        let mut shared = env.shared.lock().unwrap();
                        shared.files.extend(new_files);
                        shared.metadata =
                            flow_monitor_rpc::monitor_prot::merge_file_watcher_metadata(
                                &shared.metadata,
                                &new_metadata,
                            );
                    }
                    broadcast(env);
                    Ok(())
                }
            }
        }

        pub fn catch(
            err: &flow_edenfs_watcher::EdenfsWatcherError,
        ) -> flow_edenfs_watcher::EdenfsWatcherError {
            log::error!("EdenFS watcher unavailable. Exiting...");
            err.clone()
        }
    }

    pub async fn listen(env: Arc<Env>) -> ExitReason {
        loop {
            match edenfs_listen_loop::main(&env).await {
                Ok(()) => {}
                Err(err) => {
                    let err = edenfs_listen_loop::catch(&err);
                    return match err {
                        flow_edenfs_watcher::EdenfsWatcherError::LostChanges(_) => {
                            ExitReason::WatcherMissedChanges
                        }
                        flow_edenfs_watcher::EdenfsWatcherError::EdenfsWatcherError(_)
                        | flow_edenfs_watcher::EdenfsWatcherError::NonEdenMount => {
                            ExitReason::WatcherDied
                        }
                    };
                }
            }
        }
    }

    pub struct EdenFS {
        env: Option<Arc<Env>>,
        init_thread: Option<
            tokio::task::JoinHandle<
                Result<
                    (flow_edenfs_watcher::Instance, flow_edenfs_watcher::Clock),
                    flow_edenfs_watcher::EdenfsWatcherError,
                >,
            >,
        >,
        mergebase_with: String,
        server_options: flow_common::options::Options,
        edenfs_options: flow_server_monitor_options::EdenfsOptions,
        root_path: PathBuf,
        watch_paths: Vec<PathBuf>,
    }

    impl EdenFS {
        pub fn new(
            mergebase_with: String,
            server_options: flow_common::options::Options,
            edenfs_options: flow_server_monitor_options::EdenfsOptions,
        ) -> Self {
            let _file_options = &server_options.file_options;
            let root_path = (*server_options.root).clone();
            let watch_paths = flow_common::files::watched_paths(&server_options.file_options);
            EdenFS {
                env: None,
                init_thread: None,
                mergebase_with,
                server_options,
                edenfs_options,
                root_path,
                watch_paths,
            }
        }

        fn get_env(&self) -> &Arc<Env> {
            match &self.env {
                None => panic!("EdenFS watcher was not initialized"),
                Some(env) => env,
            }
        }

        pub fn get_env_for_waitpid(&self) -> Arc<Env> {
            self.get_env().clone()
        }

        fn log_watch_spec(&self) {
            let file_options = &self.server_options.file_options;
            let extensions =
                flow_server_file_watcher_spec::file_watcher_spec::get_suffixes(file_options);
            let file_names = flow_server_file_watcher_spec::file_watcher_spec::get_file_names(
                &self.server_options,
            );
            let include_dirs =
                flow_server_file_watcher_spec::file_watcher_spec::get_include_dirs_absolute(
                    &self.server_options,
                );
            log::info!(
                "EdenFS connection established with watch_spec: extensions=[{}], file_names=[{}], include_dirs=[{}], exclude_dirs=[{}]",
                extensions.join(", "),
                file_names.join(", "),
                include_dirs.join(", "),
                flow_server_file_watcher_spec::file_watcher_spec::EXCLUDE_DIRS.join(", ")
            );
        }
    }

    impl Watcher for EdenFS {
        fn name(&self) -> &str {
            "edenfs"
        }

        fn debug(&self) -> bool {
            self.edenfs_options.edenfs_debug
        }

        fn start_init(&mut self) {
            let flow_server_monitor_options::EdenfsOptions {
                edenfs_debug,
                edenfs_timeout_secs,
                edenfs_throttle_time_ms,
                edenfs_defer_states,
                edenfs_watchman_fallback: _,
            } = &self.edenfs_options;
            let settings = flow_edenfs_watcher::Settings {
                root: self.root_path.clone(),
                watch_spec: flow_edenfs_watcher::watch_spec(&self.server_options),
                debug_logging: *edenfs_debug,
                timeout_secs: *edenfs_timeout_secs as i64,
                throttle_time_ms: *edenfs_throttle_time_ms as i64,
                report_telemetry: true,
                state_tracking: true,
                sync_queries_obey_deferral: false,
                defer_states: edenfs_defer_states.clone(),
            };
            self.init_thread =
                Some(runtime::handle().spawn(async move { flow_edenfs_watcher::init(settings) }));
        }

        async fn wait_for_init(&mut self, timeout: Option<f64>) -> Result<(), String> {
            let init_thread = self.init_thread.take().expect("init_thread should be set");
            let go = async move {
                match init_thread.await {
                    Ok(Ok((instance, _clock))) => Ok(instance),
                    Ok(Err(err)) => Err(format!("{:?}", err)),
                    Err(join_err) => {
                        let msg = format!("Failed to initialize EdenFS watcher: {}", join_err);
                        flow_event_logger::file_watcher_uncaught_failure(&msg);
                        Err(msg)
                    }
                }
            };
            let result = match timeout {
                Some(timeout) => {
                    match tokio::time::timeout(Duration::from_secs_f64(timeout), go).await {
                        Ok(r) => r,
                        Err(_) => Err("Failed to initialize EdenFS watcher: timed out".to_string()),
                    }
                }
                None => go.await,
            };
            match result {
                Ok(instance) => {
                    if self.edenfs_options.edenfs_debug {
                        self.log_watch_spec();
                    }
                    let notification_fd = match flow_edenfs_watcher::get_notification_fd(&instance)
                    {
                        Ok(fd) => fd,
                        Err(err) => {
                            let msg = format!("{:?}", err);
                            return Err(format!("Failed to get EdenFS notification fd: {}", msg));
                        }
                    };
                    let initial_mergebase =
                        match query_mergebase(&self.root_path, &self.mergebase_with) {
                            Ok(hash) => {
                                log::info!("EdenFS: initial mergebase is {:?}", hash);
                                Some(hash)
                            }
                            Err(_) => {
                                log::warn!("EdenFS: failed to determine initial mergebase");
                                None
                            }
                        };
                    let shared = Arc::new(std::sync::Mutex::new(EnvShared {
                        files: BTreeSet::new(),
                        metadata: monitor_prot::empty_file_watcher_metadata(),
                        is_initial: true,
                        mergebase: initial_mergebase,
                    }));
                    let new_env = Arc::new(Env {
                        instance: Arc::new(tokio::sync::Mutex::new(instance)),
                        notification_fd,
                        shared,
                        listening_thread: Arc::new(tokio::sync::Mutex::new(None)),
                        changes_condition: Arc::new((Mutex::new(false), Condvar::new())),
                        root: self.root_path.clone(),
                        mergebase_with: self.mergebase_with.clone(),
                    });
                    let listen_env = new_env.clone();
                    let listening =
                        runtime::handle().spawn(async move { listen(listen_env).await });
                    *new_env.listening_thread.lock().await = Some(listening);
                    self.env = Some(new_env);
                    if self.server_options.lazy_mode {
                        let changes =
                            changes_since_mergebase(&self.mergebase_with, &self.watch_paths);
                        let env = self.env.as_ref().unwrap();
                        let mut shared = env.shared.lock().unwrap();
                        shared.files.extend(changes);
                    }
                    Ok(())
                }
                Err(msg) => Err(msg),
            }
        }

        async fn get_and_clear_changed_files(
            &mut self,
        ) -> (BTreeSet<String>, Option<FileWatcherMetadata>, bool) {
            let env = self.get_env();
            let mut shared = env.shared.lock().unwrap();
            let ret = (
                std::mem::take(&mut shared.files),
                Some(shared.metadata.clone()),
                shared.is_initial,
            );
            shared.metadata = monitor_prot::empty_file_watcher_metadata();
            shared.is_initial = false;
            ret
        }

        async fn wait_for_changed_files(&mut self) {
            let env = self.get_env();
            let cv = env.changes_condition.clone();
            tokio::task::spawn_blocking(move || {
                let (lock, cvar) = &*cv;
                let mut changed = lock.lock().unwrap();
                while !*changed {
                    changed = cvar.wait(changed).unwrap();
                }
                *changed = false;
            })
            .await
            .expect("spawn_blocking for wait_for_changed_files panicked");
        }

        async fn stop(&mut self) {
            let env = self.get_env().clone();
            log::info!("Canceling EdenFS listening thread");
            if let Some(handle) = env.listening_thread.lock().await.take() {
                handle.abort();
            }
        }

        async fn waitpid(&self) -> ExitReason {
            let env = self.get_env().clone();
            let handle_opt = env.listening_thread.lock().await.take();
            match handle_opt {
                Some(handle) => match handle.await {
                    Ok(reason) => reason,
                    Err(_join_err) => ExitReason::WatcherStopped,
                },
                None => ExitReason::WatcherStopped,
            }
        }

        fn getpid(&self) -> Option<i32> {
            None
        }
    }
}

pub use edenfs_file_watcher::EdenFS;

pub enum AnyWatcher {
    Dummy(Dummy),
    Watchman(Watchman),
    EdenFS(EdenFS),
}

impl AnyWatcher {
    pub fn name(&self) -> &str {
        match self {
            AnyWatcher::Dummy(w) => w.name(),
            AnyWatcher::Watchman(w) => w.name(),
            AnyWatcher::EdenFS(w) => w.name(),
        }
    }

    pub fn debug(&self) -> bool {
        match self {
            AnyWatcher::Dummy(w) => w.debug(),
            AnyWatcher::Watchman(w) => w.debug(),
            AnyWatcher::EdenFS(w) => w.debug(),
        }
    }

    pub fn start_init(&mut self) {
        match self {
            AnyWatcher::Dummy(w) => w.start_init(),
            AnyWatcher::Watchman(w) => w.start_init(),
            AnyWatcher::EdenFS(w) => w.start_init(),
        }
    }

    pub async fn wait_for_init(&mut self, timeout: Option<f64>) -> Result<(), String> {
        match self {
            AnyWatcher::Dummy(w) => w.wait_for_init(timeout).await,
            AnyWatcher::Watchman(w) => w.wait_for_init(timeout).await,
            AnyWatcher::EdenFS(w) => w.wait_for_init(timeout).await,
        }
    }

    pub async fn get_and_clear_changed_files(
        &mut self,
    ) -> (BTreeSet<String>, Option<FileWatcherMetadata>, bool) {
        match self {
            AnyWatcher::Dummy(w) => w.get_and_clear_changed_files().await,
            AnyWatcher::Watchman(w) => w.get_and_clear_changed_files().await,
            AnyWatcher::EdenFS(w) => w.get_and_clear_changed_files().await,
        }
    }

    pub async fn wait_for_changed_files(&mut self) {
        match self {
            AnyWatcher::Dummy(w) => w.wait_for_changed_files().await,
            AnyWatcher::Watchman(w) => w.wait_for_changed_files().await,
            AnyWatcher::EdenFS(w) => w.wait_for_changed_files().await,
        }
    }

    pub async fn stop(&mut self) {
        match self {
            AnyWatcher::Dummy(w) => w.stop().await,
            AnyWatcher::Watchman(w) => w.stop().await,
            AnyWatcher::EdenFS(w) => w.stop().await,
        }
    }

    pub async fn waitpid(&self) -> ExitReason {
        match self {
            AnyWatcher::Dummy(w) => w.waitpid().await,
            AnyWatcher::Watchman(w) => w.waitpid().await,
            AnyWatcher::EdenFS(w) => w.waitpid().await,
        }
    }

    pub fn waitpid_owned(
        &self,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ExitReason> + Send + 'static>> {
        match self {
            AnyWatcher::Dummy(_) => Box::pin(std::future::pending()),
            AnyWatcher::Watchman(w) => {
                let env = w.get_env_for_waitpid();
                Box::pin(async move {
                    let handle_opt = env.listening_thread.lock().await.take();
                    match handle_opt {
                        Some(handle) => match handle.await {
                            Ok(reason) => reason,
                            Err(_join_err) => ExitReason::WatcherStopped,
                        },
                        None => ExitReason::WatcherStopped,
                    }
                })
            }
            AnyWatcher::EdenFS(w) => {
                let env = w.get_env_for_waitpid();
                Box::pin(async move {
                    let handle_opt = env.listening_thread.lock().await.take();
                    match handle_opt {
                        Some(handle) => match handle.await {
                            Ok(reason) => reason,
                            Err(_join_err) => ExitReason::WatcherStopped,
                        },
                        None => ExitReason::WatcherStopped,
                    }
                })
            }
        }
    }

    pub fn getpid(&self) -> Option<i32> {
        match self {
            AnyWatcher::Dummy(w) => w.getpid(),
            AnyWatcher::Watchman(w) => w.getpid(),
            AnyWatcher::EdenFS(w) => w.getpid(),
        }
    }
}
