/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This crate is the OSS-facing facade for the EdenFS watcher used by Flow.
// In fbcode builds on Unix targets the public API is provided by
// `flow_facebook_edenfs_watcher`, which re-exports the underlying
// `rust_edenfs_watcher` types. `rust_edenfs_watcher`'s OCaml-FFI surface is
// cfg-gated to Linux internally, so its pure-Rust `flow_api` module
// compiles on macOS too — but the file-descriptor / `poll`-based
// notification mechanism uses `std::os::fd` and Unix `poll`, so Windows
// (where neither exists natively) and OSS builds (where `crates/facebook/`
// is stripped by ShipIt) fall back to the in-file stub whose
// `is_available()` returns `false`.

#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::ApplyIncomingChangesTelemetry;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::AsyncTelemetry;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::Changes;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::Clock;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::EdenfsWatcherError;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::Instance;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::InstanceGetAllFilesTelemetry;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::InstanceGetChangesAsyncTelemetry;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::InstanceGetChangesSyncTelemetry;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::Settings;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::StandaloneGetChangesSinceTelemetry;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::TranslationTelemetry;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::WatchSpec;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::add_hook_upon_clean_exit;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::get_changes_async;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::get_notification_fd;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::hooks_upon_clean_exit;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::init;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::is_available;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::is_instance_destroyed;
#[cfg(all(fbcode_build, unix))]
pub use flow_facebook_edenfs_watcher::watch_spec;

#[cfg(any(not(fbcode_build), not(unix)))]
mod stub {
    use std::path::PathBuf;
    use std::sync::Mutex;
    use std::sync::OnceLock;

    use flow_common::options::Options;

    pub struct TranslationTelemetry {
        pub commit_transition_count: isize,
        pub commit_transition_duration: isize,
        pub directory_rename_count: isize,
        pub directory_rename_duration: isize,
        pub raw_changes_count: isize,
        pub translated_files_count: isize,
        pub duration: isize,
    }

    pub struct ApplyIncomingChangesTelemetry {
        pub init_spurious_enters: isize,
        pub init_spurious_leaves: isize,
        pub duplicate_leaves: isize,
    }

    pub struct AsyncTelemetry {
        pub worker_restart_count: isize,
        pub notification_count: isize,
        pub aggregated_translation_telemetry: TranslationTelemetry,
        pub aggregated_apply_incoming_changes_telemetry: ApplyIncomingChangesTelemetry,
    }

    pub struct InstanceGetChangesSyncTelemetry {
        pub duration: isize,
        pub eden_get_changes_since_duration: isize,
        pub async_telemetry: AsyncTelemetry,
        pub worker_reset_duration: isize,
        pub translation_telemetry: Option<TranslationTelemetry>,
        pub apply_incoming_changes_telemetry: ApplyIncomingChangesTelemetry,
    }

    pub struct InstanceGetChangesAsyncTelemetry {
        pub duration: isize,
        pub async_telemetry: AsyncTelemetry,
    }

    pub struct InstanceGetAllFilesTelemetry {
        pub duration: isize,
        pub eden_glob_files_duration: isize,
        pub post_processing_duration: isize,
    }

    pub struct StandaloneGetChangesSinceTelemetry {
        pub duration: isize,
        pub setup_duration: isize,
        pub eden_get_changes_since_duration: isize,
        pub translation_telemetry: Option<TranslationTelemetry>,
    }

    pub struct WatchSpec {
        pub extensions: Vec<String>,
        pub file_names: Vec<String>,
        pub include_dirs: Vec<String>,
        pub exclude_dirs: Vec<String>,
    }

    pub struct Settings {
        pub root: PathBuf,
        pub watch_spec: WatchSpec,
        pub debug_logging: bool,
        pub timeout_secs: isize,
        pub throttle_time_ms: isize,
        pub report_telemetry: bool,
        pub state_tracking: bool,
        pub sync_queries_obey_deferral: bool,
        pub defer_states: Vec<String>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Changes {
        FileChanges(Vec<String>),
        CommitTransition {
            from_commit: String,
            to_commit: String,
            file_changes: Vec<String>,
        },
        StateEnter(String),
        StateLeave(String),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum EdenfsWatcherError {
        EdenfsWatcherError(String),
        LostChanges(String),
        NonEdenMount,
    }

    pub type Clock = String;

    pub struct Instance {
        #[allow(dead_code)]
        pub handle: InstanceHandleFfi,
    }

    pub struct InstanceHandleFfi;

    pub fn init(_settings: Settings) -> Result<(Instance, Clock), EdenfsWatcherError> {
        panic!("not implemented")
    }

    pub fn get_changes_async(
        _instance: &Instance,
    ) -> Result<(Vec<Changes>, Clock, Option<serde_json::Value>), EdenfsWatcherError> {
        panic!("not implemented")
    }

    pub fn get_notification_fd(
        _instance: &Instance,
    ) -> Result<std::os::raw::c_int, EdenfsWatcherError> {
        panic!("not implemented")
    }

    pub fn is_available() -> bool {
        false
    }

    pub fn watch_spec(_options: &Options) -> WatchSpec {
        panic!("not implemented")
    }

    pub fn hooks_upon_clean_exit() -> &'static Mutex<Vec<Box<dyn Fn() + Send + 'static>>> {
        static HOOKS: OnceLock<Mutex<Vec<Box<dyn Fn() + Send + 'static>>>> = OnceLock::new();
        HOOKS.get_or_init(|| Mutex::new(Vec::new()))
    }

    pub fn add_hook_upon_clean_exit(hook: Box<dyn Fn() + Send + 'static>) {
        let mut hooks = hooks_upon_clean_exit()
            .lock()
            .expect("edenfs hooks mutex poisoned");
        hooks.insert(0, hook);
    }

    pub fn is_instance_destroyed() -> bool {
        false
    }
}

#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::ApplyIncomingChangesTelemetry;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::AsyncTelemetry;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::Changes;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::Clock;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::EdenfsWatcherError;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::Instance;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::InstanceGetAllFilesTelemetry;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::InstanceGetChangesAsyncTelemetry;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::InstanceGetChangesSyncTelemetry;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::Settings;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::StandaloneGetChangesSinceTelemetry;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::TranslationTelemetry;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::WatchSpec;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::add_hook_upon_clean_exit;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::get_changes_async;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::get_notification_fd;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::hooks_upon_clean_exit;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::init;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::is_available;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::is_instance_destroyed;
#[cfg(any(not(fbcode_build), not(unix)))]
pub use stub::watch_spec;
