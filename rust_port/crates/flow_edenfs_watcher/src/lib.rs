/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::os::fd::RawFd;
use std::path::PathBuf;
use std::sync::Mutex;
use std::sync::OnceLock;

use flow_common::options::Options;

pub struct TranslationTelemetry {
    pub commit_transition_count: i64,
    pub commit_transition_duration: i64,
    pub directory_rename_count: i64,
    pub directory_rename_duration: i64,
    pub raw_changes_count: i64,
    pub translated_files_count: i64,
    pub duration: i64,
}

pub fn yojson_of_translation_telemetry(_: &TranslationTelemetry) -> serde_json::Value {
    panic!("not implemented")
}

pub struct AsyncTelemetry {
    pub worker_restart_count: i64,
    pub notification_count: i64,
    pub aggregated_translation_telemetry: TranslationTelemetry,
}

pub fn yojson_of_async_telemetry(_: &AsyncTelemetry) -> serde_json::Value {
    panic!("not implemented")
}

pub struct InstanceGetChangesSyncTelemetry {
    pub duration: i64,
    pub eden_get_changes_since_duration: i64,
    pub async_telemetry: AsyncTelemetry,
    pub worker_reset_duration: i64,
    pub translation_telemetry: Option<TranslationTelemetry>,
}

pub fn yojson_of_instance_get_changes_sync_telemetry(
    _: &InstanceGetChangesSyncTelemetry,
) -> serde_json::Value {
    panic!("not implemented")
}

pub struct InstanceGetChangesAsyncTelemetry {
    pub duration: i64,
    pub async_telemetry: AsyncTelemetry,
}

pub fn yojson_of_instance_get_changes_async_telemetry(
    _: &InstanceGetChangesAsyncTelemetry,
) -> serde_json::Value {
    panic!("not implemented")
}

pub struct InstanceGetAllFilesTelemetry {
    pub duration: i64,
    pub eden_glob_files_duration: i64,
    pub post_processing_duration: i64,
}

pub fn yojson_of_instance_get_all_files_telemetry(
    _: &InstanceGetAllFilesTelemetry,
) -> serde_json::Value {
    panic!("not implemented")
}

pub struct StandaloneGetChangesSinceTelemetry {
    pub duration: i64,
    pub setup_duration: i64,
    pub eden_get_changes_since_duration: i64,
    pub translation_telemetry: Option<TranslationTelemetry>,
}

pub fn yojson_of_standalone_get_changes_since_telemetry(
    _: &StandaloneGetChangesSinceTelemetry,
) -> serde_json::Value {
    panic!("not implemented")
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
    pub timeout_secs: i64,
    pub throttle_time_ms: i64,
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

pub fn get_notification_fd(_instance: &Instance) -> Result<RawFd, EdenfsWatcherError> {
    panic!("not implemented")
}

pub fn is_available() -> bool {
    false
}

pub fn watch_spec(_options: &Options) -> WatchSpec {
    panic!("not implemented")
}

fn hooks_upon_clean_exit_storage() -> &'static Mutex<Vec<Box<dyn Fn() + Send + 'static>>> {
    static HOOKS: OnceLock<Mutex<Vec<Box<dyn Fn() + Send + 'static>>>> = OnceLock::new();
    HOOKS.get_or_init(|| Mutex::new(Vec::new()))
}

pub fn add_hook_upon_clean_exit(hook: Box<dyn Fn() + Send + 'static>) {
    let mut hooks = hooks_upon_clean_exit_storage().lock().unwrap();
    hooks.insert(0, hook);
}

pub fn take_hooks_upon_clean_exit() -> Vec<Box<dyn Fn() + Send + 'static>> {
    let mut hooks = hooks_upon_clean_exit_storage().lock().unwrap();
    std::mem::take(&mut *hooks)
}

pub fn is_instance_destroyed() -> bool {
    false
}
