/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::time::SystemTime;

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct MonitorOptions {
    pub file_watcher: String,
    pub vcs: String,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct ServerOptions {
    pub lazy_mode: String,
    pub max_workers: i32,
    pub long_lived_workers: bool,
    pub enabled_rollouts: Vec<String>,
    pub debug: bool,
    pub log_saving: BTreeMap<String, flow_common::options::LogSaving>,
    pub log_file: String,
}

// The logging context is a little bit of singleton state that we want each
// Flow command to hold onto and log with each event. It will also be passed
// from the client to the server and logged with server events.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct LoggingContext {
    // The literal argv for this Flow command.
    pub argv: String,
    // The current working directory.
    pub cwd: String,
    // The Flow command (like check, status, etc).
    pub command: Option<String>,
    // Whether the root is an eden mount.
    pub eden: Option<bool>,
    // The --from flag that most commands should have.
    pub from: Option<String>,
    // The server monitor options.
    pub monitor_options: Option<MonitorOptions>,
    // The Flow root being operated upon. This is a path.
    pub root: Option<String>,
    // The name specified in the .flowconfig. This is some identifier.
    pub root_name: Option<String>,
    // The saved state which this server successfully loaded.
    pub saved_state_filename: Option<String>,
    // The server options.
    pub server_options: Option<ServerOptions>,
    // The time this command started.
    pub start_time: f64,
    // The agent ID from META_3PAI_INVOCATION_ID.
    pub agent_id: Option<String>,
}

// Persistent_context says what conditions were like when the message was received.
#[derive(Clone, Debug)]
pub struct PersistentContext {
    pub start_lsp_state: Option<String>,
    pub start_lsp_state_reason: Option<String>,
    pub start_server_status: Option<String>,
    pub start_watcher_status: Option<String>,
}

// Persistent_delay is an explanation of why a persistent command couldn't be
// handled immediately: what was the server busy doing?
#[derive(Clone, Debug)]
pub struct PersistentDelay {
    pub init_duration: f64,
    pub command_count: i32,
    pub command_duration: f64,
    pub command_worst: Option<String>,
    pub command_worst_duration: Option<f64>,
    pub recheck_count: i32,
    pub recheck_dependent_files: i32,
    pub recheck_changed_files: i32,
    pub recheck_duration: f64,
    pub recheck_worst_duration: Option<f64>,
    pub recheck_worst_dependent_file_count: Option<i32>,
    pub recheck_worst_changed_file_count: Option<i32>,
    pub recheck_worst_cycle_leader: Option<String>,
    pub recheck_worst_cycle_size: Option<i32>,
}

pub type Error = (String, String);

fn logging_enabled() -> &'static Mutex<bool> {
    static LOGGING_ENABLED: OnceLock<Mutex<bool>> = OnceLock::new();
    LOGGING_ENABLED.get_or_init(|| Mutex::new(true))
}

pub fn disable_logging() {
    *logging_enabled().lock().unwrap() = false;
}

fn context() -> &'static Mutex<LoggingContext> {
    static CONTEXT: OnceLock<Mutex<LoggingContext>> = OnceLock::new();
    CONTEXT.get_or_init(|| {
        let argv = std::env::args().fold(String::new(), |acc, arg| {
            if acc.is_empty() {
                arg
            } else {
                acc + " " + &arg
            }
        });
        let cwd = std::env::current_dir()
            .map(|p| p.display().to_string())
            .unwrap_or_default();
        let start_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs_f64();
        Mutex::new(LoggingContext {
            argv,
            cwd,
            command: None,
            eden: None,
            from: None,
            root: None,
            root_name: None,
            saved_state_filename: None,
            start_time,
            monitor_options: None,
            server_options: None,
            agent_id: None,
        })
    })
}

pub fn get_context() -> LoggingContext {
    context().lock().unwrap().clone()
}

// If you are using the logging context for application logic then you are a clown.
// Shame. Shame on us all.
#[allow(non_snake_case)]
pub fn get_from_I_AM_A_CLOWN() -> Option<String> {
    context().lock().unwrap().from.clone()
}

pub fn get_from_i_am_a_clown() -> Option<String> {
    get_from_I_AM_A_CLOWN()
}

pub fn restore_context(restored_context: LoggingContext) {
    // This server might have been started by a monitor process which is already pretty old,
    // so the start_time might be way out of date.
    let start_time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64();
    *context().lock().unwrap() = LoggingContext {
        start_time,
        ..restored_context
    };
}

pub fn set_command(command: Option<String>) {
    context().lock().unwrap().command = command;
}

pub fn set_eden(eden: Option<bool>) {
    context().lock().unwrap().eden = eden;
}

pub fn set_from(from: Option<String>) {
    context().lock().unwrap().from = from;
}

pub fn set_agent_id(agent_id: Option<String>) {
    context().lock().unwrap().agent_id = agent_id;
}

pub fn set_agent_id_from_client_context(client_context: &LoggingContext) {
    context().lock().unwrap().agent_id = client_context.agent_id.clone();
}

pub fn set_root(root: Option<String>) {
    context().lock().unwrap().root = root;
}

pub fn set_root_name(root_name: Option<String>) {
    context().lock().unwrap().root_name = root_name;
}

pub fn set_saved_state_filename(fn_: String) {
    context().lock().unwrap().saved_state_filename = Some(fn_);
}

pub fn set_monitor_options(file_watcher: String, vcs: String) {
    let monitor_options = Some(MonitorOptions { file_watcher, vcs });
    context().lock().unwrap().monitor_options = monitor_options;
}

#[allow(clippy::too_many_arguments)]
pub fn set_server_options(
    lazy_mode: &str,
    max_workers: i32,
    long_lived_workers: bool,
    enabled_rollouts: &BTreeMap<String, String>,
    debug: bool,
    log_saving: &BTreeMap<String, flow_common::options::LogSaving>,
    log_file: &str,
) {
    let enabled_rollouts: Vec<String> = enabled_rollouts
        .iter()
        .map(|(rollout, group)| format!("{}={}", rollout, group))
        .collect();
    let server_options = Some(ServerOptions {
        lazy_mode: lazy_mode.to_string(),
        max_workers,
        long_lived_workers,
        enabled_rollouts,
        debug,
        log_saving: log_saving.clone(),
        log_file: log_file.to_string(),
    });
    context().lock().unwrap().server_options = server_options;
}

#[allow(non_camel_case_types)]
#[derive(Clone, Debug)]
pub enum Event {
    // When the server finishes initializing.
    INIT_END,
    // When any flow command starts.
    INIT_START,
    // When the server loses the lock.
    LOCK_LOST,
    // When the server fails to reaquire the lock.
    LOCK_STOLEN,
    // When the server is out of date.
    OUT_OF_DATE,
    // The response of "flow status".
    STATUS_RESPONSE,
    // When a command exits.
    EXIT,
    // When the server finishes a recheck.
    RECHECK,
    // When a recheck is canceled.
    RECHECK_CANCELED,
    // When the server finishes a series of rechecks where some may have been canceled.
    RECHECK_SERIES,
    // When the server loads from a newer saved state instead of rechecking after a rebase.
    // Followed by a recheck.
    REINIT,
    REINIT_FULL_CHECK,
    // The oom killer killed the server.
    MURDERED_BY_OOM_KILLER,
    // A command from an ephemeral client finished successfully.
    EPHEMERAL_COMMAND_SUCCESS,
    // A command from an ephemeral client failed.
    EPHEMERAL_COMMAND_FAILURE,
    // A command from a persistent client finished successfully.
    PERSISTENT_COMMAND_SUCCESS,
    // A command from a persistent client failed.
    PERSISTENT_COMMAND_FAILURE,
    // A non-command-related expected error, e.g. server shuts down.
    PERSISTENT_EXPECTED_ERROR,
    // A non-command-related unhandled exception.
    PERSISTENT_UNEXPECTED_ERROR,
    // We successfully fetched a saved state.
    SAVED_STATE_FB_FETCHER_SUCCESS,
    // We failed to fetch a saved state.
    SAVED_STATE_FB_FETCHER_ERROR,
    // Finished loading saved state.
    LOAD_SAVED_STATE_SUCCESS,
    // Tried and failed to load saved state.
    LOAD_SAVED_STATE_ERROR,
    // The server has been idle (not handling requests or rechecks) for N seconds.
    IDLE_HEARTBEAT,
    // The Flow LSP sent the client the live parse errors.
    LIVE_PARSE_ERRORS,
    // The Flow LSP sent the client the live non-parse errors.
    LIVE_NON_PARSE_ERRORS,
    // The Flow LSP failed to send the client the live non-parse errors.
    LIVE_NON_PARSE_ERRORS_FAILED,
    // The file watcher notified us that an event started.
    FILE_WATCHER_EVENT_STARTED,
    // The file watcher notified us that an event finished.
    FILE_WATCHER_EVENT_FINISHED,
    // The file watcher encountered an uncaught failure.
    FILE_WATCHER_UNCAUGHT_FAILURE,
    WATCHMAN_ERROR,
    WATCHMAN_WARNING,
    WATCHMAN_UNCAUGHT_FAILURE,
    WATCHMAN_CONNECTION_REESTABLISHED,
    WATCHMAN_CONNECTION_REESTABLISHMENT_FAILED,
    SHAREDMEM_GC_RAN,
    SHAREDMEM_INIT_DONE,
    SHAREDMEM_FAILED_ANONYMOUS_MEMFD_INIT,
    WORKER_EXCEPTION,
    DFIND_READY,
    PARSING_EXCEPTION,
    EDENFS_WATCHER_FALLBACK,
    EDENFS_WATCHER_ERROR,
    EDENFS_WATCHER_LOST_CHANGES,
    EDENFS_WATCHER_NON_EDEN_WWW,
}

pub fn string_of_event(event: &Event) -> &'static str {
    match event {
        Event::INIT_END => "INIT_END",
        Event::INIT_START => "INIT_START",
        Event::LOCK_LOST => "LOCK_LOST",
        Event::LOCK_STOLEN => "LOCK_STOLEN",
        Event::OUT_OF_DATE => "OUT_OF_DATE",
        Event::STATUS_RESPONSE => "STATUS_RESPONSE",
        Event::EXIT => "EXIT",
        Event::RECHECK => "RECHECK",
        Event::RECHECK_CANCELED => "RECHECK_CANCELED",
        Event::RECHECK_SERIES => "RECHECK_SERIES",
        Event::REINIT => "REINIT",
        Event::REINIT_FULL_CHECK => "REINIT_FULL_CHECK",
        Event::MURDERED_BY_OOM_KILLER => "MURDERED_BY_OOM_KILLER",
        Event::EPHEMERAL_COMMAND_SUCCESS => "EPHEMERAL_COMMAND_SUCCESS",
        Event::EPHEMERAL_COMMAND_FAILURE => "EPHEMERAL_COMMAND_FAILURE",
        Event::PERSISTENT_COMMAND_SUCCESS => "PERSISTENT_COMMAND_SUCCESS",
        Event::PERSISTENT_COMMAND_FAILURE => "PERSISTENT_COMMAND_FAILURE",
        Event::PERSISTENT_EXPECTED_ERROR => "PERSISTENT_EXPECTED_ERROR",
        Event::PERSISTENT_UNEXPECTED_ERROR => "PERSISTENT_UNEXPECTED_ERROR",
        Event::SAVED_STATE_FB_FETCHER_SUCCESS => "SAVED_STATE_FB_FETCHER_SUCCESS",
        Event::SAVED_STATE_FB_FETCHER_ERROR => "SAVED_STATE_FB_FETCHER_ERROR",
        Event::LOAD_SAVED_STATE_SUCCESS => "LOAD_SAVED_STATE_SUCCESS",
        Event::LOAD_SAVED_STATE_ERROR => "LOAD_SAVED_STATE_ERROR",
        Event::IDLE_HEARTBEAT => "IDLE_HEARTBEAT",
        Event::LIVE_PARSE_ERRORS => "LIVE_PARSE_ERRORS",
        Event::LIVE_NON_PARSE_ERRORS => "LIVE_NON_PARSE_ERRORS",
        Event::LIVE_NON_PARSE_ERRORS_FAILED => "LIVE_NON_PARSE_ERRORS_FAILED",
        Event::FILE_WATCHER_EVENT_STARTED => "FILE_WATCHER_EVENT_STARTED",
        Event::FILE_WATCHER_EVENT_FINISHED => "FILE_WATCHER_EVENT_FINISHED",
        Event::FILE_WATCHER_UNCAUGHT_FAILURE => "FILE_WATCHER_UNCAUGHT_FAILURE",
        Event::WATCHMAN_ERROR => "WATCHMAN_ERROR",
        Event::WATCHMAN_WARNING => "WATCHMAN_WARNING",
        Event::WATCHMAN_UNCAUGHT_FAILURE => "WATCHMAN_UNCAUGHT_FAILURE",
        Event::WATCHMAN_CONNECTION_REESTABLISHED => "WATCHMAN_CONNECTION_REESTABLISHED",
        Event::WATCHMAN_CONNECTION_REESTABLISHMENT_FAILED => {
            "WATCHMAN_CONNECTION_REESTABLISHMENT_FAILED"
        }
        Event::SHAREDMEM_GC_RAN => "SHAREDMEM_GC_RAN",
        Event::SHAREDMEM_INIT_DONE => "SHAREDMEM_INIT_DONE",
        Event::SHAREDMEM_FAILED_ANONYMOUS_MEMFD_INIT => "SHAREDMEM_FAILED_ANONYMOUS_MEMFD_INIT",
        Event::WORKER_EXCEPTION => "WORKER_EXCEPTION",
        Event::DFIND_READY => "DFIND_READY",
        Event::PARSING_EXCEPTION => "PARSING_EXCEPTION",
        Event::EDENFS_WATCHER_FALLBACK => "EDENFS_WATCHER_FALLBACK",
        Event::EDENFS_WATCHER_ERROR => "EDENFS_WATCHER_ERROR",
        Event::EDENFS_WATCHER_LOST_CHANGES => "EDENFS_WATCHER_LOST_CHANGES",
        Event::EDENFS_WATCHER_NON_EDEN_WWW => "EDENFS_WATCHER_NON_EDEN_WWW",
    }
}

pub fn status_response(_num_errors: i32) {}

pub fn init_done(
    _first_internal_error: Option<&str>,
    _saved_state_fetcher: &str,
    _profiling: &serde_json::Value,
) {
}

pub fn init_flow_command(_init_id: &str) {}

pub fn init_worker(_init_id: &str, _arg: &serde_json::Value) {}

pub fn should_log() -> bool {
    *logging_enabled().lock().unwrap()
}

pub fn lock_lost(_arg: &str) {}

pub fn lock_stolen(_arg: &str) {}

pub fn out_of_date() {}

pub fn exit(_error: Option<&Error>, _msg: &str, _code: i32) {}

pub fn report_from_monitor_server_exit_due_to_signal(_signal: i32) {}

#[allow(clippy::too_many_arguments)]
pub fn recheck(
    _modified_count: i32,
    _deleted_count: i32,
    _merged_dependency_count: i32,
    _dependent_file_count: i32,
    _merge_skip_count: i32,
    _check_skip_count: i32,
    _profiling: &serde_json::Value,
    _time_to_resolve_all_type_errors: Option<f64>,
    _time_to_resolve_all_type_errors_in_one_file: Option<f64>,
    _time_to_resolve_all_subtyping_errors: Option<f64>,
    _time_to_resolve_all_subtyping_errors_in_one_file: Option<f64>,
    _first_internal_error: Option<&str>,
    _slowest_file: Option<&str>,
    _num_slow_files: Option<i32>,
    _scm_changed_mergebase: Option<bool>,
) {
}

pub fn log_typing_errors(_data: &serde_json::Value) {}

pub fn recheck_canceled(
    _priority: &str,
    _num_files_to_prioritize: i32,
    _num_files_to_recheck: i32,
    _num_files_to_force: i32,
) {
}

pub fn recheck_series(_recheck_count: i32, _profiling: &serde_json::Value) {}

pub fn reinit(_reason: &str, _profiling: &serde_json::Value) {}

pub fn reinit_full_check(_profiling: &serde_json::Value) {}

pub fn murdered_by_oom_killer(_arg: &str) {}

pub fn ephemeral_command_success(
    _json_data: &serde_json::Value,
    _client_context: &LoggingContext,
    _profiling: &serde_json::Value,
) {
}

pub fn ephemeral_command_failure(_json_data: &serde_json::Value, _client_context: &LoggingContext) {
}

#[allow(clippy::too_many_arguments)]
pub fn persistent_command_success(
    _server_logging_context: &LoggingContext,
    _request_id: &str,
    _method_name: &str,
    _request: &serde_json::Value,
    _extra_data: &serde_json::Value,
    _client_context: &LoggingContext,
    _persistent_context: &PersistentContext,
    _persistent_delay: &PersistentDelay,
    _server_profiling: Option<&serde_json::Value>,
    _client_duration: Option<f64>,
    _wall_start: f64,
    _activity_key: Option<&serde_json::Value>,
    _error: Option<&Error>,
) {
}

#[allow(clippy::too_many_arguments)]
pub fn persistent_command_failure(
    _server_logging_context: &LoggingContext,
    _request: &serde_json::Value,
    _extra_data: &serde_json::Value,
    _client_context: &LoggingContext,
    _persistent_context: &PersistentContext,
    _persistent_delay: &PersistentDelay,
    _server_profiling: Option<&serde_json::Value>,
    _client_duration: Option<f64>,
    _wall_start: f64,
    _activity_key: Option<&serde_json::Value>,
    _error: &Error,
) {
}

pub fn persistent_expected_error(
    _request: &serde_json::Value,
    _client_context: &LoggingContext,
    _activity_key: Option<&serde_json::Value>,
    _error: &Error,
) {
}

pub fn persistent_unexpected_error(
    _request: &serde_json::Value,
    _client_context: &LoggingContext,
    _activity_key: Option<&serde_json::Value>,
    _error: &Error,
) {
}

#[allow(clippy::too_many_arguments)]
pub fn saved_state_fb_fetcher_success(
    _repo_root: &str,
    _merge_base_hash: &str,
    _merge_base_timestamp: f64,
    _saved_state_hash: &str,
    _changed_files_count: i32,
    _saved_state_filename: &str,
    _profiling: &serde_json::Value,
) {
}

pub fn saved_state_fb_fetcher_error(_step: &str, _trace: &str, _profiling: &serde_json::Value) {}

pub fn load_saved_state_success(_changed_files_count: i32) {}

pub fn load_saved_state_error(
    _saved_state_filename: &str,
    _changed_files_count: i32,
    _invalid_reason: &str,
    _trace: &str,
) {
}

pub fn idle_heartbeat(_idle_time: f64, _profiling: &serde_json::Value) {}

pub fn live_parse_errors(
    _request: &serde_json::Value,
    _data: &serde_json::Value,
    _wall_start: f64,
) {
}

pub fn live_non_parse_errors(
    _request: &serde_json::Value,
    _data: &serde_json::Value,
    _wall_start: f64,
) {
}

pub fn live_non_parse_errors_failed(
    _request: &serde_json::Value,
    _data: &serde_json::Value,
    _wall_start: f64,
) {
}

pub fn file_watcher_event_started(_name: &str, _data: &str) {}

pub fn file_watcher_event_finished(_name: &str, _data: &str) {}

pub fn file_watcher_uncaught_failure(_msg: &str) {}

pub fn watchman_error(_request: Option<&str>, _response: Option<&str>, _msg: &str) {}

pub fn watchman_warning(_warning: &str) {}

pub fn watchman_uncaught_failure(_msg: &str) {}

pub fn watchman_connection_reestablished(_downtime: f64) {}

pub fn watchman_connection_reestablishment_failed(_msg: &str) {}

pub fn sharedmem_gc_ran(_a: &str, _b: f64, _c: f64, _d: f64) {}

pub fn sharedmem_init_done(_size: u64) {}

pub fn sharedmem_failed_memfd_init(_msg: &str) {}

pub fn worker_exception(_msg: &str) {}

pub fn dfind_ready(_a: &str, _b: f64) {}

pub fn parsing_exception(_msg: &str) {}

pub fn set_file_watcher(_name: &str) {}

pub fn edenfs_watcher_fallback(_msg: &str) {}

pub fn edenfs_watcher_non_eden_www(_backtrace: &str) {}

pub fn edenfs_watcher_error(_msg: &str, _backtrace: &str) {}

pub fn edenfs_watcher_lost_changes(_msg: &str, _backtrace: &str) {}
