/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    serde::Serialize,
    serde::Deserialize
)]
pub enum FlowExitStatus {
    // Signaled
    Interrupted,
    // The generic 0 exit code
    NoError,
    // Killed by Windows task manage
    WindowsKilledByTaskManager,
    // There are flow errors
    TypeError,
    // A command with a timeout timed out
    OutOfTime,
    // Failed to kill a server
    KillError,
    // The Flow server appears unused so it died out of sadness
    UnusedServer,
    // There is no server running and we were told not to start one
    NoServerRunning,
    // Ran out of retries
    OutOfRetries,
    // Invalid .flowconfig
    InvalidFlowconfig,
    // Provided path is not a file as required
    PathIsNotAFile,
    // Different binaries being used together
    BuildIdMismatch,
    // Generic "Bad Input" kind of error
    InputError,
    // Failed to acquire lock or lost lock
    LockStolen,
    // Specific error for not being able to find a .flowconfig
    CouldNotFindFlowconfig,
    // Failed to extract flowlibs into temp dir
    CouldNotExtractFlowlibs,
    // Generic out-of-date error. This could be a version thing or maybe
    // something changed and Flow can't handle it incrementally yet
    ServerOutOfDate,
    // When the shared memory is missing space (e.g. full /dev/shm)
    OutOfSharedMemory,
    // The .flowconfig has changed and we're out of date
    FlowconfigChanged,
    // Failed to parse the command line or misuse of command line arguments
    CommandlineUsageError,
    // No input
    NoInput,
    // Failed to start a server
    ServerStartFailed,
    // Something went wrong with extracting the flowlib
    MissingFlowlib,
    // Flow monitor had been instructed to exit when there were no more clients
    Autostop,
    // Server exited because the monitor asked it to
    KilledByMonitor,
    // The saved state file is invalid and we're running with --saved-state-no-fallback
    InvalidSavedState,
    // The server would like to restart, likely since re-init'ing is faster than a recheck
    Restart,
    // The hack code might throw this
    SocketError,
    // The hack code might throw this
    DfindDied,
    // A fatal error with Watchman
    WatchmanError,
    // A fatal error with Watchman (TODO: dedupe with Watchman_error)
    WatchmanFailed,
    // Watchman restarted
    FileWatcherMissedChanges,
    // Shared memory hash table is full
    HashTableFull,
    // Shared memory heap is full
    HeapFull,
    // EventLogger daemon ran out of restarts
    EventLoggerRestartOutOfRetries,
    // A generic something-else-went-wrong
    UnknownError,
    // The EdenFS watcher failed in a way we cannot recover from
    EdenfsWatcherFailed,
    // EdenFS watcher lost track of changes (e.g., due to an Eden restart)
    EdenfsWatcherLostChanges,
}

// Exit codes are part of Flow's API and thus changing exit codes is a
// breaking change to Flow's API. Tools that call Flow may be watching for
// certain exit codes.
//
// In reality, probably no one cares about many of these exit codes. The ones
// I know are definitely being watched for are:
//
// No_error
// Type_error
// Out_of_time
pub fn error_code(status: FlowExitStatus) -> i32 {
    use FlowExitStatus::*;
    match status {
        Interrupted => -6,
        NoError => 0,
        WindowsKilledByTaskManager => 1,
        TypeError => 2,
        OutOfTime => 3,
        KillError => 4,
        UnusedServer => 5,
        NoServerRunning => 6,
        OutOfRetries => 7,
        InvalidFlowconfig => 8,
        BuildIdMismatch => 9,
        InputError => 10,
        LockStolen => 11,
        CouldNotFindFlowconfig => 12,
        ServerOutOfDate => 13,
        OutOfSharedMemory => 15,
        FlowconfigChanged => 16,
        PathIsNotAFile => 17,
        Autostop => 18,
        KilledByMonitor => 19,
        InvalidSavedState => 20,
        Restart => 21,
        CouldNotExtractFlowlibs => 22,
        // EX_USAGE -- command line usage error -- from glibc's sysexits.h
        CommandlineUsageError => 64,
        NoInput => 66,
        ServerStartFailed => 78,
        MissingFlowlib => 97,
        SocketError => 98,
        DfindDied => 99,
        WatchmanError => 101,
        HashTableFull => 102,
        HeapFull => 103,
        WatchmanFailed => 104,
        FileWatcherMissedChanges => 105,
        EventLoggerRestartOutOfRetries => 108,
        UnknownError => 110,
        EdenfsWatcherFailed => 111,
        EdenfsWatcherLostChanges => 112,
    }
}

pub fn to_string(status: FlowExitStatus) -> &'static str {
    use FlowExitStatus::*;
    match status {
        Interrupted => "Interrupted",
        NoError => "Ok",
        WindowsKilledByTaskManager => "Windows_killed_by_task_manager",
        TypeError => "Type_error",
        OutOfTime => "Out_of_time",
        KillError => "Kill_error",
        UnusedServer => "Unused_server",
        NoServerRunning => "No_server_running",
        OutOfRetries => "Out_of_retries",
        InvalidFlowconfig => "Invalid_flowconfig",
        PathIsNotAFile => "Path_is_not_a_file",
        BuildIdMismatch => "Build_id_mismatch",
        InputError => "Input_error",
        LockStolen => "Lock_stolen",
        CouldNotFindFlowconfig => "Could_not_find_flowconfig",
        CouldNotExtractFlowlibs => "Could_not_extract_flowlibs",
        ServerOutOfDate => "Server_out_of_date",
        OutOfSharedMemory => "Out_of_shared_memory",
        FlowconfigChanged => "Flowconfig_changed",
        CommandlineUsageError => "Commandline_usage_error",
        NoInput => "No_input",
        ServerStartFailed => "Server_start_failed",
        MissingFlowlib => "Missing_flowlib",
        Autostop => "Autostop",
        KilledByMonitor => "Killed_by_monitor",
        InvalidSavedState => "Invalid_saved_state",
        Restart => "Restart",
        SocketError => "Socket_error",
        DfindDied => "Dfind_died",
        WatchmanError => "Watchman_error",
        WatchmanFailed => "Watchman_failed",
        FileWatcherMissedChanges => "File_watcher_missed_changes",
        HashTableFull => "Hash_table_full",
        HeapFull => "Heap_full",
        EventLoggerRestartOutOfRetries => "EventLogger_restart_out_of_retries",
        UnknownError => "Unknown_error",
        EdenfsWatcherFailed => "Edenfs_watcher_failed",
        EdenfsWatcherLostChanges => "Edenfs_watcher_lost_changes",
    }
}

pub fn json_props_of_t(
    status: FlowExitStatus,
    msg: Option<&str>,
) -> Vec<(String, serde_json::Value)> {
    let mut exit_props = vec![
        (
            "code".to_string(),
            serde_json::Value::Number(error_code(status).into()),
        ),
        (
            "reason".to_string(),
            serde_json::Value::String(to_string(status).to_string()),
        ),
    ];
    if let Some(msg) = msg {
        exit_props.push((
            "msg".to_string(),
            serde_json::Value::String(msg.to_string()),
        ));
    }
    vec![
        (
            "flowVersion".to_string(),
            serde_json::Value::String(flow_common::flow_version::VERSION.to_string()),
        ),
        (
            "exit".to_string(),
            serde_json::Value::Object(exit_props.into_iter().collect()),
        ),
    ]
}

pub fn exit(status: FlowExitStatus) -> ! {
    std::process::exit(error_code(status))
}

pub fn exit_with_msg(status: FlowExitStatus, msg: &str) -> ! {
    eprintln!("{}", msg);
    std::process::exit(error_code(status))
}
