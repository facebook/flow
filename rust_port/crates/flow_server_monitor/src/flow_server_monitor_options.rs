/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// These are all the little bits of information which the Flow server monitor needs in order to
// function

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct WatchmanOptions {
    /// Turn on debugging messages for the file watcher
    pub debug: bool,
    /// Defer watchman notifications while these states are asserted
    pub defer_states: Vec<String>,
    /// How long to wait for the file watcher to synchronize, in milliseconds
    pub sync_timeout: Option<u32>,
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct EdenfsOptions {
    /// Turn on debugging messages for the EdenFS watcher
    pub edenfs_debug: bool,
    /// Timeout for EdenFS watcher initialization
    pub edenfs_timeout_secs: u32,
    /// Throttle time for EdenFS watcher notifications
    pub edenfs_throttle_time_ms: u32,
    /// States to track and defer in the EdenFS watcher
    pub edenfs_defer_states: Vec<String>,
    /// Skip file diff and signal a restart when a commit transition exceeds this many commits. 0 disables.
    pub edenfs_max_commit_distance: u32,
    /// Watchman options to use if EdenFS watcher fails to initialize (e.g., non-Eden mount)
    pub edenfs_watchman_fallback: WatchmanOptions,
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum FileWatcher {
    NoFileWatcher,
    DFind,
    Watchman(WatchmanOptions),
    EdenFS(EdenfsOptions),
}

pub use FileWatcher::DFind;
pub use FileWatcher::EdenFS;
pub use FileWatcher::NoFileWatcher;
pub use FileWatcher::Watchman;

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct SharedMemConfig {
    pub heap_size: u64,
    pub hash_table_pow: u32,
}

#[derive(Clone)]
pub struct MonitorOptions {
    // Where the monitor logs will go by default
    pub log_file: String,
    // If true then the monitor will exit when the last client exits. This is used by lsp.
    pub autostop: bool,
    // If true then the monitor will always exit when a server exits, and will never try to create
    // a new server. This is currently only used for testing what causes servers to die
    pub no_restart: bool,
    // Where the server logs will go
    pub server_log_file: String,
    // The server's options
    pub server_options: flow_common::options::Options,
    // The explicit lazy-mode CLI override, if one was provided.
    pub lazy_mode: Option<String>,
    // Whether bundled flowlib should be disabled when the monitor spawns servers.
    pub no_flowlib: bool,
    // Whether version checks should be ignored when the monitor spawns servers.
    pub ignore_version: bool,
    // The shared memory config
    pub shared_mem_config: SharedMemConfig,
    // The argv of the process which created the server monitor
    pub argv: Vec<String>,
    // What to use for file watching
    pub file_watcher: FileWatcher,
    // How long to wait for the file watcher to initialize, in seconds
    pub file_watcher_timeout: Option<f64>,
    /// symbolic commit to find changes against
    pub file_watcher_mergebase_with: String,
}

pub fn string_of_file_watcher(file_watcher: &FileWatcher) -> &'static str {
    match file_watcher {
        FileWatcher::NoFileWatcher => "Dummy",
        FileWatcher::DFind => "DFind",
        FileWatcher::Watchman(_) => "Watchman",
        FileWatcher::EdenFS(_) => "EdenFS",
    }
}
