/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// (* These are all the little bits of information which the Flow server monitor needs in order to
//  * function *)

// type watchman_options = {
//   debug: bool;  (** Turn on debugging messages for the file watcher *)
//   defer_states: string list;  (** Defer watchman notifications while these states are asserted *)
//   sync_timeout: int option;
//       (** How long to wait for the file watcher to synchronize, in milliseconds *)
// }
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WatchmanOptions {
    pub debug: bool,
    pub defer_states: Vec<String>,
    pub sync_timeout: Option<u32>,
}

// type edenfs_options = {
//   edenfs_debug: bool;  (** Turn on debugging messages for the EdenFS watcher *)
//   edenfs_timeout_secs: int;  (** Timeout for EdenFS watcher initialization *)
//   edenfs_throttle_time_ms: int;  (** Throttle time for EdenFS watcher notifications *)
//   edenfs_defer_states: string list;  (** States to track and defer in the EdenFS watcher *)
//   edenfs_watchman_fallback: watchman_options;
//       (** Watchman options to use if EdenFS watcher fails to initialize (e.g., non-Eden mount) *)
// }
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EdenfsOptions {
    pub edenfs_debug: bool,
    pub edenfs_timeout_secs: u32,
    pub edenfs_throttle_time_ms: u32,
    pub edenfs_defer_states: Vec<String>,
    pub edenfs_watchman_fallback: WatchmanOptions,
}

// type file_watcher =
//   | NoFileWatcher
//   | DFind
//   | Watchman of watchman_options
//   | EdenFS of edenfs_options
#[derive(Clone, Debug, PartialEq, Eq)]
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

pub fn string_of_file_watcher(file_watcher: &FileWatcher) -> &'static str {
    match file_watcher {
        FileWatcher::NoFileWatcher => "Dummy",
        FileWatcher::DFind => "DFind",
        FileWatcher::Watchman(_) => "Watchman",
        FileWatcher::EdenFS(_) => "EdenFS",
    }
}

pub fn cli_arg_of_file_watcher(file_watcher: &FileWatcher) -> &'static str {
    match file_watcher {
        FileWatcher::NoFileWatcher => "none",
        FileWatcher::DFind => "dfind",
        FileWatcher::Watchman(_) => "watchman",
        FileWatcher::EdenFS(_) => "edenfs",
    }
}
