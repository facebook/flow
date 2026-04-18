/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum FileWatcher {
    NoFileWatcher,
    Watchman,
    EdenFS,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum StatusKind {
    Initializing,
    Ready,
    Deferred { reason: String },
}

pub type Status = (FileWatcher, StatusKind);

pub fn string_of_file_watcher(fw: &FileWatcher) -> &'static str {
    match fw {
        FileWatcher::NoFileWatcher => "Dummy",
        FileWatcher::Watchman => "Watchman",
        FileWatcher::EdenFS => "EdenFS",
    }
}

pub fn string_of_status((watcher, status): &Status) -> String {
    fn string_of_status_kind(status: &StatusKind) -> String {
        match status {
            StatusKind::Initializing => "still initializing".to_string(),
            StatusKind::Ready => "ready".to_string(),
            StatusKind::Deferred { reason } => format!("deferred ({})", reason),
        }
    }
    format!(
        "{} file watcher is {}",
        string_of_file_watcher(watcher),
        string_of_status_kind(status)
    )
}
