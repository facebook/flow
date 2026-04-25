/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

use flow_daemon::Handle;
use flow_daemon::StdioFd;
use flow_daemon::from_channel;
use flow_daemon::kill;
use flow_daemon::null_fd;
use flow_daemon::spawn;
use flow_daemon::to_channel;

pub mod dfind_server;

use crate::dfind_server::Msg;
use crate::dfind_server::Param;

// ---------------------------------------------------------------------------
// Public surface (must remain backwards-compatible with the existing callers
// in `flow_server_monitor/src/file_watcher.rs`).
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub enum Error {
    Notify(notify::Error),
    Stopped,
    Io(std::io::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Notify(e) => write!(f, "dfind notify error: {}", e),
            Error::Stopped => write!(f, "dfind watcher has been stopped"),
            Error::Io(e) => write!(f, "dfind io error: {}", e),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Notify(e) => Some(e),
            Error::Stopped => None,
            Error::Io(e) => Some(e),
        }
    }
}

impl From<notify::Error> for Error {
    fn from(e: notify::Error) -> Self {
        Error::Notify(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

#[derive(Debug, Clone)]
pub struct DaemonFds {
    pub log_file: PathBuf,
}

#[derive(Debug, Clone)]
pub struct InitArgs {
    pub scuba_table: String,
    pub roots: Vec<PathBuf>,
}

pub struct Dfind {
    inner: Arc<Mutex<Option<Handle<Msg, ()>>>>,
}

pub fn init(fds: DaemonFds, args: InitArgs) -> Result<Dfind, Error> {
    let DaemonFds { log_file } = fds;
    let InitArgs { scuba_table, roots } = args;
    let entry = dfind_server::entry_point();
    let pretty_pid = std::process::id();
    let name = format!("file watching process for server {}", pretty_pid);

    let stdin_fd = StdioFd::Owned(null_fd());
    let stdout_fd = StdioFd::Owned(open_append(&log_file)?);
    let stderr_fd = StdioFd::Owned(open_append(&log_file)?);

    let param = Param {
        scuba_table,
        roots,
        log_file,
    };
    let handle = spawn(
        None,
        Some(&name),
        (stdin_fd, stdout_fd, stderr_fd),
        entry,
        param,
    )?;
    Ok(Dfind {
        inner: Arc::new(Mutex::new(Some(handle))),
    })
}

fn open_append(path: &PathBuf) -> Result<std::fs::File, Error> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    Ok(std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)?)
}

pub fn pid(d: &Dfind) -> Option<u32> {
    let g = match d.inner.lock() {
        Ok(g) => g,
        Err(p) => p.into_inner(),
    };
    g.as_ref().map(|h| h.child.id())
}

pub async fn wait_until_ready(d: &Dfind) {
    let inner = Arc::clone(&d.inner);
    let result: Result<(), String> = tokio::task::spawn_blocking(move || {
        let mut g = inner.lock().expect("dfind handle mutex poisoned");
        let h = g.as_mut().ok_or_else(|| "dfind: stopped".to_string())?;
        let msg: Msg = from_channel(&mut h.channels.0, None);
        if msg != Msg::Ready {
            return Err(format!("dfind: expected Ready, got {:?}", msg));
        }
        Ok(())
    })
    .await
    .unwrap_or_else(|e| Err(format!("dfind wait_until_ready task panicked: {}", e)));
    if let Err(e) = result {
        tracing::warn!(target: "flow_dfind", "wait_until_ready failed: {}", e);
    }
}

pub async fn get_changes(d: &Dfind) -> Result<BTreeSet<String>, Error> {
    let inner = Arc::clone(&d.inner);
    tokio::task::spawn_blocking(move || -> Result<BTreeSet<String>, Error> {
        let mut acc: BTreeSet<String> = BTreeSet::new();
        loop {
            let mut g = inner.lock().expect("dfind handle mutex poisoned");
            let h = g.as_mut().ok_or(Error::Stopped)?;
            to_channel(&mut h.channels.1, &(), true);
            let msg: Msg = from_channel(&mut h.channels.0, None);
            drop(g);
            let diff = match msg {
                Msg::Updates(s) => s,
                Msg::Ready => {
                    return Err(Error::Notify(notify::Error::generic(
                        "dfind: unexpected Ready msg in get_changes loop",
                    )));
                }
            };
            if diff.is_empty() {
                return Ok(acc);
            }
            acc.extend(diff);
        }
    })
    .await
    .unwrap_or_else(|e| {
        Err(Error::Notify(notify::Error::generic(&format!(
            "dfind get_changes task panicked: {}",
            e
        ))))
    })
}

pub fn stop(d: &Dfind) {
    let mut g = match d.inner.lock() {
        Ok(g) => g,
        Err(p) => p.into_inner(),
    };
    if let Some(handle) = g.take() {
        if let Err(e) = kill(handle) {
            tracing::warn!(target: "flow_dfind", "dfind stop: kill failed: {}", e);
        }
    }
}
