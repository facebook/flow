/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::sync::mpsc;

use notify::Event;
use notify::RecommendedWatcher;
use notify::RecursiveMode;
use notify::Watcher as _;
use regex::Regex;

fn excludes() -> &'static [Regex] {
    static EXCLUDES: OnceLock<Vec<Regex>> = OnceLock::new();
    EXCLUDES.get_or_init(|| {
        [r".*/wiki/images/.*", r".*/\.git", r".*/\.svn", r".*/\.hg"]
            .iter()
            .map(|p| Regex::new(p).expect("dfind exclusion regex compiles"))
            .collect()
    })
}

fn is_excluded(path: &str) -> bool {
    excludes().iter().any(|re| re.is_match(path))
}

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

struct Shared {
    files: BTreeSet<String>,
    stopped: bool,
}

pub struct Dfind {
    watcher: Mutex<Option<RecommendedWatcher>>,
    shared: Arc<Mutex<Shared>>,
    log_file: Arc<Mutex<File>>,
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

pub fn init(fds: DaemonFds, args: InitArgs) -> Result<Dfind, Error> {
    let DaemonFds { log_file } = fds;
    let mut log_file_handle = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log_file)?;

    let InitArgs { scuba_table, roots } = args;

    writeln!(
        log_file_handle,
        "starting dfind watcher (label={:?}, roots={:?})",
        scuba_table, roots
    )?;
    log_file_handle.flush()?;

    tracing::info!(
        target: "flow_dfind",
        "starting dfind watcher (label={:?}, roots={:?})",
        scuba_table,
        roots
    );

    let shared = Arc::new(Mutex::new(Shared {
        files: BTreeSet::new(),
        stopped: false,
    }));
    let log_file_arc = Arc::new(Mutex::new(log_file_handle));

    let (tx, rx) = mpsc::channel::<Result<Event, notify::Error>>();
    let mut watcher: RecommendedWatcher = notify::recommended_watcher(tx)?;
    for root in &roots {
        if let Err(err) = watcher.watch(root, RecursiveMode::Recursive) {
            tracing::warn!(
                target: "flow_dfind",
                "failed to watch root {:?}: {}",
                root,
                err
            );
            write_log(
                &log_file_arc,
                format_args!("failed to watch root {:?}: {}", root, err),
            );
        }
    }

    let event_shared = shared.clone();
    let event_log = log_file_arc.clone();
    std::thread::Builder::new()
        .name("flow-dfind-events".to_string())
        .spawn(move || event_loop(rx, event_shared, event_log))
        .map_err(|err| {
            Error::Notify(notify::Error::generic(&format!(
                "failed to spawn dfind event thread: {}",
                err
            )))
        })?;

    Ok(Dfind {
        watcher: Mutex::new(Some(watcher)),
        shared,
        log_file: log_file_arc,
    })
}

fn write_log(log_file: &Arc<Mutex<File>>, args: std::fmt::Arguments<'_>) {
    let mut guard = match log_file.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
    };
    match writeln!(*guard, "{}", args) {
        Ok(()) => {}
        Err(err) => tracing::warn!(
            target: "flow_dfind",
            "failed to write to dfind log file: {}",
            err
        ),
    }
}

fn event_loop(
    rx: mpsc::Receiver<Result<Event, notify::Error>>,
    shared: Arc<Mutex<Shared>>,
    log_file: Arc<Mutex<File>>,
) {
    while let Ok(event_result) = rx.recv() {
        match event_result {
            Ok(event) => record_event(&shared, event),
            Err(err) => {
                tracing::warn!(
                    target: "flow_dfind",
                    "dfind notify error: {}",
                    err
                );
                write_log(&log_file, format_args!("dfind notify error: {}", err));
            }
        }
    }
}

fn record_event(shared: &Arc<Mutex<Shared>>, event: Event) {
    if matches!(
        event.kind,
        notify::EventKind::Access(_) | notify::EventKind::Other
    ) {
        return;
    }
    let mut guard = match shared.lock() {
        Ok(guard) => guard,
        Err(poisoned) => {
            tracing::error!(
                target: "flow_dfind",
                "dfind shared state poisoned; recovering"
            );
            poisoned.into_inner()
        }
    };
    if guard.stopped {
        return;
    }
    for path in event.paths {
        let path_str = path.to_string_lossy().into_owned();
        if is_excluded(&path_str) {
            continue;
        }
        guard.files.insert(path_str);
    }
}

pub async fn wait_until_ready(_d: &Dfind) {}

pub async fn get_changes(d: &Dfind) -> Result<BTreeSet<String>, Error> {
    let mut guard = match d.shared.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
    };
    if guard.stopped {
        return Err(Error::Stopped);
    }
    Ok(std::mem::take(&mut guard.files))
}

pub fn stop(d: &Dfind) {
    {
        let mut guard = match d.shared.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        };
        guard.stopped = true;
    }
    let mut watcher_guard = match d.watcher.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
    };
    drop(watcher_guard.take());
    write_log(&d.log_file, format_args!("dfind watcher stopped"));
    tracing::info!(target: "flow_dfind", "dfind watcher stopped");
}
