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

use flow_daemon::ChannelPair;
use flow_daemon::Entry;
use flow_daemon::from_channel;
use flow_daemon::register_entry_point;
use flow_daemon::to_channel;
use notify::Event;
use notify::EventKind;
use notify::RecommendedWatcher;
use notify::RecursiveMode;
use notify::Watcher as _;
use regex::Regex;
use serde::Deserialize;
use serde::Serialize;

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

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Msg {
    Ready,
    Updates(BTreeSet<String>),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Param {
    pub scuba_table: String,
    pub roots: Vec<PathBuf>,
    pub log_file: PathBuf,
}

fn run_daemon(param: Param, pair: ChannelPair<(), Msg>) {
    let ChannelPair(mut in_chan, mut out_chan) = pair;
    let Param {
        scuba_table,
        roots,
        log_file,
    } = param;

    // Open a log file inside the daemon for diagnostics. OCaml routes its
    // output to the inherited stdout/stderr which the parent has redirected
    // to the same log file via `Daemon.spawn (null_fd, log_fd, log_fd)`.
    // For the Rust port we additionally open a `File` we own so we can write
    // structured progress lines without depending on the global tracing
    // subscriber being initialized in the child.
    let log = open_log_file(&log_file);
    write_log(
        &log,
        format_args!(
            "dfind daemon starting (scuba_table={:?}, roots={:?})",
            scuba_table, roots
        ),
    );

    let acc: Arc<Mutex<BTreeSet<String>>> = Arc::new(Mutex::new(BTreeSet::new()));
    let acc_for_watcher = Arc::clone(&acc);
    let log_for_watcher = Arc::clone(&log);
    let (tx, rx) = mpsc::channel::<Result<Event, notify::Error>>();
    let mut watcher: RecommendedWatcher = match notify::recommended_watcher(tx) {
        Ok(w) => w,
        Err(e) => {
            write_log(&log, format_args!("dfind: failed to create watcher: {}", e));
            std::process::exit(2);
        }
    };
    for root in &roots {
        if let Err(e) = watcher.watch(root, RecursiveMode::Recursive) {
            write_log(
                &log,
                format_args!("dfind: failed to watch {:?}: {}", root, e),
            );
        }
    }

    // Background thread: drain notify events into `acc`. Mirrors OCaml's
    // `fsnotify_callback` which folds events into `dirty`.
    if let Err(e) = std::thread::Builder::new()
        .name("flow-dfind-events".to_string())
        .spawn(move || event_loop(rx, acc_for_watcher, log_for_watcher))
    {
        write_log(
            &log,
            format_args!("dfind: failed to spawn event thread: {}", e),
        );
        std::process::exit(2);
    }

    write_log(&log, format_args!("dfind: ready"));

    to_channel(&mut out_chan, &Msg::Ready, true);

    loop {
        let _: () = from_channel(&mut in_chan, None);
        let updates = {
            let mut g = acc.lock().expect("dfind acc mutex poisoned");
            std::mem::take(&mut *g)
        };
        let count = updates.len();
        if count > 0 {
            write_log(&log, format_args!("dfind: sending {} file updates", count));
        }
        to_channel(&mut out_chan, &Msg::Updates(updates), true);
    }
}

fn open_log_file(path: &PathBuf) -> Arc<Mutex<File>> {
    if let Some(parent) = path.parent() {
        if let Err(e) = std::fs::create_dir_all(parent) {
            eprintln!("dfind: failed to create log dir {:?}: {}", parent, e);
        }
    }
    match OpenOptions::new().create(true).append(true).open(path) {
        Ok(file) => Arc::new(Mutex::new(file)),
        Err(e) => {
            eprintln!("dfind: failed to open log file {:?}: {}", path, e);
            std::process::exit(2);
        }
    }
}

fn write_log(log: &Arc<Mutex<File>>, args: std::fmt::Arguments<'_>) {
    let mut g = match log.lock() {
        Ok(g) => g,
        Err(p) => p.into_inner(),
    };
    if let Err(e) = writeln!(*g, "{}", args) {
        eprintln!("dfind log write error: {}", e);
    }
}

fn event_loop(
    rx: mpsc::Receiver<Result<Event, notify::Error>>,
    acc: Arc<Mutex<BTreeSet<String>>>,
    log: Arc<Mutex<File>>,
) {
    while let Ok(event_result) = rx.recv() {
        match event_result {
            Ok(event) => record_event(&acc, event),
            Err(err) => write_log(&log, format_args!("dfind notify error: {}", err)),
        }
    }
}

fn record_event(acc: &Arc<Mutex<BTreeSet<String>>>, event: Event) {
    if matches!(event.kind, EventKind::Access(_) | EventKind::Other) {
        return;
    }
    let mut g = match acc.lock() {
        Ok(g) => g,
        Err(p) => p.into_inner(),
    };
    for path in event.paths {
        let s = path.to_string_lossy().into_owned();
        if is_excluded(&s) {
            continue;
        }
        g.insert(s);
    }
}

pub fn entry_point() -> &'static Entry<Param, (), Msg> {
    static ENTRY: OnceLock<Entry<Param, (), Msg>> = OnceLock::new();
    ENTRY.get_or_init(|| register_entry_point("dfind", run_daemon))
}

// Convenience for callers that just want to ensure registration ran but
// don't need the handle.
pub fn register() {
    entry_point();
}
