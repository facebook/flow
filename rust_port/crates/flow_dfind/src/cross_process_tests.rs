/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Cross-process regression tests for the dfind daemon protocol.
//!
//! These spawn the real dfind daemon (a re-exec of this test binary) and drive
//! it through the same `init` / `wait_until_ready` / `get_changes` path the
//! server monitor uses. The headline test guards against the busy-spin bug:
//! when the poll request was encoded as the unit type `()` (zero bincode
//! bytes), the daemon's `from_channel::<()>` never blocked, so its main loop
//! spun draining the empty accumulator and flooded the response channel with
//! empty `Updates`. A real file change then sat buried behind that backlog and
//! was never reported, leaving `flow check` with stale errors indefinitely.

use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::OnceLock;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

use flow_daemon::check_entry_point;

use crate::DaemonFds;
use crate::Dfind;
use crate::InitArgs;
use crate::dfind_server;

// The daemon re-execs this same test binary with `HH_SERVER_DAEMON` set. The
// child must have the "dfind" entry point registered before `check_entry_point`
// dispatches to it, so we register it (and dispatch) from a ctor that runs
// before libtest gains control. In a non-daemon process `check_entry_point`
// returns immediately and the tests run normally.
#[ctor::ctor(unsafe)]
fn register_dfind_entry_and_dispatch() {
    dfind_server::register();
    check_entry_point();
}

fn runtime() -> &'static tokio::runtime::Runtime {
    static RT: OnceLock<tokio::runtime::Runtime> = OnceLock::new();
    RT.get_or_init(|| {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to build tokio runtime for dfind tests")
    })
}

/// A throwaway directory under the system temp dir, removed on drop so the test
/// cleans up even if it panics. Names are unique per process + call so parallel
/// tests never collide.
struct ScratchDir {
    path: PathBuf,
}

impl ScratchDir {
    fn new(tag: &str) -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!(
            "flow-dfind-test-{}-{}-{}",
            std::process::id(),
            tag,
            n
        ));
        std::fs::create_dir_all(&path).expect("create scratch dir");
        // Canonicalize so the absolute paths the watcher reports line up with
        // what we compare against (e.g. /tmp may be a symlink to /private/tmp).
        let path = path.canonicalize().expect("canonicalize scratch dir");
        Self { path }
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for ScratchDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.path);
    }
}

fn start_daemon(root: &Path, log_file: PathBuf) -> Dfind {
    crate::init(
        DaemonFds { log_file },
        InitArgs {
            scuba_table: "flow_test".to_string(),
            roots: vec![root.to_path_buf()],
        },
    )
    .expect("dfind init failed")
}

/// Poll `get_changes` until a reported path's file name matches `file_name`, or
/// `timeout` elapses. Returns the last observed change set either way so the
/// caller can produce a useful assertion message.
async fn poll_until_change(dfind: &Dfind, file_name: &str, timeout: Duration) -> BTreeSet<String> {
    let deadline = Instant::now() + timeout;
    loop {
        let changes = crate::get_changes(dfind).await.expect("get_changes failed");
        if changes.iter().any(|p| p.ends_with(file_name)) {
            return changes;
        }
        if Instant::now() >= deadline {
            return changes;
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
}

#[test]
fn file_changes_are_reported_promptly() {
    let root = ScratchDir::new("root");
    // Keep the daemon log outside the watched root so writes to it don't
    // register as spurious file changes.
    let log_dir = ScratchDir::new("log");
    let log_file = log_dir.path().join("dfind.log");

    let dfind = start_daemon(root.path(), log_file);

    runtime().block_on(async {
        crate::wait_until_ready(&dfind).await;

        // Nothing has changed yet: the root is a fresh empty directory and
        // watching is active by the time `Ready` arrives, so the first poll
        // must be empty.
        let initial = crate::get_changes(&dfind)
            .await
            .expect("get_changes failed");
        assert!(
            initial.is_empty(),
            "expected no changes before any file edit, got {:?}",
            initial
        );

        // Create a file under the watched root; the watcher must report it.
        let watched = root.path().join("watched_file.js");
        std::fs::write(&watched, "// initial\n").expect("write watched file");
        let changes = poll_until_change(&dfind, "watched_file.js", Duration::from_secs(10)).await;
        assert!(
            changes.iter().any(|p| p.ends_with("watched_file.js")),
            "dfind did not report the created file within the timeout. This is \
             the regression for the busy-spin bug where the daemon flooded the \
             response channel with empty updates and buried real changes. \
             Got: {:?}",
            changes
        );

        // A second edit must also be reported -- the daemon keeps blocking on
        // real poll requests rather than spinning and draining nothing.
        std::fs::write(&watched, "// edited\n").expect("rewrite watched file");
        let changes = poll_until_change(&dfind, "watched_file.js", Duration::from_secs(10)).await;
        assert!(
            changes.iter().any(|p| p.ends_with("watched_file.js")),
            "dfind did not report the second edit within the timeout. Got: {:?}",
            changes
        );
    });

    crate::stop(&dfind);
}
