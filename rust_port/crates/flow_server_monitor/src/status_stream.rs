/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This module is a singleton that is responsible for keeping track of the Flow server's status.
// The basic idea is that we have a stream of status updates coming from the Flow server. When
// The Flow server dies and is replaced with a new server, we create a new stream.
//
// The main goal of this module is to answer the question "What is the server's current status" and
// to invoke callbacks when the server becomes free

use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::time::Duration;

use flow_server_env::file_watcher_status;
use flow_server_env::server_status;
use tokio::sync::mpsc;

use crate::FlowServerMonitorOptions;
use crate::flow_server_monitor_options::FileWatcher;
use crate::runtime;

// This is the status info for a single Flow server
struct StatusInfo {
    status: server_status::Status,
    watcher_status: file_watcher_status::Status,
    ever_been_free: bool,
    restart_reason: Option<server_status::RestartReason>,
    stream: Mutex<Option<mpsc::UnboundedReceiver<server_status::Status>>>,
    push_to_stream: mpsc::UnboundedSender<server_status::Status>,
}

// Multiple threads might call StreamStatus functions.
static MUTEX: Mutex<()> = Mutex::new(());

// A list of callbacks which will be invoked once the next time the server is free.
static TO_CALL_ON_FREE: Mutex<Vec<Box<dyn FnOnce() + Send>>> = Mutex::new(Vec::new());

static SIGNIFICANT_TRANSITION: (
    Mutex<Option<(server_status::Status, file_watcher_status::Status)>>,
    Condvar,
) = (Mutex::new(None), Condvar::new());

fn check_if_free(t: &mut StatusInfo) {
    fn invoke_all_call_on_free() {
        let to_call = {
            let _lock = MUTEX.lock().unwrap();
            let mut to_call_on_free = TO_CALL_ON_FREE.lock().unwrap();
            std::mem::take(&mut *to_call_on_free)
        };
        for f in to_call {
            f();
        }
    }

    if server_status::is_free(&t.status)
        && t.watcher_status.1 == file_watcher_status::StatusKind::Ready
    {
        t.ever_been_free = true;
        runtime::handle().spawn(async {
            invoke_all_call_on_free();
        });
    }
}

fn broadcast_significant_transition(t: &StatusInfo) {
    let (lock, cvar) = &SIGNIFICANT_TRANSITION;
    let mut data = lock.lock().unwrap();
    *data = Some((t.status.clone(), t.watcher_status.clone()));
    cvar.notify_all();
}

mod update_loop {
    use super::*;

    pub(super) fn process_update(t: &mut StatusInfo, new_status: server_status::Status) {
        let new_status = server_status::change_init_to_restart(t.restart_reason, new_status);
        log::debug!(
            "Server status: {}",
            server_status::string_of_status(false, false, &new_status)
        );

        let old_status = t.status.clone();
        // We don't need a lock here, since we're the only thread processing statuses.
        t.status = new_status.clone();

        check_if_free(t);

        if server_status::is_significant_transition(&old_status, &new_status) {
            broadcast_significant_transition(t);
        }
    }

    pub(super) async fn main(
        t: Arc<Mutex<StatusInfo>>,
        rx: &mut mpsc::UnboundedReceiver<server_status::Status>,
    ) -> Result<(), StreamEmpty> {
        let new_status = rx.recv().await.ok_or(StreamEmpty)?;
        let mut t = t.lock().unwrap();
        process_update(&mut t, new_status);
        Ok(())
    }

    pub(super) fn catch(_t: &Arc<Mutex<StatusInfo>>, exn: LoopError) {
        match exn {
            LoopError::StreamEmpty => {}
            LoopError::Other(e) => {
                log::error!(
                    "ServerStatus update loop hit an unexpected exception: {}",
                    e
                );
            }
        }
    }

    pub(super) struct StreamEmpty;

    pub(super) enum LoopError {
        StreamEmpty,
        #[allow(dead_code)]
        Other(String),
    }

    impl From<StreamEmpty> for LoopError {
        fn from(_: StreamEmpty) -> Self {
            LoopError::StreamEmpty
        }
    }

    pub(super) async fn run(
        t: Arc<Mutex<StatusInfo>>,
        mut rx: mpsc::UnboundedReceiver<server_status::Status>,
    ) {
        loop {
            tokio::task::yield_now().await;
            match main(t.clone(), &mut rx).await {
                Ok(()) => continue,
                Err(e) => {
                    catch(&t, LoopError::from(e));
                    return;
                }
            }
        }
    }
}

fn file_watcher_for_status(fw: &FileWatcher) -> file_watcher_status::FileWatcher {
    match fw {
        FlowServerMonitorOptions::NoFileWatcher => file_watcher_status::FileWatcher::NoFileWatcher,
        FlowServerMonitorOptions::Watchman(_) => file_watcher_status::FileWatcher::Watchman,
        FlowServerMonitorOptions::EdenFS(_) => file_watcher_status::FileWatcher::EdenFS,
    }
}

fn empty(
    file_watcher: file_watcher_status::FileWatcher,
    restart_reason: Option<server_status::RestartReason>,
) -> Arc<Mutex<StatusInfo>> {
    let (push_to_stream, stream) = mpsc::unbounded_channel();
    let ret = Arc::new(Mutex::new(StatusInfo {
        status: server_status::INITIAL_STATUS,
        watcher_status: (file_watcher, file_watcher_status::StatusKind::Initializing),
        ever_been_free: false,
        restart_reason,
        stream: Mutex::new(Some(stream)),
        push_to_stream,
    }));
    let ret_for_loop = ret.clone();
    let stream = ret
        .lock()
        .unwrap()
        .stream
        .lock()
        .unwrap()
        .take()
        .expect("stream receiver missing immediately after creation");
    runtime::handle().spawn(async move {
        update_loop::run(ret_for_loop, stream).await;
    });
    ret
}

// This is the status info for the current Flow server.
static CURRENT_STATUS: std::sync::LazyLock<Mutex<Arc<Mutex<StatusInfo>>>> =
    std::sync::LazyLock::new(|| {
        Mutex::new(empty(file_watcher_status::FileWatcher::NoFileWatcher, None))
    });

// Call f the next time the server is free. If the server is currently free, then call now.
pub fn call_on_free(f: Box<dyn FnOnce() + Send>) {
    let is_free = {
        let current = CURRENT_STATUS.lock().unwrap();
        let info = current.lock().unwrap();
        server_status::is_free(&info.status)
    };
    if is_free {
        f();
    } else {
        let _lock = MUTEX.lock().unwrap();
        let mut to_call_on_free = TO_CALL_ON_FREE.lock().unwrap();
        to_call_on_free.push(f);
    }
}

pub fn file_watcher_ready() {
    let current = CURRENT_STATUS.lock().unwrap();
    let mut t = current.lock().unwrap();
    t.watcher_status.1 = file_watcher_status::StatusKind::Ready;
    check_if_free(&mut t);
    broadcast_significant_transition(&t);
}

pub fn file_watcher_deferred(reason: String) {
    let current = CURRENT_STATUS.lock().unwrap();
    let mut t = current.lock().unwrap();
    t.watcher_status.1 = file_watcher_status::StatusKind::Deferred { reason };
    broadcast_significant_transition(&t);
}

// When a new server starts up, we close the old server's status stream and start over.
pub fn reset(file_watcher: &FileWatcher, restart_reason: Option<server_status::RestartReason>) {
    let file_watcher = file_watcher_for_status(file_watcher);
    let _lock = MUTEX.lock().unwrap();
    let mut current_status = CURRENT_STATUS.lock().unwrap();
    *current_status = empty(file_watcher, restart_reason);
}

pub fn get_status() -> (server_status::Status, file_watcher_status::Status) {
    let current = CURRENT_STATUS.lock().unwrap();
    let info = current.lock().unwrap();
    let StatusInfo {
        ref status,
        ref watcher_status,
        ..
    } = *info;
    (status.clone(), watcher_status.clone())
}

pub fn ever_been_free() -> bool {
    CURRENT_STATUS
        .lock()
        .unwrap()
        .lock()
        .unwrap()
        .ever_been_free
}

// If there is a significant transition before the timeout, the cancel the sleep and return the
// new status. Otherwise, stop waiting on the condition variable and return the current status.
pub fn wait_for_signficant_status(
    timeout: f64,
) -> (server_status::Status, file_watcher_status::Status) {
    let (lock, cvar) = &SIGNIFICANT_TRANSITION;
    let guard = lock.lock().unwrap();
    let timeout_duration = Duration::from_secs_f64(timeout);
    let result = cvar.wait_timeout(guard, timeout_duration).unwrap();
    match &*result.0 {
        Some((status, watcher_status)) if !result.1.timed_out() => {
            (status.clone(), watcher_status.clone())
        }
        _ => get_status(),
    }
}

// Updates will show up on the connection in order. Let's push them immediately to a stream to
// preserve that order.
pub fn update(status: server_status::Status) {
    let current = CURRENT_STATUS.lock().unwrap();
    let info = current.lock().unwrap();
    if let Err(e) = info.push_to_stream.send(status) {
        log::debug!("StatusStream.update: stream closed, dropping update: {}", e);
    }
}
