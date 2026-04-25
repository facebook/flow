/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Condvar;
use std::sync::Mutex;

use flow_common_exit_status::FlowExitStatus;
use tokio::sync::Notify;

pub struct ExitCondition {
    value: Mutex<Option<(FlowExitStatus, String)>>,
    condvar: Condvar,
    // Async-side analogue of `condvar`. Awaiters of `notified()` register via
    // `Notified::enable` before checking `value`, so a concurrent `broadcast` can't
    // be missed. Awaiting on a `Notified` future is cancellable by dropping it,
    // which is what makes the async waiter abortable from a `tokio::select!`.
    notify: Notify,
}

impl ExitCondition {
    const fn new() -> Self {
        ExitCondition {
            value: Mutex::new(None),
            condvar: Condvar::new(),
            notify: Notify::const_new(),
        }
    }

    pub fn broadcast(&self, exit_status: FlowExitStatus, msg: String) {
        let mut value = self.value.lock().unwrap();
        *value = Some((exit_status, msg));
        self.condvar.notify_all();
        self.notify.notify_waiters();
    }

    pub fn wait(&self) -> (FlowExitStatus, String) {
        let mut value = self.value.lock().unwrap();
        loop {
            if let Some(v) = value.clone() {
                return v;
            }
            value = self.condvar.wait(value).unwrap();
        }
    }

    // Async analogue of `wait`. The OCaml original is `Lwt_condition.wait ExitSignal.signal`,
    // a cancellable suspension point. Dropping the returned future (e.g. via
    // `tokio::select!` losing a race) actually cancels the wait, matching `Lwt.pick`
    // semantics. `Notified::enable` registers the waiter before we sample `value`, which
    // closes the subscribe-vs-broadcast race.
    pub async fn notified(&self) -> (FlowExitStatus, String) {
        let mut notified = std::pin::pin!(self.notify.notified());
        notified.as_mut().enable();
        if let Some(v) = self.value.lock().unwrap().clone() {
            return v;
        }
        notified.await;
        self.value
            .lock()
            .unwrap()
            .clone()
            .expect("ExitCondition::notified woke without value being set")
    }
}

// A well known condition variable. When we need to exit, we will broadcast on it. This allows
// various bits of code to wait for this signal and execute clean up code when it's received.
pub static SIGNAL: ExitCondition = ExitCondition::new();
