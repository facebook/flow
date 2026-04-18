/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Condvar;
use std::sync::Mutex;

use flow_common_exit_status::FlowExitStatus;

pub struct ExitCondition {
    value: Mutex<Option<(FlowExitStatus, String)>>,
    condvar: Condvar,
}

impl ExitCondition {
    const fn new() -> Self {
        ExitCondition {
            value: Mutex::new(None),
            condvar: Condvar::new(),
        }
    }

    pub fn broadcast(&self, exit_status: FlowExitStatus, msg: String) {
        let mut value = self.value.lock().unwrap();
        *value = Some((exit_status, msg));
        self.condvar.notify_all();
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
}

// A well known condition variable. When we need to exit, we will broadcast on it. This allows
// various bits of code to wait for this signal and execute clean up code when it's received.
pub static SIGNAL: ExitCondition = ExitCondition::new();
