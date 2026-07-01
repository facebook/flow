/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The daemon's `flow_clock` state -- a per-process instance id and a monotonic counter -- behind
//! `flow_monitor_rpc::flow_clock::FlowClock`.

use std::sync::OnceLock;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use flow_monitor_rpc::flow_clock::FlowClock;
use uuid::Uuid;

// A fresh v4 `instance_id` per process makes a restarted server's clocks distinguishable.
static INSTANCE_ID: OnceLock<Uuid> = OnceLock::new();
static COUNTER: AtomicU64 = AtomicU64::new(0);

pub fn current() -> FlowClock {
    FlowClock::new(instance_id(), COUNTER.load(Ordering::SeqCst))
}

pub fn increment() {
    COUNTER.fetch_add(1, Ordering::SeqCst);
}

fn instance_id() -> Uuid {
    *INSTANCE_ID.get_or_init(Uuid::new_v4)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counter_advances_within_a_stable_instance() {
        let before = current();
        increment();
        let after = current();
        assert_eq!(
            before.instance_id(),
            after.instance_id(),
            "instance is stable within a process"
        );
        assert!(
            after.counter() > before.counter(),
            "increment advances the counter"
        );
    }
}
