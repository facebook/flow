/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::OnceLock;
use std::time::Duration;

use crate::ChannelPair;
use crate::Entry;
use crate::Handle;
use crate::StdioFd;
use crate::check_entry_point;
use crate::from_channel;
use crate::kill;
use crate::register_entry_point;
use crate::spawn;
use crate::to_channel;

// Entry: echo one received value back.
fn echo_entry(_p: (), pair: ChannelPair<String, String>) {
    let ChannelPair(mut in_chan, mut out_chan) = pair;
    let value: String = from_channel(&mut in_chan, None);
    to_channel(&mut out_chan, &value, true);
}

// Entry: receive a string param, send it back as a single message (no input
// expected). Tests that the param marshaling round-trips faithfully.
fn echo_param_entry(p: String, pair: ChannelPair<(), String>) {
    let ChannelPair(_in_chan, mut out_chan) = pair;
    to_channel(&mut out_chan, &p, true);
}

// Entry: sleep until killed. Tests that `kill` actually terminates the child.
fn sleep_entry(_p: (), _pair: ChannelPair<(), ()>) {
    std::thread::sleep(Duration::from_secs(60));
}

struct TestEntries {
    echo: Entry<(), String, String>,
    echo_param: Entry<String, (), String>,
    sleep: Entry<(), (), ()>,
}

static TEST_ENTRIES: OnceLock<TestEntries> = OnceLock::new();

#[ctor::ctor]
fn register_test_entries_and_dispatch() {
    let entries = TestEntries {
        echo: register_entry_point("__test_echo", echo_entry),
        echo_param: register_entry_point("__test_echo_param", echo_param_entry),
        sleep: register_entry_point("__test_sleep", sleep_entry),
    };
    if TEST_ENTRIES.set(entries).is_err() {
        panic!("TEST_ENTRIES already initialized -- ctor ran twice?");
    }
    // If HH_SERVER_DAEMON is set we are a daemon child; check_entry_point
    // dispatches and exits (so libtest never gains control). Otherwise it
    // returns and libtest proceeds normally.
    check_entry_point();
}

fn entries() -> &'static TestEntries {
    TEST_ENTRIES
        .get()
        .expect("TEST_ENTRIES not initialized (ctor failed?)")
}

#[test]
fn spawn_returns_distinct_pid() {
    let entries = entries();
    let mut h: Handle<String, String> = spawn(
        None,
        Some("test_spawn_pid"),
        (StdioFd::Inherit, StdioFd::Inherit, StdioFd::Inherit),
        &entries.echo,
        (),
    )
    .expect("spawn failed");
    assert_ne!(
        h.child.id(),
        std::process::id(),
        "child PID must differ from parent PID -- this is the regression \
         test for the previous in-thread `daemonize` bug where the monitor \
         was about to SIGKILL itself"
    );
    // Drive the echo so the child completes and exits cleanly.
    to_channel(&mut h.channels.1, &"ping".to_string(), true);
    let echoed: String = from_channel(&mut h.channels.0, Some(Duration::from_secs(5)));
    assert_eq!(echoed, "ping");
    kill(h).expect("kill failed");
}

#[test]
fn roundtrip_bidirectional() {
    let entries = entries();
    let mut h: Handle<String, String> = spawn(
        None,
        Some("test_roundtrip"),
        (StdioFd::Inherit, StdioFd::Inherit, StdioFd::Inherit),
        &entries.echo,
        (),
    )
    .expect("spawn failed");
    to_channel(&mut h.channels.1, &"hello world".to_string(), true);
    let received: String = from_channel(&mut h.channels.0, Some(Duration::from_secs(5)));
    assert_eq!(
        received, "hello world",
        "value did not round-trip the parent->child->parent path"
    );
    kill(h).expect("kill failed");
}

#[test]
fn param_serialization_round_trip() {
    let entries = entries();
    let payload = "secret-payload-12345".to_string();
    let mut h: Handle<String, ()> = spawn(
        None,
        Some("test_param_round_trip"),
        (StdioFd::Inherit, StdioFd::Inherit, StdioFd::Inherit),
        &entries.echo_param,
        payload.clone(),
    )
    .expect("spawn failed");
    let received: String = from_channel(&mut h.channels.0, Some(Duration::from_secs(5)));
    assert_eq!(
        received, payload,
        "param did not round-trip via the tempfile + bincode path"
    );
    kill(h).expect("kill failed");
}

#[test]
fn kill_child_does_not_kill_parent() {
    let entries = entries();
    let h: Handle<(), ()> = spawn(
        None,
        Some("test_kill_does_not_kill_parent"),
        (StdioFd::Inherit, StdioFd::Inherit, StdioFd::Inherit),
        &entries.sleep,
        (),
    )
    .expect("spawn failed");
    let pid = h.child.id();
    assert_ne!(pid, std::process::id());
    kill(h).expect("kill failed");
    // Implicit assertion: the test process survived `kill`. If `kill` had
    // delivered the signal to the parent (the bug we are guarding against),
    // this line would not be reached and the test would not be reported as
    // passing.
}
