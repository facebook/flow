/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[cfg(unix)]
pub fn string_of_signal(n: i32) -> String {
    if n == libc::SIGABRT {
        "sigabrt".to_string()
    } else if n == libc::SIGALRM {
        "sigalrm".to_string()
    } else if n == libc::SIGFPE {
        "sigfpe".to_string()
    } else if n == libc::SIGHUP {
        "sighup".to_string()
    } else if n == libc::SIGILL {
        "sigill".to_string()
    } else if n == libc::SIGINT {
        "sigint".to_string()
    } else if n == libc::SIGKILL {
        "sigkill".to_string()
    } else if n == libc::SIGPIPE {
        "sigpipe".to_string()
    } else if n == libc::SIGQUIT {
        "sigquit".to_string()
    } else if n == libc::SIGSEGV {
        "sigsegv".to_string()
    } else if n == libc::SIGTERM {
        "sigterm".to_string()
    } else if n == libc::SIGUSR1 {
        "sigusr1".to_string()
    } else if n == libc::SIGUSR2 {
        "sigusr2".to_string()
    } else if n == libc::SIGCHLD {
        "sigchld".to_string()
    } else if n == libc::SIGCONT {
        "sigcont".to_string()
    } else if n == libc::SIGSTOP {
        "sigstop".to_string()
    } else if n == libc::SIGTSTP {
        "sigtstp".to_string()
    } else if n == libc::SIGTTIN {
        "sigttin".to_string()
    } else if n == libc::SIGTTOU {
        "sigttou".to_string()
    } else if n == libc::SIGVTALRM {
        "sigvtalrm".to_string()
    } else if n == libc::SIGPROF {
        "sigprof".to_string()
    } else if n == libc::SIGBUS {
        "sigbus".to_string()
    } else if n == libc::SIGIO {
        "sigpoll".to_string()
    } else if n == libc::SIGSYS {
        "sigsys".to_string()
    } else if n == libc::SIGTRAP {
        "sigtrap".to_string()
    } else if n == libc::SIGURG {
        "sigurg".to_string()
    } else if n == libc::SIGXCPU {
        "sigxcpu".to_string()
    } else if n == libc::SIGXFSZ {
        "sigxfsz".to_string()
    } else {
        format!("unknown signal {}", n)
    }
}

#[cfg(windows)]
pub fn string_of_signal(n: i32) -> String {
    if n == libc::SIGABRT {
        "sigabrt".to_string()
    } else if n == libc::SIGFPE {
        "sigfpe".to_string()
    } else if n == libc::SIGILL {
        "sigill".to_string()
    } else if n == libc::SIGINT {
        "sigint".to_string()
    } else if n == libc::SIGSEGV {
        "sigsegv".to_string()
    } else if n == libc::SIGTERM {
        "sigterm".to_string()
    } else {
        format!("unknown signal {}", n)
    }
}
