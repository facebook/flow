/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// OSS-facing facade for the lower-level `EventLogger` and `Scuba` primitives
// that Hack's `hack_forked/stubs/logging/common/{eventLogger,scuba}.ml` stubs
// expose. In fbcode builds the public API comes from
// `flow_facebook_logging::{event_logger,scuba}`, which is the real Rust
// port of `hack_forked/facebook/logging/{eventLogger,scuba}.ml`. ShipIt
// strips `crates/facebook/`, so only the `cfg(not(fbcode_build))` branch in
// this file falls back to the in-file stub.

#[cfg(fbcode_build)]
pub mod event_logger {
    pub use flow_facebook_logging::event_logger::*;
}

#[cfg(not(fbcode_build))]
pub mod event_logger {
    #[cfg(unix)]
    use std::os::fd::OwnedFd;
    #[cfg(windows)]
    use std::os::windows::io::OwnedHandle as OwnedFd;

    pub struct InitSettings {
        pub scuba_table_name: String,
        /// File descriptors for the logger daemon's stdout and stderr.
        pub log_out: OwnedFd,
        pub log_err: OwnedFd,
    }

    pub fn init(
        _log_pid: Option<bool>,
        _init_id: Option<String>,
        _init_settings: InitSettings,
        _init_time: f64,
    ) {
    }

    pub fn disable_logging() {}

    pub fn log(_sample: super::scuba::Sample) {}

    pub fn logger_pid() -> Option<i32> {
        None
    }

    pub fn should_log() -> bool {
        false
    }

    pub fn flush() {}
}

#[cfg(fbcode_build)]
pub mod scuba {
    pub use flow_facebook_logging::scuba::*;
}

#[cfg(not(fbcode_build))]
pub mod scuba {
    #[derive(Clone, Copy, Default)]
    pub struct Sample;

    pub mod table {
        #[derive(Clone, Copy, Default)]
        pub struct T;

        pub fn of_name(_table_name: &str) -> T {
            T
        }
    }

    pub fn new_sample(_table: Option<table::T>) -> Sample {
        Sample
    }

    pub fn add_int(_name: &str, _value: i64, _sample: Sample) -> Sample {
        Sample
    }

    pub fn add_normal(_name: &str, _value: &str, _sample: Sample) -> Sample {
        Sample
    }

    pub fn add_denorm(_name: &str, _value: &str, _sample: Sample) -> Sample {
        Sample
    }

    pub fn add_normvector(_name: &str, _value: &str, _sample: Sample) -> Sample {
        Sample
    }

    pub fn add_tags(_name: &str, _values: Vec<String>, _sample: Sample) -> Sample {
        Sample
    }
}
