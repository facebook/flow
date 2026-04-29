/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This crate is the OSS-facing facade for `FlowInteractionLogger`. In fbcode
// builds the public API is provided by
// `flow_facebook_logging::flow_interaction_logger`. ShipIt strips
// `crates/facebook/` for the OSS publish, so only the
// `cfg(not(fbcode_build))` branch falls back to the in-file stub whose
// functions are no-ops.

#[cfg(fbcode_build)]
pub use flow_facebook_logging::flow_interaction_logger::disable_logging;
#[cfg(fbcode_build)]
pub use flow_facebook_logging::flow_interaction_logger::flush;
#[cfg(fbcode_build)]
pub use flow_facebook_logging::flow_interaction_logger::init;
#[cfg(fbcode_build)]
pub use flow_facebook_logging::flow_interaction_logger::interaction;
#[cfg(fbcode_build)]
pub use flow_facebook_logging::flow_interaction_logger::set_server_config;

#[cfg(not(fbcode_build))]
mod stub {
    use std::path::PathBuf;

    use flow_common::options::LogSaving;

    pub fn init() {}

    pub fn set_server_config(
        _tls: Option<LogSaving>,
        _flowconfig_name: &str,
        _root: PathBuf,
        _root_name: Option<&str>,
    ) {
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "OCaml signature pins each labeled argument; preserve them 1:1"
    )]
    pub fn interaction(
        _lsp_id: Option<&str>,
        _is_timeout_ux: bool,
        _source: &str,
        _uri: Option<&str>,
        _trigger: &str,
        _ux: &str,
        _start_time_ms: i64,
        _end_time_ms: i64,
        _start_server_status: &str,
        _end_server_status: &str,
        _start_buffer_status: &str,
        _end_buffer_status: &str,
    ) {
    }

    pub async fn flush() {}

    pub fn disable_logging() {}
}

#[cfg(not(fbcode_build))]
pub use stub::disable_logging;
#[cfg(not(fbcode_build))]
pub use stub::flush;
#[cfg(not(fbcode_build))]
pub use stub::init;
#[cfg(not(fbcode_build))]
pub use stub::interaction;
#[cfg(not(fbcode_build))]
pub use stub::set_server_config;
