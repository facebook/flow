/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// OSS-facing facade for `EventLoggerLwt`. In fbcode builds the public API
// comes from `flow_facebook_logging::event_logger_lwt`, which mirrors the
// FB-internal `hack_forked/facebook/logging/eventLoggerLwt.ml`. In OSS
// builds (where `crates/facebook/` is stripped by ShipIt) the
// `cfg(not(fbcode_build))` branch falls back to the in-file stub that
// matches `flow/src/stubs/eventLoggerLwt.ml`

#[cfg(fbcode_build)]
pub use flow_facebook_logging::event_logger_flusher::FlushingError;
#[cfg(fbcode_build)]
pub use flow_facebook_logging::event_logger_lwt::flush;

#[cfg(not(fbcode_build))]
mod stub {
    #[derive(Debug)]
    pub enum FlushingError {}

    impl std::fmt::Display for FlushingError {
        fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match *self {}
        }
    }

    impl std::error::Error for FlushingError {}

    pub async fn flush() -> Result<(), FlushingError> {
        Ok(())
    }
}

#[cfg(not(fbcode_build))]
pub use stub::FlushingError;
#[cfg(not(fbcode_build))]
pub use stub::flush;
