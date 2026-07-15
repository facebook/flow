/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io;

use serde::Deserialize;

#[derive(Clone, Debug, Deserialize)]
pub(crate) struct FlowResult {
    pub(crate) passed: bool,
    pub(crate) errors: Vec<FlowError>,
}

#[derive(Clone, Debug, Deserialize)]
pub(crate) struct FlowError {
    pub(crate) kind: String,
    pub(crate) error_codes: Vec<String>,
    pub(crate) level: String,
    pub(crate) message: Vec<FlowMessage>,
    pub(crate) operation: Option<FlowMessage>,
}

#[derive(Clone, Debug, Deserialize)]
pub(crate) struct FlowMessage {
    pub(crate) descr: String,
    pub(crate) loc: Option<FlowLoc>,
}

#[derive(Clone, Debug, Deserialize)]
pub(crate) struct FlowLoc {
    pub(crate) source: Option<String>,
    #[serde(rename = "type")]
    pub(crate) source_type: Option<String>,
    pub(crate) start: FlowPos,
    pub(crate) end: FlowPos,
}

#[derive(Clone, Debug, Deserialize)]
pub(crate) struct FlowPos {
    pub(crate) line: i64,
    pub(crate) column: i64,
    pub(crate) offset: Option<usize>,
}

pub(crate) fn required_offset(pos: &FlowPos, name: &str) -> io::Result<usize> {
    pos.offset
        .ok_or_else(|| io::Error::other(format!("missing offset for {}", name)))
}
