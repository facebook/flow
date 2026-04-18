/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Verbose {
    pub indent: u32,
    pub depth: u32,
    pub enabled_during_flowlib: bool,
    pub focused_files: Option<Vec<String>>,
}
