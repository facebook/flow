/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Clone, Copy, Default, serde::Serialize, serde::Deserialize)]
pub struct SlowToCheckLogging {
    pub slow_files_logging_internal: Option<f64>,
    pub slow_components_logging_threshold: Option<f64>,
    pub slow_expressions_logging_threshold: Option<f64>,
}
