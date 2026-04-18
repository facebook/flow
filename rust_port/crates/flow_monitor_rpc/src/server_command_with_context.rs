/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::lsp_prot::LoggingContext;
use crate::server_prot::request;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct ServerCommandWithContext {
    pub client_logging_context: LoggingContext,
    pub command: request::Command,
}
