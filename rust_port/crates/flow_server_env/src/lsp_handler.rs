/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub enum UnitLspResultHandler {
    ShowMessageHandler(Box<dyn FnOnce(Option<lsp_types::MessageActionItem>) + Send>),
    ShowStatusHandler(Box<dyn FnOnce(crate::lsp::show_status::Result) + Send>),
    ApplyWorkspaceEditHandler(Box<dyn FnOnce(lsp_types::ApplyWorkspaceEditResponse) + Send>),
    ConfigurationHandler(Box<dyn FnOnce(Vec<serde_json::Value>) + Send>),
    VoidHandler,
}

pub type UnitLspErrorHandler = Box<dyn FnOnce((crate::lsp::error::T, String)) + Send>;

pub type UnitLspHandler = (UnitLspResultHandler, UnitLspErrorHandler);
