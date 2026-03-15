/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast::expression::Expression;
use flow_parser::loc::Loc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlowMode {
    OptIn,
    OptInStrict,
    OptInStrictLocal,
    OptOut,
}

#[derive(Debug, Clone)]
pub struct JsxPragma {
    pub raw: String,
    pub expression: Expression<Loc, Loc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsxRuntimePragma {
    Classic,
    Automatic,
}

#[derive(Debug, Clone, Default)]
pub struct Docblock {
    pub flow: Option<FlowMode>,
    pub prevent_munge: bool,
    pub jsx: Option<Arc<JsxPragma>>,
    pub jsx_runtime: Option<JsxRuntimePragma>,
    pub supports_platform: Option<Vec<FlowSmolStr>>,
}

impl Docblock {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn flow(&self) -> Option<FlowMode> {
        self.flow
    }

    pub fn prevent_munge(&self) -> bool {
        self.prevent_munge
    }

    pub fn jsx(&self) -> Option<&JsxPragma> {
        self.jsx.as_deref()
    }

    pub fn jsx_runtime(&self) -> Option<JsxRuntimePragma> {
        self.jsx_runtime
    }

    pub fn is_strict(&self) -> bool {
        matches!(self.flow, Some(FlowMode::OptInStrict))
    }

    pub fn is_flow(&self) -> bool {
        matches!(
            self.flow,
            Some(FlowMode::OptIn | FlowMode::OptInStrict | FlowMode::OptInStrictLocal)
        )
    }

    pub fn supports_platform(&self) -> Option<&[FlowSmolStr]> {
        self.supports_platform.as_deref()
    }
}
