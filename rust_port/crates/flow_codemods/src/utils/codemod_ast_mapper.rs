/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;

pub type Name = FlowSmolStr;

pub struct CodemodAstMapper<Acc> {
    pub acc: Acc,
    name: Name,
}

impl<Acc> CodemodAstMapper<Acc> {
    pub fn new(name: Name, init: Acc) -> Self {
        Self { acc: init, name }
    }

    pub fn log(&self, s: &str) {
        flow_hh_logger::info!("{{{}}}: {}", self.name, s);
    }
}

impl<'ast, Acc> AstVisitor<'ast, Loc> for CodemodAstMapper<Acc> {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }
}
