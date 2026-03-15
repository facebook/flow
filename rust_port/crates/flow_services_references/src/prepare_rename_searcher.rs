/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;

struct Searcher<F: Fn(&Loc) -> bool> {
    contains_loc: F,
}

impl<'ast, F: Fn(&Loc) -> bool> AstVisitor<'ast, Loc, Loc, &'ast Loc, Loc> for Searcher<F> {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn identifier(&mut self, id: &'ast ast::Identifier<Loc, Loc>) -> Result<(), Loc> {
        let loc = &id.loc;
        if (self.contains_loc)(loc) {
            return Err(loc.dupe());
        }
        ast_visitor::identifier_default(self, id)
    }

    fn private_name(&mut self, id: &'ast ast::PrivateName<Loc>) -> Result<(), Loc> {
        let loc = &id.loc;
        if (self.contains_loc)(loc) {
            return Err(loc.dupe());
        }
        ast_visitor::private_name_default(self, id)
    }
}

pub fn search_rename_loc(ast: &ast::Program<Loc, Loc>, cursor_loc: &Loc) -> Option<Loc> {
    let mut s = Searcher {
        contains_loc: |loc: &Loc| loc.contains(cursor_loc),
    };
    s.program(ast).err()
}
