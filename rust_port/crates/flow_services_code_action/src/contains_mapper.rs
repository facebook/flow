/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::ast;
use flow_parser::loc::Loc;

pub struct ContainsMapper {
    target: Loc,
}

impl ContainsMapper {
    pub fn new(target: Loc) -> Self {
        Self { target }
    }

    pub fn target_ref(&self) -> &Loc {
        &self.target
    }

    pub fn target_contains(&self, loc: &Loc) -> bool {
        self.target.contains(loc)
    }

    pub fn target_contained_by(&self, loc: &Loc) -> bool {
        loc.contains(&self.target)
    }

    pub fn is_target(&self, loc: &Loc) -> bool {
        self.target == *loc
    }

    pub fn should_map_program(&self, program: &ast::Program<Loc, Loc>) -> bool {
        self.target_contained_by(&program.loc)
            || program
                .all_comments
                .iter()
                .any(|c| self.target_contained_by(&c.loc))
    }

    pub fn should_map_statement(&self, stmt: &ast::statement::Statement<Loc, Loc>) -> bool {
        self.target_contained_by(stmt.loc())
    }

    pub fn should_map_comment(&self, comment: &ast::Comment<Loc>) -> bool {
        self.target_contained_by(&comment.loc)
    }

    pub fn should_map_expression(&self, expr: &ast::expression::Expression<Loc, Loc>) -> bool {
        self.target_contained_by(expr.loc())
    }
}
