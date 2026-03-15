/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::ast;
use crate::ast_visitor::AstVisitor;
use crate::loc::Loc;

struct InlineCommentsStripper;

impl<'ast> AstVisitor<'ast, Loc> for InlineCommentsStripper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn map_syntax_opt<Internal: Dupe>(
        &mut self,
        _syntax_opt: Option<&'ast ast::Syntax<Loc, Internal>>,
    ) -> Option<ast::Syntax<Loc, Internal>>
    where
        Loc: Dupe,
    {
        None
    }
}

pub(super) fn strip_inlined_comments(program: &mut ast::Program<Loc, Loc>) {
    *program = InlineCommentsStripper.map_program(program);
}

#[expect(dead_code)]
pub(super) fn strip_inlined_comments_expression(
    expression: &mut ast::expression::Expression<Loc, Loc>,
) {
    *expression = InlineCommentsStripper.map_expression(expression);
}
