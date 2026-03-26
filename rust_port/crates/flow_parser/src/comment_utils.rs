/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;

use crate::ast;
use crate::ast_visitor::AstVisitor;
use crate::loc::Loc;

/// returns all of the comments that start before `loc`, and discards the rest
fn comments_before_loc(loc: &Loc, comments: &[ast::Comment<Loc>]) -> Arc<[ast::Comment<Loc>]> {
    comments
        .iter()
        .take_while(|comment| comment.loc < *loc)
        .cloned()
        .collect::<Vec<_>>()
        .into()
}

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

fn strip_comments_list(program: &mut ast::Program<Loc, Loc>, preserve_docblock: bool) {
    program.all_comments = if preserve_docblock {
        comments_before_loc(&program.loc, &program.all_comments)
    } else {
        Arc::from([])
    };
}

pub fn strip_all_comments(program: &mut ast::Program<Loc, Loc>, preserve_docblock: bool) {
    strip_comments_list(program, preserve_docblock);
    strip_inlined_comments(program);
}

pub fn strip_inlined_comments_expression(expression: &mut ast::expression::Expression<Loc, Loc>) {
    *expression = InlineCommentsStripper.map_expression(expression);
}
