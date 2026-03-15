/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `typed_ast_finder.ml`
//!
//! Provides utilities for finding types at specific positions in typed ASTs.
//! This includes exact match queries, type-at-pos queries, and type parameter
//! management during AST traversal.

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_parser::ast;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_statement::statement;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeParam;

pub fn mk_bound_t(cx: &Context, tparam: &TypeParam) -> Type {
    flow_js_utils::generic_of_tparam(cx, |x: &Type| x.dupe(), tparam)
}

#[derive(Clone)]
pub enum EnclosingNode<M: Dupe, T: Dupe> {
    EnclosingProgram(ast::Program<M, T>),
    EnclosingStatement(ast::statement::Statement<M, T>),
    EnclosingExpression(ast::expression::Expression<M, T>),
}

use flow_typing_utils::abnormal::AbnormalControlFlow;

pub fn infer_node(
    cx: &Context,
    node: EnclosingNode<ALoc, ALoc>,
) -> Result<EnclosingNode<ALoc, (ALoc, Type)>, AbnormalControlFlow> {
    match node {
        EnclosingNode::EnclosingProgram(prog) => {
            let ast::Program {
                loc: prog_aloc,
                statements,
                interpreter,
                comments,
                all_comments,
            } = prog;
            let statements = statement::statement_list(cx, &statements);
            Ok(EnclosingNode::EnclosingProgram(ast::Program {
                loc: prog_aloc,
                statements: statements.into(),
                interpreter,
                comments,
                all_comments,
            }))
        }
        EnclosingNode::EnclosingStatement(stmt) => Ok(EnclosingNode::EnclosingStatement(
            statement::statement(cx, &stmt)?,
        )),
        EnclosingNode::EnclosingExpression(expr) => Ok(EnclosingNode::EnclosingExpression(
            statement::expression(None, None, None, cx, &expr)?,
        )),
    }
}

struct FindTypeAnnotVisitor {
    target_loc: ALoc,
}

impl flow_parser::polymorphic_ast_mapper::LocMapper<ALoc, (ALoc, Type), ALoc, (ALoc, Type), Type>
    for FindTypeAnnotVisitor
{
    fn on_loc_annot(&mut self, loc: &ALoc) -> Result<ALoc, Type> {
        Ok(loc.dupe())
    }

    fn on_type_annot(&mut self, annot: &(ALoc, Type)) -> Result<(ALoc, Type), Type> {
        let (loc, t) = annot;
        if *loc == self.target_loc {
            Err(t.dupe())
        } else {
            Ok(annot.dupe())
        }
    }
}

pub fn find_type_annot_in_node(
    loc: ALoc,
    node: &EnclosingNode<ALoc, (ALoc, Type)>,
) -> Option<Type> {
    let mut visitor = FindTypeAnnotVisitor { target_loc: loc };
    use flow_parser::polymorphic_ast_mapper;
    let result = match node {
        EnclosingNode::EnclosingProgram(prog) => {
            polymorphic_ast_mapper::program(&mut visitor, prog).map(|_| ())
        }
        EnclosingNode::EnclosingStatement(stmt) => {
            polymorphic_ast_mapper::statement(&mut visitor, stmt).map(|_| ())
        }
        EnclosingNode::EnclosingExpression(expr) => {
            polymorphic_ast_mapper::expression(&mut visitor, expr).map(|_| ())
        }
    };
    result.err()
}

/// Find exact location match
pub mod exact_match_query {
    use flow_parser::polymorphic_ast_mapper;
    use flow_parser::polymorphic_ast_mapper::LocMapper;

    use super::*;

    struct ExactMatchSearcher {
        target_loc: ALoc,
    }

    impl LocMapper<ALoc, (ALoc, Type), ALoc, (ALoc, Type), Type> for ExactMatchSearcher {
        fn on_loc_annot(&mut self, loc: &ALoc) -> Result<ALoc, Type> {
            Ok(loc.dupe())
        }

        fn on_type_annot(&mut self, annot: &(ALoc, Type)) -> Result<(ALoc, Type), Type> {
            let (loc, t) = annot;
            if *loc == self.target_loc {
                Err(t.dupe())
            } else {
                Ok(annot.dupe())
            }
        }
    }

    pub fn find(typed_ast: &ast::Program<ALoc, (ALoc, Type)>, aloc: ALoc) -> Option<Type> {
        let mut searcher = ExactMatchSearcher { target_loc: aloc };
        polymorphic_ast_mapper::program(&mut searcher, typed_ast).err()
    }
}

pub fn find_exact_match_annotation(
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    aloc: ALoc,
) -> Option<Type> {
    exact_match_query::find(typed_ast, aloc)
}
