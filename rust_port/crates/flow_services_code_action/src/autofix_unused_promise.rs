/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::expression::Unary;
use flow_parser::ast::expression::UnaryOperator;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::Loc;

use crate::contains_mapper::ContainsMapper;

struct Mapper {
    contains: ContainsMapper,
    operator: UnaryOperator,
}

impl AstVisitor<'_, Loc> for Mapper {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn map_statement(
        &mut self,
        stmt: &ast::statement::Statement<Loc, Loc>,
    ) -> ast::statement::Statement<Loc, Loc> {
        match &**stmt {
            StatementInner::Expression { loc, inner } if self.contains.is_target(loc) => {
                ast::statement::Statement::new(StatementInner::Expression {
                    loc: loc.dupe(),
                    inner: Arc::new(ast::statement::Expression {
                        expression: ast::expression::Expression::new(ExpressionInner::Unary {
                            loc: loc.dupe(),
                            inner: Arc::new(Unary {
                                operator: self.operator,
                                argument: inner.expression.dupe(),
                                comments: None,
                            }),
                        }),
                        directive: inner.directive.clone(),
                        comments: inner.comments.clone(),
                    }),
                })
            }
            _ => {
                if self.contains.should_map_statement(stmt) {
                    map_statement_default(self, stmt)
                } else {
                    stmt.dupe()
                }
            }
        }
    }

    fn map_program(&mut self, program: &ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
        if self.contains.should_map_program(program) {
            map_program_default(self, program)
        } else {
            program.clone()
        }
    }

    fn map_expression(
        &mut self,
        expr: &ast::expression::Expression<Loc, Loc>,
    ) -> ast::expression::Expression<Loc, Loc> {
        if self.contains.should_map_expression(expr) {
            map_expression_default(self, expr)
        } else {
            expr.dupe()
        }
    }
}

fn insert(
    operator: UnaryOperator,
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        operator,
    };
    mapper.map_program(ast)
}

pub fn insert_await(ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc> {
    insert(UnaryOperator::Await, ast, loc)
}

pub fn insert_void(ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc> {
    insert(UnaryOperator::Void, ast, loc)
}
