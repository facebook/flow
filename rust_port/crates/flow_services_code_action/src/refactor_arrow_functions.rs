/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_analysis::scope_api::ScopeInfo;
use flow_common::reason;
use flow_parser::ast;
use flow_parser::ast::function;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_arrow_function_default;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::Loc;
use flow_parser_utils::ast_builder;

use crate::contains_mapper::ContainsMapper;

struct Mapper {
    contains: ContainsMapper,
    scope_loc: Loc,
    title: Option<String>,
}

impl AstVisitor<'_, Loc> for Mapper {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn map_arrow_function(
        &mut self,
        loc: &Loc,
        func: &ast::function::Function<Loc, Loc>,
    ) -> ast::function::Function<Loc, Loc> {
        match &func.body {
            function::Body::BodyBlock((body_loc, block)) if *body_loc == self.scope_loc => {
                if block.body.len() == 1 {
                    if let StatementInner::Return { loc: _, inner: ret } = &*block.body[0] {
                        if let Some(return_expr) = &ret.argument {
                            self.title = Some("Remove braces from arrow function".to_string());
                            return ast::function::Function {
                                body: ast_builder::functions::body_expression(return_expr.dupe()),
                                ..func.clone()
                            };
                        }
                    }
                }
                map_arrow_function_default(self, loc, func)
            }
            function::Body::BodyExpression(body_expr) if *body_expr.loc() == self.scope_loc => {
                self.title = Some("Add braces to arrow function".to_string());
                ast::function::Function {
                    body: ast_builder::functions::body(
                        None,
                        None,
                        vec![ast_builder::statements::return_(
                            None,
                            None,
                            Some(body_expr.dupe()),
                        )],
                    ),
                    ..func.clone()
                }
            }
            _ => map_arrow_function_default(self, loc, func),
        }
    }

    fn map_program(&mut self, program: &ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
        if self.contains.should_map_program(program) {
            map_program_default(self, program)
        } else {
            program.clone()
        }
    }

    fn map_statement(
        &mut self,
        stmt: &ast::statement::Statement<Loc, Loc>,
    ) -> ast::statement::Statement<Loc, Loc> {
        if self.contains.should_map_statement(stmt) {
            map_statement_default(self, stmt)
        } else {
            stmt.dupe()
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

pub fn add_or_remove_braces(
    ast: &ast::Program<Loc, Loc>,
    scope_info: &ScopeInfo<Loc>,
    loc: &Loc,
) -> Option<(ast::Program<Loc, Loc>, String)> {
    let scope_id = scope_info.closest_enclosing_scope(loc, reason::in_range);
    let enclosing_scope_loc = scope_info.scope(scope_id).loc.dupe();
    let mut mapper = Mapper {
        contains: ContainsMapper::new(enclosing_scope_loc.dupe()),
        scope_loc: enclosing_scope_loc,
        title: None,
    };
    let ast_prime = mapper.map_program(ast);
    mapper.title.map(|title| (ast_prime, title))
}
