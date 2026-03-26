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
use flow_parser::ast::expression::OptionalMember;
use flow_parser::ast::expression::OptionalMemberKind;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::Loc;

use crate::contains_mapper::ContainsMapper;

struct Mapper {
    contains: ContainsMapper,
}

impl AstVisitor<'_, Loc> for Mapper {
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn map_expression(
        &mut self,
        expr: &ast::expression::Expression<Loc, Loc>,
    ) -> ast::expression::Expression<Loc, Loc> {
        match &**expr {
            ExpressionInner::Member { loc, inner } if self.contains.is_target(loc) => {
                ast::expression::Expression::new(ExpressionInner::OptionalMember {
                    loc: loc.dupe(),
                    inner: Arc::new(OptionalMember {
                        member: (**inner).clone(),
                        optional: OptionalMemberKind::Optional,
                        filtered_out: loc.dupe(),
                    }),
                })
            }
            _ => {
                if self.contains.should_map_expression(expr) {
                    map_expression_default(self, expr)
                } else {
                    expr.dupe()
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
}

pub fn add_optional_chaining(ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
    };
    mapper.map_program(ast)
}
