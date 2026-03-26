/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_typing_statement::switch_to_match;

use crate::contains_mapper::ContainsMapper;

struct Mapper {
    contains: ContainsMapper,
    found: bool,
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
        if self.found {
            return stmt.dupe();
        }
        match &**stmt {
            StatementInner::Switch { loc, inner: switch }
                if self.contains.target_contained_by(loc) =>
            {
                match switch_to_match::convert_switch(&LOC_NONE, loc.dupe(), switch) {
                    Some(stmt_inner) if stmt_inner != **stmt => {
                        self.found = true;
                        ast::statement::Statement::new(stmt_inner)
                    }
                    _ => stmt.dupe(),
                }
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

pub fn refactor(ast: &ast::Program<Loc, Loc>, loc: Loc) -> Option<ast::Program<Loc, Loc>> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        found: false,
    };
    let ast_prime = mapper.map_program(ast);
    if !mapper.found {
        // No change
        None
    } else {
        Some(ast_prime)
    }
}
