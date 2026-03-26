/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::IdentifierInner;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::expression::Record;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;

use crate::contains_mapper::ContainsMapper;

struct Mapper {
    contains: ContainsMapper,
    record_name: FlowSmolStr,
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
        let loc = expr.loc();
        if self.contains.is_target(loc) {
            match &**expr {
                ExpressionInner::Object { loc, inner: obj } => {
                    let constructor =
                        ast::expression::Expression::new(ExpressionInner::Identifier {
                            loc: LOC_NONE,
                            inner: ast::Identifier::new(IdentifierInner {
                                loc: LOC_NONE,
                                name: self.record_name.dupe(),
                                comments: None,
                            }),
                        });
                    ast::expression::Expression::new(ExpressionInner::Record {
                        loc: loc.dupe(),
                        inner: Arc::new(Record {
                            constructor,
                            targs: None,
                            properties: (loc.dupe(), (**obj).clone()),
                            comments: None,
                        }),
                    })
                }
                _ => map_expression_default(self, expr),
            }
        } else {
            if self.contains.should_map_expression(expr) {
                map_expression_default(self, expr)
            } else {
                expr.dupe()
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

pub fn convert_object_to_record_expression(
    record_name: FlowSmolStr,
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        record_name,
    };
    mapper.map_program(ast)
}
