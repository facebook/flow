/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This class maps each node that contains the target until a node is contained
// by the target

use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::types::TypeInner;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::ast_visitor::map_type_default;
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

    fn map_type_(&mut self, t: &ast::types::Type<Loc, Loc>) -> ast::types::Type<Loc, Loc> {
        match &**t {
            TypeInner::Object { loc, inner: ot } if self.contains.is_target(loc) => {
                // interfaces behave like inexact object, but they cannot have the explicit `...` syntax
                ast::types::Type::new(TypeInner::Interface {
                    loc: loc.dupe(),
                    inner: Arc::new(ast::types::Interface {
                        body: (
                            loc.dupe(),
                            ast::types::Object {
                                inexact: false,
                                exact: ot.exact,
                                properties: ot.properties.dupe(),
                                comments: ot.comments.clone(),
                            },
                        ),
                        extends: Arc::from([]),
                        comments: None,
                    }),
                })
            }
            _ => map_type_default(self, t),
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

pub fn replace_object_at_target(ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
    };
    mapper.map_program(ast)
}
