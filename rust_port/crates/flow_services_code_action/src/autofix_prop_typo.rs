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
use flow_parser::ast::expression::member;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::Loc;
use flow_parser_utils::ast_builder;

use crate::contains_mapper::ContainsMapper;

struct Mapper {
    contains: ContainsMapper,
    fixed_prop_name: FlowSmolStr,
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
            ExpressionInner::Member { loc, inner: m } => match &m.property {
                member::Property::PropertyIdentifier(id) if self.contains.is_target(&id.loc) => {
                    ast::expression::Expression::new(ExpressionInner::Member {
                        loc: loc.dupe(),
                        inner: Arc::new(ast::expression::Member {
                            object: m.object.dupe(),
                            property: member::Property::PropertyIdentifier(ast::Identifier::new(
                                IdentifierInner {
                                    loc: id.loc.dupe(),
                                    name: self.fixed_prop_name.dupe(),
                                    comments: id.comments.clone(),
                                },
                            )),
                            comments: m.comments.clone(),
                        }),
                    })
                }
                member::Property::PropertyExpression(prop_expr) => {
                    if let ExpressionInner::StringLiteral {
                        loc: property_literal_loc,
                        inner: string_lit,
                    } = &**prop_expr
                    {
                        if self.contains.is_target(property_literal_loc) {
                            let fixed_prop_name_literal = ast_builder::literals::string(
                                string_lit.comments.clone(),
                                self.fixed_prop_name.as_str(),
                            );
                            return ast::expression::Expression::new(ExpressionInner::Member {
                                loc: loc.dupe(),
                                inner: Arc::new(ast::expression::Member {
                                    object: m.object.dupe(),
                                    property: member::Property::PropertyExpression(
                                        ast::expression::Expression::new(
                                            ExpressionInner::StringLiteral {
                                                loc: property_literal_loc.dupe(),
                                                inner: Arc::new(fixed_prop_name_literal),
                                            },
                                        ),
                                    ),
                                    comments: m.comments.clone(),
                                }),
                            });
                        }
                    }
                    if self.contains.should_map_expression(expr) {
                        map_expression_default(self, expr)
                    } else {
                        expr.dupe()
                    }
                }
                _ => {
                    if self.contains.should_map_expression(expr) {
                        map_expression_default(self, expr)
                    } else {
                        expr.dupe()
                    }
                }
            },
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

pub fn replace_prop_typo_at_target(
    fixed_prop_name: FlowSmolStr,
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        fixed_prop_name,
    };
    mapper.map_program(ast)
}
