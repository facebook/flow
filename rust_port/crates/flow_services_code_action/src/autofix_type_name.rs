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
use flow_parser::ast::types::TypeInner;
use flow_parser::ast::types::generic;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::ast_visitor::map_type_default;
use flow_parser::loc::Loc;
use flow_typing_errors::intermediate_error_types::IncorrectType;

use crate::contains_mapper::ContainsMapper;

struct Mapper {
    contains: ContainsMapper,
    incorrect_name: FlowSmolStr,
    replacement_name: FlowSmolStr,
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
            TypeInner::Mixed { loc, comments }
                if self.incorrect_name.as_str() == "mixed" && self.contains.is_target(loc) =>
            {
                ast::types::Type::new(TypeInner::Unknown {
                    loc: loc.dupe(),
                    comments: comments.clone(),
                })
            }
            TypeInner::Generic { loc, inner: gt } => {
                if !self.contains.is_target(loc) {
                    map_type_default(self, t)
                } else {
                    match &gt.id {
                        generic::Identifier::Unqualified(id) if id.name == self.incorrect_name => {
                            let new_id = generic::Identifier::Unqualified(ast::Identifier::new(
                                IdentifierInner {
                                    loc: id.loc.dupe(),
                                    name: self.replacement_name.dupe(),
                                    comments: id.comments.clone(),
                                },
                            ));
                            ast::types::Type::new(TypeInner::Generic {
                                loc: loc.dupe(),
                                inner: Arc::new(ast::types::Generic {
                                    id: new_id,
                                    targs: gt.targs.clone(),
                                    comments: gt.comments.clone(),
                                }),
                            })
                        }
                        _ => map_type_default(self, t),
                    }
                }
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

pub fn convert_type(
    incorrect_name: FlowSmolStr,
    replacement_name: FlowSmolStr,
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        incorrect_name,
        replacement_name,
    };
    mapper.map_program(ast)
}

pub fn convert_incorrect_type(
    kind: IncorrectType,
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let incorrect_name = FlowSmolStr::new(kind.incorrect_of_kind());
    let replacement_name = FlowSmolStr::new(kind.replacement_of_kind());
    convert_type(incorrect_name, replacement_name, ast, loc)
}
