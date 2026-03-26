/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::ast_visitor::map_type_default;
use flow_parser::loc::Loc;
use flow_parser_utils::ast_builder;

use crate::contains_mapper::ContainsMapper;

#[derive(PartialEq, Eq)]
enum Kind {
    ColonCast,
    SatisfiesExpression,
}

struct Mapper {
    contains: ContainsMapper,
    kind: Kind,
}

impl Mapper {
    fn build_cast(
        &mut self,
        comments: &Option<ast::Syntax<Loc, ()>>,
        expression: &ast::expression::Expression<Loc, Loc>,
        annot: &ast::types::Type<Loc, Loc>,
    ) -> ast::expression::Expression<Loc, Loc> {
        let expression = map_expression_default(self, expression);
        let annot = map_type_default(self, annot);
        ast_builder::expressions::as_expression(None, comments.clone(), expression, annot)
    }
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
            ExpressionInner::TypeCast { loc: _, inner }
                if self.kind == Kind::ColonCast && self.contains.is_target(expr.loc()) =>
            {
                let annot = &inner.annot.annotation;
                self.build_cast(&inner.comments, &inner.expression, annot)
            }
            ExpressionInner::TSSatisfies { loc: _, inner }
                if self.kind == Kind::SatisfiesExpression
                    && self.contains.is_target(expr.loc()) =>
            {
                let annot = &inner.annot.annotation;
                self.build_cast(&inner.comments, &inner.expression, annot)
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

pub fn convert_satisfies_expression(
    ast: &ast::Program<Loc, Loc>,
    loc: Loc,
) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::SatisfiesExpression,
    };
    mapper.map_program(ast)
}

pub fn convert_colon_cast(ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc> {
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        kind: Kind::ColonCast,
    };
    mapper.map_program(ast)
}

struct AllMapper {
    kind: Kind,
}

impl AllMapper {
    fn build_cast(
        &mut self,
        comments: &Option<ast::Syntax<Loc, ()>>,
        expression: &ast::expression::Expression<Loc, Loc>,
        annot: &ast::types::Type<Loc, Loc>,
    ) -> ast::expression::Expression<Loc, Loc> {
        let expression = map_expression_default(self, expression);
        let annot = map_type_default(self, annot);
        ast_builder::expressions::as_expression(None, comments.clone(), expression, annot)
    }
}

impl AstVisitor<'_, Loc> for AllMapper {
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
            ExpressionInner::TypeCast { loc: _, inner } if self.kind == Kind::ColonCast => {
                let annot = &inner.annot.annotation;
                self.build_cast(&inner.comments, &inner.expression, annot)
            }
            ExpressionInner::TSSatisfies { loc: _, inner }
                if self.kind == Kind::SatisfiesExpression =>
            {
                let annot = &inner.annot.annotation;
                self.build_cast(&inner.comments, &inner.expression, annot)
            }
            _ => map_expression_default(self, expr),
        }
    }
}

pub fn convert_all_colon_casts(ast: &ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
    let mut mapper = AllMapper {
        kind: Kind::ColonCast,
    };
    mapper.map_program(ast)
}

pub fn convert_all_satisfies_expressions(ast: &ast::Program<Loc, Loc>) -> ast::Program<Loc, Loc> {
    let mut mapper = AllMapper {
        kind: Kind::SatisfiesExpression,
    };
    mapper.map_program(ast)
}
