/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

struct Mapper<'a, F> {
    contains: ContainsMapper,
    f: &'a F,
}

impl<'a, F> AstVisitor<'_, Loc> for Mapper<'a, F>
where
    F: Fn(&TypeInner<Loc, Loc>) -> TypeInner<Loc, Loc>,
{
    fn normalize_loc(loc: &Loc) -> &Loc {
        loc
    }

    fn normalize_type(type_: &Loc) -> &Loc {
        type_
    }

    fn map_type_(&mut self, t: &ast::types::Type<Loc, Loc>) -> ast::types::Type<Loc, Loc> {
        let loc = t.loc();
        if self.contains.is_target(loc) {
            ast::types::Type::new((self.f)(t))
        } else {
            map_type_default(self, t)
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

pub fn replace_type<F>(f: &F, ast: &ast::Program<Loc, Loc>, loc: Loc) -> ast::Program<Loc, Loc>
where
    F: Fn(&TypeInner<Loc, Loc>) -> TypeInner<Loc, Loc>,
{
    let mut mapper = Mapper {
        contains: ContainsMapper::new(loc),
        f,
    };
    mapper.map_program(ast)
}
