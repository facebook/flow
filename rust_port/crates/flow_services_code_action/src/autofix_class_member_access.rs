/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/code_action/autofix_class_member_access.ml`

use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::reason::Name;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::ast_visitor::map_expression_default;
use flow_parser::ast_visitor::map_program_default;
use flow_parser::ast_visitor::map_statement_default;
use flow_parser::loc::Loc;
use flow_parser::polymorphic_ast_mapper;
use flow_parser::polymorphic_ast_mapper::LocMapper;
use flow_parser_utils::ast_builder::expressions;
use flow_parser_utils::file_sig::FileSig;
use flow_typing::ty_members;
use flow_typing_context::Context;
use flow_typing_type::type_::Type;

use crate::contains_mapper::ContainsMapper;

// The Found "exception" type: holds the result of finding the enclosing class.
// None means we found the target but it was not inside any class.
// Some(Type) means we found the target inside a class with the given type.
struct Found(Option<Type>);

struct EnclosingClassFinder<'a> {
    loc_of_aloc: &'a dyn Fn(&ALoc) -> Loc,
    target: Loc,
    enclosing_classes: Vec<Type>,
}

impl<'a> EnclosingClassFinder<'a> {
    fn with_enclosing_class_t<R>(&mut self, class_t: Type, f: impl FnOnce(&mut Self) -> R) -> R {
        self.enclosing_classes.push(class_t);
        let result = f(self);
        self.enclosing_classes.pop();
        result
    }
}

impl<'a> LocMapper<ALoc, (ALoc, Type), ALoc, (ALoc, Type), Found> for EnclosingClassFinder<'a> {
    fn on_loc_annot(&mut self, loc: &ALoc) -> Result<ALoc, Found> {
        if (self.loc_of_aloc)(loc) == self.target {
            Err(Found(self.enclosing_classes.last().map(|t| t.dupe())))
        } else {
            Ok(loc.dupe())
        }
    }

    fn on_type_annot(&mut self, x: &(ALoc, Type)) -> Result<(ALoc, Type), Found> {
        if (self.loc_of_aloc)(&x.0) == self.target {
            Err(Found(self.enclosing_classes.last().map(|t| t.dupe())))
        } else {
            Ok(x.dupe())
        }
    }
}

fn enclosing_class_finder_statement(
    finder: &mut EnclosingClassFinder<'_>,
    stmt: &ast::statement::Statement<ALoc, (ALoc, Type)>,
) -> Result<ast::statement::Statement<ALoc, (ALoc, Type)>, Found> {
    match &**stmt {
        StatementInner::ClassDeclaration { inner, .. } if let Some(ref id) = inner.id => {
            let t = id.loc.1.dupe();
            finder
                .with_enclosing_class_t(t, |finder| polymorphic_ast_mapper::statement(finder, stmt))
        }
        _ => polymorphic_ast_mapper::statement(finder, stmt),
    }
}

#[allow(dead_code)]
fn enclosing_class_finder_expression(
    finder: &mut EnclosingClassFinder<'_>,
    expr: &ast::expression::Expression<ALoc, (ALoc, Type)>,
) -> Result<ast::expression::Expression<ALoc, (ALoc, Type)>, Found> {
    match &**expr {
        ExpressionInner::Class { loc, .. } => {
            let class_t = loc.1.dupe();
            finder.with_enclosing_class_t(class_t, |finder| {
                polymorphic_ast_mapper::expression(finder, expr)
            })
        }
        _ => polymorphic_ast_mapper::expression(finder, expr),
    }
}

fn enclosing_class_finder_program(
    finder: &mut EnclosingClassFinder<'_>,
    program: &ast::Program<ALoc, (ALoc, Type)>,
) -> Result<(), Found> {
    finder.on_loc_annot(&program.loc)?;
    for stmt in program.statements.iter() {
        enclosing_class_finder_statement(finder, stmt)?;
    }
    Ok(())
}

fn find_enclosing_class(
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    target: Loc,
) -> Option<Type> {
    let mut finder = EnclosingClassFinder {
        loc_of_aloc,
        target,
        enclosing_classes: Vec::new(),
    };
    match enclosing_class_finder_program(&mut finder, typed_ast) {
        Ok(()) => None,
        Err(Found(t)) => t,
    }
}

struct PrefixWithThisMapper {
    contains: ContainsMapper,
}

impl AstVisitor<'_, Loc> for PrefixWithThisMapper {
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
            ExpressionInner::Identifier { inner, .. } if self.contains.is_target(&inner.loc) => {
                let this_expr = expressions::this(None, None);
                let member = expressions::members::identifier(None, inner.dupe(), this_expr);
                expressions::member(None, member)
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

fn prefix_with_this(ast: &ast::Program<Loc, Loc>, target: Loc) -> ast::Program<Loc, Loc> {
    let mut mapper = PrefixWithThisMapper {
        contains: ContainsMapper::new(target),
    };
    mapper.map_program(ast)
}

fn is_member(
    cx: &Context,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    file_sig: Arc<FileSig>,
    type_: &Type,
    name: &str,
) -> bool {
    match ty_members::extract(
        true,
        Some(vec![Name::new(name)]),
        None,
        cx,
        Some(typed_ast),
        file_sig,
        type_,
    ) {
        Err(_) => false,
        Ok(ty_members::TyMembers { members, .. }) => members.keys().any(|n| n.as_str() == name),
    }
}

pub fn fix(
    cx: &Context,
    file_sig: Arc<FileSig>,
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    ast: &ast::Program<Loc, Loc>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    member_name: &str,
    target: Loc,
) -> Option<ast::Program<Loc, Loc>> {
    let enclosing_class_t = find_enclosing_class(loc_of_aloc, typed_ast, target.dupe())?;
    if is_member(cx, typed_ast, file_sig, &enclosing_class_t, member_name) {
        let ast = prefix_with_this(ast, target);
        Some(ast)
    } else {
        None
    }
}
