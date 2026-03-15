/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_parser::ast;
use flow_parser::ast::expression;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast_utils;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_typing_context::Context;
use flow_typing_type::type_::Type;

use crate::refinement;

struct Marker<'cx> {
    cond: bool,
    predicate: bool,
    cx: &'cx Context,
}

impl<'cx> Marker<'cx> {
    fn new(cx: &'cx Context) -> Self {
        Marker {
            cond: false,
            predicate: false,
            cx,
        }
    }

    fn push_pred<R>(&mut self, v: bool, f: impl FnOnce(&mut Self) -> R) -> R {
        let init = self.predicate;
        self.predicate = v;
        let res = f(self);
        self.predicate = init;
        res
    }

    //     method base_expression c expr =
    fn base_expression<'ast>(
        &mut self,
        c: bool,
        expr: &'ast expression::Expression<ALoc, (ALoc, Type)>,
    ) where
        'cx: 'ast,
    {
        self.cond = c;
        if c {
            match expr.deref() {
                ExpressionInner::OptionalMember { loc, inner }
                    if matches!(
                        inner.member.property,
                        expression::member::Property::PropertyExpression(_)
                    ) =>
                {
                    let (loc, ty) = loc;
                    self.cx.add_exists_check(loc.dupe(), ty.dupe());
                }
                ExpressionInner::OptionalMember { inner, .. } => {
                    let (loc, ty) = &inner.filtered_out;
                    self.cx.add_exists_check(loc.dupe(), ty.dupe());
                }
                ExpressionInner::Member { loc, .. } => {
                    let (loc, ty) = loc;
                    self.cx.add_exists_check(loc.dupe(), ty.dupe());
                }
                ExpressionInner::Assignment { loc, inner } => {
                    let (_, ty) = loc;
                    let (unwrapped_left, _) = ast_utils::unwrap_nonnull_lhs(&inner.left);
                    match &*unwrapped_left {
                        ast::pattern::Pattern::Identifier { loc: pat_loc, .. } => {
                            let (pat_aloc, _) = pat_loc;
                            self.cx.add_exists_check(pat_aloc.dupe(), ty.dupe());
                        }
                        _ => {}
                    }
                }
                _ => {
                    if refinement::keys::key(false, expr).is_some() {
                        let (loc, ty) = expr.loc();
                        self.cx.add_exists_check(loc.dupe(), ty.dupe());
                    }
                }
            }
        }
        let Ok(()) = ast_visitor::expression_default(self, expr);
    }
}

impl<'ast, 'cx: 'ast> AstVisitor<'ast, ALoc, (ALoc, Type), &'ast ALoc, !> for Marker<'cx> {
    fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
        loc
    }

    fn normalize_type(_type: &'ast (ALoc, Type)) -> &'ast ALoc {
        &_type.0
    }

    fn expression(
        &mut self,
        expr: &'ast expression::Expression<ALoc, (ALoc, Type)>,
    ) -> Result<(), !> {
        self.base_expression(false, expr);
        Ok(())
    }

    fn predicate_expression(
        &mut self,
        expr: &'ast expression::Expression<ALoc, (ALoc, Type)>,
    ) -> Result<(), !> {
        self.base_expression(true, expr);
        Ok(())
    }

    fn predicate(
        &mut self,
        pred: &'ast ast::types::Predicate<ALoc, (ALoc, Type)>,
    ) -> Result<(), !> {
        let ast::types::Predicate {
            loc: _,
            kind,
            comments: _,
        } = pred;
        match kind {
            ast::types::PredicateKind::Inferred => {}
            ast::types::PredicateKind::Declared(expr) => {
                self.base_expression(self.cond, expr);
            }
        }
        Ok(())
    }

    fn return_(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::Return<ALoc, (ALoc, Type)>,
    ) -> Result<(), !> {
        let ast::statement::Return {
            argument,
            comments: _,
            return_out: _,
        } = stmt;
        if let Some(argument) = argument {
            self.base_expression(self.predicate, argument);
        }
        Ok(())
    }

    fn function_(
        &mut self,
        loc: &'ast ALoc,
        fn_: &'ast ast::function::Function<ALoc, (ALoc, Type)>,
    ) -> Result<(), !> {
        let has_predicate = fn_.predicate.is_some();
        self.push_pred(has_predicate, |this| {
            let Ok(()) = ast_visitor::function_default(this, loc, fn_);
        });
        Ok(())
    }

    fn call(
        &mut self,
        loc: &'ast (ALoc, Type),
        expr: &'ast expression::Call<ALoc, (ALoc, Type)>,
    ) -> Result<(), !> {
        let expression::Call {
            callee,
            targs,
            arguments,
            comments: _,
        } = expr;
        let is_invariant = matches!(
            callee.deref(),
            ExpressionInner::Identifier { inner, .. }
            if inner.name.as_str() == "invariant"
        );
        if is_invariant && targs.is_none() {
            if let Some((expression::ExpressionOrSpread::Expression(conditional), rest_args)) =
                arguments.arguments.split_first()
            {
                self.expression(callee)?;
                self.base_expression(true, conditional);
                for arg in rest_args {
                    self.expression_or_spread(arg)?;
                }
                return Ok(());
            }
        }
        let Ok(()) = ast_visitor::call_default(self, loc, expr);
        Ok(())
    }

    fn logical(
        &mut self,
        _loc: &'ast (ALoc, Type),
        expr: &'ast expression::Logical<ALoc, (ALoc, Type)>,
    ) -> Result<(), !> {
        let expression::Logical {
            operator,
            left,
            right,
            comments: _,
        } = expr;
        let c = match operator {
            expression::LogicalOperator::And | expression::LogicalOperator::Or => true,
            expression::LogicalOperator::NullishCoalesce => false,
        };
        let cur_c = self.cond;
        self.base_expression(c, left);
        self.base_expression(cur_c, right);
        Ok(())
    }

    fn assignment(
        &mut self,
        _loc: &'ast (ALoc, Type),
        expr: &'ast expression::Assignment<ALoc, (ALoc, Type)>,
    ) -> Result<(), !> {
        let expression::Assignment {
            operator,
            left,
            right,
            comments: _,
        } = expr;
        let (unwrapped_left, _) = ast_utils::unwrap_nonnull_lhs(left);
        let left_expr: Option<expression::Expression<ALoc, (ALoc, Type)>> = match &*unwrapped_left {
            ast::pattern::Pattern::Identifier {
                loc: lhs_loc,
                inner,
            } => Some(expression::Expression::new(ExpressionInner::Identifier {
                loc: lhs_loc.dupe(),
                inner: inner.name.dupe(),
            })),
            ast::pattern::Pattern::Expression {
                loc: lhs_loc,
                inner,
            } => match &***inner {
                ExpressionInner::Member { inner: mem, .. } => {
                    Some(expression::Expression::new(ExpressionInner::Member {
                        loc: lhs_loc.dupe(),
                        inner: mem.dupe(),
                    }))
                }
                _ => None,
            },
            _ => None,
        };
        match (operator, left_expr) {
            (
                Some(
                    expression::AssignmentOperator::AndAssign
                    | expression::AssignmentOperator::OrAssign,
                ),
                Some(left_expr),
            ) => {
                self.base_expression(true, &left_expr);
                self.expression(right)?;
            }
            _ => {
                let Ok(()) = ast_visitor::assignment_default(self, _loc, expr);
            }
        }
        Ok(())
    }

    fn unary_expression(
        &mut self,
        _loc: &'ast (ALoc, Type),
        expr: &'ast expression::Unary<ALoc, (ALoc, Type)>,
    ) -> Result<(), !> {
        let expression::Unary {
            argument,
            operator,
            comments: _,
        } = expr;
        match operator {
            expression::UnaryOperator::Not => {
                self.base_expression(self.cond, argument);
            }
            _ => {
                let Ok(()) = ast_visitor::unary_expression_default(self, _loc, expr);
            }
        }
        Ok(())
    }
}

pub fn mark(cx: &Context, ast: &ast::Program<ALoc, (ALoc, Type)>) {
    let mut marker = Marker::new(cx);
    let Ok(()) = marker.program(ast);
}
