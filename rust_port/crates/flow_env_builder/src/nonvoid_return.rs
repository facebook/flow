/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! OCaml: flow/src/analysis/env_builder/nonvoid_return.ml
//!
//! Utility for detecting functions that might have non-void returns.
//! Uses the visitor pattern to traverse the AST and detect if a function body
//! might return a non-void value (explicit return with argument, throw, or body expression).

use std::collections::BTreeMap;
use std::ops::Deref;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::function;
use flow_parser::ast_utils::is_call_to_invariant;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;

use crate::assertion_call_target::AssertionInfo;
use crate::assertion_call_target::bare_assertion_call_always_throws;

struct ReturnFinder<'a, L> {
    acc: bool,
    invariant_special_casing_disabled: bool,
    assertion_calls: &'a BTreeMap<L, AssertionInfo>,
}

impl<'a, L> ReturnFinder<'a, L> {
    fn new(
        invariant_special_casing_disabled: bool,
        assertion_calls: &'a BTreeMap<L, AssertionInfo>,
    ) -> Self {
        Self {
            acc: false,
            invariant_special_casing_disabled,
            assertion_calls,
        }
    }

    fn set_acc(&mut self, value: bool) {
        self.acc = value;
    }
}

impl<'ast, Loc: Dupe + Ord> AstVisitor<'ast, Loc> for ReturnFinder<'_, Loc> {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn return_(
        &mut self,
        _loc: &'ast Loc,
        ret: &'ast ast::statement::Return<Loc, Loc>,
    ) -> Result<(), !> {
        if ret.argument.is_some() {
            self.set_acc(true);
        }
        Ok(())
    }

    fn call(
        &mut self,
        _loc: &'ast Loc,
        expr: &'ast ast::expression::Call<Loc, Loc>,
    ) -> Result<(), !> {
        if self
            .assertion_calls
            .get(expr.callee.loc())
            .is_some_and(|assertion| {
                bare_assertion_call_always_throws(*assertion, &expr.arguments.arguments)
            })
        {
            self.set_acc(true);
        }
        if !self.invariant_special_casing_disabled && is_call_to_invariant(&expr.callee) {
            // invariant() and invariant(false, ...) are treated like throw
            match &expr.arguments.arguments[..] {
                [] => self.set_acc(true),
                [first, ..] => {
                    if let ast::expression::ExpressionOrSpread::Expression(e) = first {
                        if let ExpressionInner::BooleanLiteral { inner, .. } = e.deref() {
                            if !inner.value {
                                self.set_acc(true);
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn throw(
        &mut self,
        _loc: &'ast Loc,
        _throw: &'ast ast::statement::Throw<Loc, Loc>,
    ) -> Result<(), !> {
        self.set_acc(true);
        Ok(())
    }

    /// If it's a body expression, some value is implicitly returned
    fn body_expression(
        &mut self,
        expr: &'ast ast::expression::Expression<Loc, Loc>,
    ) -> Result<(), !> {
        self.set_acc(true);
        ast_visitor::body_expression_default(self, expr)
    }

    // Any returns in these constructs would be for nested function definitions,
    // so we short-circuit them.

    fn class_(
        &mut self,
        _loc: &'ast Loc,
        _cls: &'ast ast::class::Class<Loc, Loc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn function_declaration(
        &mut self,
        _loc: &'ast Loc,
        _stmt: &'ast function::Function<Loc, Loc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn function_expression(
        &mut self,
        _loc: &'ast Loc,
        _stmt: &'ast function::Function<Loc, Loc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn arrow_function(
        &mut self,
        _loc: &'ast Loc,
        _expr: &'ast function::Function<Loc, Loc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn component_declaration(
        &mut self,
        _loc: &'ast Loc,
        _component: &'ast ast::statement::ComponentDeclaration<Loc, Loc>,
    ) -> Result<(), !> {
        Ok(())
    }
}

pub fn might_have_nonvoid_return<'ast, L: Clone + Dupe + Ord>(
    loc: &'ast L,
    function: &'ast function::Function<L, L>,
    invariant_special_casing_disabled: bool,
    assertion_calls: &BTreeMap<L, AssertionInfo>,
) -> bool {
    let mut finder = ReturnFinder::new(invariant_special_casing_disabled, assertion_calls);
    let Ok(()) = finder.function_(loc, function);
    finder.acc
}
