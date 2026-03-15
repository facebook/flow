/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Checks that super() is called before accessing this or super in derived class constructors.
//!
//! In JavaScript/Flow, a derived class constructor (a class that extends another)
//! must call super() before any access to `this` or `super`. This checker validates
//! that constraint by tracking control flow through the constructor body.

use std::ops::Deref;

use dupe::Dupe;
use flow_common::reason::Name;
use flow_parser::ast;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_typing_errors::error_message::BindingError;
use flow_typing_errors::error_message::ErrorMessage;

/// State tracking whether super() has been called on the current control flow path
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SuperCallState {
    NotCalled,   // super() definitely not called on this path
    Called,      // super() definitely called on this path
    MaybeCalled, // super() called on some but not all paths
}

fn merge_state(s1: SuperCallState, s2: SuperCallState) -> SuperCallState {
    match (s1, s2) {
        (SuperCallState::Called, SuperCallState::Called) => SuperCallState::Called,
        (SuperCallState::NotCalled, SuperCallState::NotCalled) => SuperCallState::NotCalled,
        _ => SuperCallState::MaybeCalled,
    }
}

fn state_allows_access(state: SuperCallState) -> bool {
    matches!(state, SuperCallState::Called)
}

struct Checker<'a, Loc: Clone + Dupe + PartialOrd + Ord + PartialEq + Eq> {
    add_output: &'a mut dyn FnMut(ErrorMessage<Loc>),
    this_def_loc: Loc,
    super_def_loc: Loc,
    // Current state: has super() been called?
    super_call_state: SuperCallState,
    // Track error locations
    this_before_super_locs: Vec<Loc>,
    super_before_super_call_locs: Vec<Loc>,
    duplicate_super_call_locs: Vec<Loc>,
}

impl<'a, Loc: Clone + Dupe + PartialOrd + Ord + PartialEq + Eq> Checker<'a, Loc> {
    fn new(
        add_output: &'a mut dyn FnMut(ErrorMessage<Loc>),
        this_def_loc: Loc,
        super_def_loc: Loc,
    ) -> Self {
        Self {
            add_output,
            this_def_loc,
            super_def_loc,
            super_call_state: SuperCallState::NotCalled,
            this_before_super_locs: Vec::new(),
            super_before_super_call_locs: Vec::new(),
            duplicate_super_call_locs: Vec::new(),
        }
    }

    fn get_state(&self) -> SuperCallState {
        self.super_call_state
    }

    fn set_state(&mut self, s: SuperCallState) {
        self.super_call_state = s;
    }

    fn mark_super_called(&mut self, loc: Loc) {
        match self.super_call_state {
            SuperCallState::Called => {
                self.duplicate_super_call_locs.push(loc);
            }
            SuperCallState::MaybeCalled => {
                self.duplicate_super_call_locs.push(loc);
                self.super_call_state = SuperCallState::Called;
            }
            SuperCallState::NotCalled => {
                self.super_call_state = SuperCallState::Called;
            }
        }
    }

    fn run_in_branch<F, R>(&mut self, f: F) -> SuperCallState
    where
        F: FnOnce(&mut Self) -> R,
    {
        let saved_state = self.get_state();
        let _ = f(self);
        let result_state = self.get_state();
        self.set_state(saved_state);
        result_state
    }
}

impl<'ast, 'a, Loc: Clone + Dupe + PartialOrd + Ord + PartialEq + Eq>
    AstVisitor<'ast, Loc, Loc, &'ast Loc, ()> for Checker<'a, Loc>
{
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn this_expression(&mut self, loc: &Loc, _expr: &ast::expression::This<Loc>) -> Result<(), ()> {
        if !state_allows_access(self.super_call_state) {
            self.this_before_super_locs.push(loc.dupe());
        }
        Ok(())
    }

    fn super_expression(
        &mut self,
        loc: &Loc,
        _expr: &ast::expression::Super<Loc>,
    ) -> Result<(), ()> {
        if !state_allows_access(self.super_call_state) {
            self.super_before_super_call_locs.push(loc.dupe());
        }
        Ok(())
    }

    fn call(&mut self, loc: &Loc, expr: &ast::expression::Call<Loc, Loc>) -> Result<(), ()> {
        use ast::expression::ExpressionInner as E;
        match expr.callee.deref() {
            E::Super { .. } => {
                // Visit arguments first (they might reference this/super), then mark super called
                if let Some(targs) = &expr.targs {
                    self.call_type_args(targs)?;
                }
                self.arg_list(&expr.arguments)?;
                // Only after the super call, `this` and `super` are defined.
                self.mark_super_called(loc.dupe());
                Ok(())
            }
            _ => ast_visitor::call_default(self, loc, expr),
        }
    }

    // Skip nested classes - they have their own super() requirements
    fn class_(&mut self, _loc: &Loc, _cls: &ast::class::Class<Loc, Loc>) -> Result<(), ()> {
        Ok(())
    }

    fn record_declaration(
        &mut self,
        _loc: &Loc,
        _record: &ast::statement::RecordDeclaration<Loc, Loc>,
    ) -> Result<(), ()> {
        Ok(())
    }

    // Skip nested regular functions - they bind their own `this` and `super`,
    // so this/super references inside them don't refer to the enclosing constructor.
    // Note: arrow functions are NOT skipped because they inherit `this` from the
    // enclosing scope.
    fn function_declaration(
        &mut self,
        _loc: &Loc,
        _decl: &ast::function::Function<Loc, Loc>,
    ) -> Result<(), ()> {
        Ok(())
    }

    fn function_expression(
        &mut self,
        _loc: &Loc,
        _expr: &ast::function::Function<Loc, Loc>,
    ) -> Result<(), ()> {
        Ok(())
    }

    // Control flow merging: if statement
    fn if_statement(&mut self, _loc: &Loc, stmt: &ast::statement::If<Loc, Loc>) -> Result<(), ()> {
        self.expression(&stmt.test)?;
        let then_state = self.run_in_branch(|this| this.statement(&stmt.consequent));
        let else_state = match &stmt.alternate {
            Some(alt) => self.run_in_branch(|this| this.statement(&alt.body)),
            None => self.get_state(), // No else = unchanged state
        };
        self.set_state(merge_state(then_state, else_state));
        Ok(())
    }

    // Control flow merging: conditional expression
    fn conditional(
        &mut self,
        _loc: &Loc,
        expr: &ast::expression::Conditional<Loc, Loc>,
    ) -> Result<(), ()> {
        self.expression(&expr.test)?;
        let then_state = self.run_in_branch(|this| this.expression(&expr.consequent));
        let else_state = self.run_in_branch(|this| this.expression(&expr.alternate));
        self.set_state(merge_state(then_state, else_state));
        Ok(())
    }

    // Control flow merging: logical expressions (short-circuit)
    fn logical(&mut self, _loc: &Loc, expr: &ast::expression::Logical<Loc, Loc>) -> Result<(), ()> {
        self.expression(&expr.left)?;
        let right_state = self.run_in_branch(|this| this.expression(&expr.right));
        // Short-circuit: right may or may not execute
        self.set_state(merge_state(self.get_state(), right_state));
        Ok(())
    }

    // Control flow merging: switch statement
    fn switch(&mut self, _loc: &Loc, stmt: &ast::statement::Switch<Loc, Loc>) -> Result<(), ()> {
        self.expression(&stmt.discriminant)?;
        let pre_switch_state = self.get_state();
        let states: Vec<_> = stmt
            .cases
            .iter()
            .map(|case| {
                self.run_in_branch(|this| {
                    if let Some(test) = &case.test {
                        let _ = this.expression(test);
                    }
                    for s in case.consequent.iter() {
                        let _ = this.statement(s);
                    }
                })
            })
            .collect();
        // Merge all case states with pre-switch state (in case no case matches)
        let merged = states.into_iter().fold(pre_switch_state, merge_state);
        self.set_state(merged);
        Ok(())
    }

    // Control flow merging: try/catch/finally
    fn try_catch(&mut self, _loc: &Loc, stmt: &ast::statement::Try<Loc, Loc>) -> Result<(), ()> {
        let pre_try_state = self.get_state();
        // Try block
        let try_state = self.run_in_branch(|this| this.block(&stmt.block.0, &stmt.block.1));
        // Catch block - exception could happen at any point in try, so start from pre_try_state
        let catch_state = match &stmt.handler {
            Some(catch) => {
                self.set_state(pre_try_state);
                self.run_in_branch(|this| {
                    if let Some(param) = &catch.param {
                        let _ = this.catch_clause_pattern(param);
                    }
                    this.block(&catch.body.0, &catch.body.1)
                })
            }
            None => pre_try_state,
        };
        // After try/catch, merge states
        self.set_state(merge_state(try_state, catch_state));
        // Finally always runs, update state
        if let Some((loc, final_block)) = &stmt.finalizer {
            self.block(loc, final_block)?;
        }
        Ok(())
    }

    // Control flow merging: while loop (body may not execute)
    fn while_(&mut self, _loc: &Loc, stmt: &ast::statement::While<Loc, Loc>) -> Result<(), ()> {
        self.expression(&stmt.test)?;
        let body_state = self.run_in_branch(|this| this.statement(&stmt.body));
        // Loop may not execute at all, so merge with pre-loop state
        self.set_state(merge_state(self.get_state(), body_state));
        Ok(())
    }

    // Control flow merging: do-while loop (body executes at least once)
    fn do_while(&mut self, _loc: &Loc, stmt: &ast::statement::DoWhile<Loc, Loc>) -> Result<(), ()> {
        // Do-while body executes at least once
        self.statement(&stmt.body)?;
        self.expression(&stmt.test)?;
        Ok(())
    }

    // Control flow merging: for loop
    fn for_statement(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::For<Loc, Loc>,
    ) -> Result<(), ()> {
        if let Some(init) = &stmt.init {
            use ast::statement::for_::Init;
            match init {
                Init::InitDeclaration((loc, decl)) => {
                    self.variable_declaration(loc, decl)?;
                }
                Init::InitExpression(expr) => {
                    self.expression(expr)?;
                }
            }
        }
        if let Some(test) = &stmt.test {
            self.expression(test)?;
        }
        let body_state = self.run_in_branch(|this| {
            let _ = this.statement(&stmt.body);
            if let Some(update) = &stmt.update {
                let _ = this.expression(update);
            }
        });
        self.set_state(merge_state(self.get_state(), body_state));
        Ok(())
    }

    // Control flow merging: for-in loop
    fn for_in_statement(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::ForIn<Loc, Loc>,
    ) -> Result<(), ()> {
        use ast::statement::for_in::Left;
        match &stmt.left {
            Left::LeftDeclaration((loc, decl)) => {
                self.variable_declaration(loc, decl)?;
            }
            Left::LeftPattern(pat) => {
                self.for_in_assignment_pattern(pat)?;
            }
        }
        self.expression(&stmt.right)?;
        let body_state = self.run_in_branch(|this| this.statement(&stmt.body));
        self.set_state(merge_state(self.get_state(), body_state));
        Ok(())
    }

    // Control flow merging: for-of loop
    fn for_of_statement(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::ForOf<Loc, Loc>,
    ) -> Result<(), ()> {
        use ast::statement::for_of::Left;
        match &stmt.left {
            Left::LeftDeclaration((loc, decl)) => {
                self.variable_declaration(loc, decl)?;
            }
            Left::LeftPattern(pat) => {
                self.for_of_assignment_pattern(pat)?;
            }
        }
        self.expression(&stmt.right)?;
        let body_state = self.run_in_branch(|this| this.statement(&stmt.body));
        self.set_state(merge_state(self.get_state(), body_state));
        Ok(())
    }
}

impl<'a, Loc: Clone + Dupe + PartialOrd + Ord + PartialEq + Eq> Checker<'a, Loc> {
    fn add_errors(&mut self) {
        for loc in &self.this_before_super_locs {
            (self.add_output)(ErrorMessage::EBindingError(
                BindingError::EReferencedThisSuperBeforeSuperCall,
                loc.dupe(),
                Name::new("this"),
                self.this_def_loc.dupe(),
            ));
        }
        for loc in &self.super_before_super_call_locs {
            (self.add_output)(ErrorMessage::EBindingError(
                BindingError::EReferencedThisSuperBeforeSuperCall,
                loc.dupe(),
                Name::new("super"),
                self.super_def_loc.dupe(),
            ));
        }
        for loc in &self.duplicate_super_call_locs {
            (self.add_output)(ErrorMessage::EBindingError(
                BindingError::ENameAlreadyBound,
                loc.dupe(),
                Name::new("this"),
                self.this_def_loc.dupe(),
            ));
            (self.add_output)(ErrorMessage::EBindingError(
                BindingError::ENameAlreadyBound,
                loc.dupe(),
                Name::new("super"),
                self.super_def_loc.dupe(),
            ));
        }
    }
}

/// Checks that super() is called before accessing this or super in derived class constructors.
pub fn check<Loc: Clone + Dupe + PartialOrd + Ord + PartialEq + Eq>(
    _enable_enums: bool,
    add_output: &mut dyn FnMut(ErrorMessage<Loc>),
    loc: Loc,
    cls: &ast::class::Class<Loc, Loc>,
) {
    let this_def_loc = cls
        .id
        .as_ref()
        .map(|id| id.loc.dupe())
        .unwrap_or_else(|| loc.dupe());
    let Some(extends) = &cls.extends else {
        return;
    };
    let super_def_loc = extends.loc.dupe();

    for member in cls.body.body.iter() {
        use ast::class::BodyElement;
        use ast::class::MethodKind;
        if let BodyElement::Method(method) = member {
            if method.kind == MethodKind::Constructor {
                let (func_loc, constructor) = &method.value;
                let mut checker =
                    Checker::new(add_output, this_def_loc.dupe(), super_def_loc.dupe());
                let _ = ast_visitor::function_default(&mut checker, func_loc, constructor);
                checker.add_errors();
            }
        }
    }
}
