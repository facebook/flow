/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::ops::Deref;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_lint_settings::lints::PropertyAssignmentKind;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::expression::member;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast_utils;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use vec1::Vec1;

use crate::ssa_builder::AbruptCompletion;

#[derive(Debug, Clone)]
pub struct Error<Loc> {
    pub loc: Loc,
    pub desc: PropertyAssignmentKind,
}

#[derive(Debug, Clone, Default)]
pub struct Errors<Loc> {
    pub public_property_errors: BTreeMap<FlowSmolStr, Vec<Error<Loc>>>,
    pub private_property_errors: BTreeMap<FlowSmolStr, Vec<Error<Loc>>>,
}

// let public_property loc ident =
//   let (_, ({ Ast.Identifier.name; comments = _ } as r)) = ident in
//   (loc, { r with Ast.Identifier.name = "this." ^ name })
fn public_property<'a>(
    loc: &'a ALoc,
    ident: &'a ast::Identifier<ALoc, ALoc>,
) -> (&'a ALoc, ast::Identifier<ALoc, ALoc>) {
    let name = ast_utils::name_of_ident(ident);
    (
        loc,
        ast::Identifier::new(ast::IdentifierInner {
            loc: loc.dupe(),
            name: FlowSmolStr::from(format!("this.{}", name)),
            comments: ident.comments.clone(),
        }),
    )
}

// let private_property loc ident =
//   let (_, { Ast.PrivateName.name; comments }) = ident in
//   (loc, { Ast.Identifier.name = "this.#" ^ name; comments })
fn private_property<'a>(
    loc: &'a ALoc,
    ident: &'a ast::PrivateName<ALoc>,
) -> (&'a ALoc, ast::Identifier<ALoc, ALoc>) {
    let name = &ident.name;
    (
        loc,
        ast::Identifier::new(ast::IdentifierInner {
            loc: loc.dupe(),
            name: FlowSmolStr::from(format!("this.#{}", name)),
            comments: ident.comments.clone(),
        }),
    )
}

// Boolean init environment: true = definitely initialized, false = potentially uninitialized
type InitEnv = BTreeMap<FlowSmolStr, bool>;

fn merge_envs(env1: &InitEnv, env2: &InitEnv) -> InitEnv {
    let mut result = InitEnv::new();
    for key in env1.keys().chain(env2.keys()) {
        if result.contains_key(key) {
            continue;
        }
        let val = match (env1.get(key), env2.get(key)) {
            (Some(&a), Some(&b)) => a && b,
            _ => false,
        };
        result.insert(key.dupe(), val);
    }
    result
}

/// Unreachable environment: all properties are "true" so AND-merging
///  with a reachable env yields the reachable env's values  
fn unreachable_env(env: &InitEnv) -> InitEnv {
    env.keys().map(|k| (k.dupe(), true)).collect()
}

fn from_completion(completion: Option<AbruptCompletion>) -> Result<(), AbruptCompletion> {
    match completion {
        None => Ok(()),
        Some(abrupt) => Err(abrupt),
    }
}

fn merge_completion_states(
    hd_completion_state: &Option<AbruptCompletion>,
    tl_completion_states: &[Option<AbruptCompletion>],
) -> Result<(), AbruptCompletion> {
    match hd_completion_state {
        None => Ok(()),
        Some(abrupt) => {
            let all_same = tl_completion_states.iter().all(|state| match state {
                None => false,
                Some(other) => abrupt == other,
            });
            if all_same { Err(abrupt.dupe()) } else { Ok(()) }
        }
    }
}

struct PropertyAssignment<'ast> {
    property_names: &'ast BTreeSet<FlowSmolStr>,
    init_env: InitEnv,
    possible_labeled_continues: Vec<AbruptCompletion>,
    abrupt_completion_envs: Vec<(AbruptCompletion, InitEnv)>,
    read_errors: Vec<(ALoc, FlowSmolStr)>,
    this_escape_errors: Vec<(ALoc, PropertyAssignmentKind, InitEnv)>,
}

impl<'ast> PropertyAssignment<'ast> {
    fn new(
        property_names: &'ast BTreeSet<FlowSmolStr>,
        properties: &[&ast::Identifier<ALoc, ALoc>],
    ) -> Self {
        let init_env = properties.iter().fold(InitEnv::new(), |mut acc, id| {
            acc.insert(ast_utils::name_of_ident(id).dupe(), false);
            acc
        });
        Self {
            property_names,
            init_env,
            possible_labeled_continues: Vec::new(),
            abrupt_completion_envs: Vec::new(),
            read_errors: Vec::new(),
            this_escape_errors: Vec::new(),
        }
    }

    fn init_env(&self) -> &InitEnv {
        &self.init_env
    }

    fn run_to_completion(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<(), AbruptCompletion>,
    ) -> Option<AbruptCompletion> {
        f(self).err()
    }

    fn run(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<(), AbruptCompletion>,
        finally: impl FnOnce(&mut Self),
    ) -> Result<(), AbruptCompletion> {
        match f(self) {
            Ok(result) => {
                finally(self);
                Ok(result)
            }
            Err(e) => {
                finally(self);
                Err(e)
            }
        }
    }

    fn raise_abrupt_completion(&mut self, ac: AbruptCompletion) -> Result<(), AbruptCompletion> {
        let env = self.init_env.clone();
        self.init_env = unreachable_env(&self.init_env);
        self.abrupt_completion_envs.push((ac.dupe(), env));
        Err(ac)
    }

    fn expecting_abrupt_completions(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<(), AbruptCompletion>,
    ) -> Result<(), AbruptCompletion> {
        let saved = std::mem::take(&mut self.abrupt_completion_envs);
        self.run(f, |this| {
            let current = std::mem::take(&mut this.abrupt_completion_envs);
            this.abrupt_completion_envs = saved;
            this.abrupt_completion_envs.reverse();
            this.abrupt_completion_envs.extend(current);
        })
    }

    fn commit_abrupt_completion_matching(
        &mut self,
        filter: impl Fn(&AbruptCompletion) -> bool,
        completion_state: &Option<AbruptCompletion>,
    ) -> Result<(), AbruptCompletion> {
        let (matching, non_matching): (Vec<_>, Vec<_>) = self
            .abrupt_completion_envs
            .drain(..)
            .partition(|(ac, _env)| filter(ac));

        if !matching.is_empty() {
            for (_ac, env) in matching {
                self.merge_remote_env(&env);
            }
            self.abrupt_completion_envs = non_matching;
            Ok(())
        } else {
            self.abrupt_completion_envs = non_matching;
            match completion_state {
                Some(ac) if !filter(ac) => Err(ac.dupe()),
                _ => Ok(()),
            }
        }
    }

    fn expecting_return_or_throw(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<(), AbruptCompletion>,
    ) {
        let completion_state = self.run_to_completion(f);
        let Ok(()) = self.commit_abrupt_completion_matching(
            |ac| {
                AbruptCompletion::mem(
                    &[AbruptCompletion::return_(), AbruptCompletion::throw()],
                    ac,
                )
            },
            &completion_state,
        ) else {
            panic!("unexpected abrupt completion in expecting_return_or_throw");
        };
    }

    // WRITES
    fn merge_remote_env(&mut self, env: &InitEnv) {
        self.init_env = merge_envs(&self.init_env, env);
    }

    fn merge_self_env(&mut self, env: &InitEnv) {
        self.init_env = merge_envs(&self.init_env, env);
    }

    fn reset_env(&mut self, env: &InitEnv) {
        self.init_env = env.clone();
    }

    fn initialize_property(
        &mut self,
        property_id: &ast::Identifier<ALoc, ALoc>,
        value: &'ast ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        match value.deref() {
            ExpressionInner::ArrowFunction { .. } | ExpressionInner::Function { .. } => {}
            _ => {
                self.expression(value)?;
            }
        }
        let name = ast_utils::name_of_ident(property_id);
        if self.init_env.contains_key(name) {
            self.init_env.insert(name.dupe(), true);
        }
        Ok(())
    }

    // READS
    fn read_errors(&self) -> &[(ALoc, FlowSmolStr)] {
        &self.read_errors
    }

    // PREVENT THIS FROM ESCAPING
    fn this_escape_errors(&self) -> &[(ALoc, PropertyAssignmentKind, InitEnv)] {
        &self.this_escape_errors
    }

    fn add_this_escape_error(&mut self, error: (ALoc, PropertyAssignmentKind, InitEnv)) {
        self.this_escape_errors.push(error);
    }

    fn handle_switch_case(
        &mut self,
        env: InitEnv,
        mut case_completion_states: Vec<Option<AbruptCompletion>>,
        case: &'ast ast::statement::switch::Case<ALoc, ALoc>,
    ) -> Result<(InitEnv, Vec<Option<AbruptCompletion>>), AbruptCompletion> {
        let ast::statement::switch::Case {
            test,
            case_test_loc: _,
            consequent,
            comments: _,
            ..
        } = case;
        if let Some(test) = test {
            self.expression(test)?;
        }
        let env0 = self.init_env.clone();
        self.init_env = merge_envs(&env0, &env);
        let case_completion_state = self.run_to_completion(|this| this.statement_list(consequent));
        let env_prime = self.init_env.clone();
        self.reset_env(&env0);
        case_completion_states.push(case_completion_state);
        Ok((env_prime, case_completion_states))
    }

    // LAMBDAS
    fn visit_lambda(&mut self, body_f: impl FnOnce(&mut Self) -> Result<(), AbruptCompletion>) {
        let saved_env = self.init_env.clone();
        let saved_completions = std::mem::take(&mut self.abrupt_completion_envs);
        let saved_continues = std::mem::take(&mut self.possible_labeled_continues);
        self.init_env = self.init_env.keys().map(|k| (k.dupe(), false)).collect();
        let _completion_state = self.run_to_completion(body_f);
        self.abrupt_completion_envs = saved_completions;
        self.possible_labeled_continues = saved_continues;
        self.init_env = saved_env;
    }
}

impl<'ast> AstVisitor<'ast, ALoc, ALoc, &'ast ALoc, AbruptCompletion> for PropertyAssignment<'ast> {
    fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
        loc
    }

    fn normalize_type(type_: &'ast ALoc) -> &'ast ALoc {
        type_
    }

    fn identifier(
        &mut self,
        _id: &'ast ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn jsx_element_name_identifier(
        &mut self,
        _ident: &'ast ast::jsx::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    // method! member loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Member.t) =
    //   match expr with
    //   | { ... } ->
    //     let property_name : string =
    //       Flow_ast_utils.name_of_ident
    //         Ast.Expression.Member.(
    //           match property with
    //           | PropertyIdentifier id -> public_property loc id
    //           | PropertyPrivateName id -> private_property loc id
    //           | PropertyExpression _ -> failwith "match on expr makes this impossible"
    //         )
    //     in
    //     (match SMap.find_opt property_name init_env with
    //     | Some false -> read_errors <- (loc, property_name) :: read_errors
    //     | _ -> ());
    //     expr
    //   | _ -> super#member loc expr
    fn member(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast ast::expression::Member<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        match expr {
            ast::expression::Member {
                object,
                property:
                    property @ (member::Property::PropertyIdentifier(_)
                    | member::Property::PropertyPrivateName(_)),
                comments: _,
            } if matches!(object.deref(), ExpressionInner::This { .. }) => {
                // let property_name = Flow_ast_utils.name_of_ident @@ (match property with ...)
                let property_name = ast_utils::name_of_ident(&match property {
                    member::Property::PropertyIdentifier(id) => public_property(loc, id).1,
                    member::Property::PropertyPrivateName(id) => private_property(loc, id).1,
                    member::Property::PropertyExpression(_) => {
                        unreachable!("match on expr makes this impossible")
                    }
                })
                .dupe();
                // (* Check if the property is tracked and not yet initialized *)
                match self.init_env.get(&property_name) {
                    Some(false) => {
                        self.read_errors.push((loc.dupe(), property_name));
                    }
                    _ => {}
                }
                Ok(())
            }
            _ => ast_visitor::member_default(self, loc, expr),
        }
    }

    // EVALUATION ORDER
    fn assignment(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast ast::expression::Assignment<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::expression::Assignment {
            operator,
            left,
            right,
            comments: _,
        } = expr;
        if let ast::pattern::Pattern::Expression { inner, .. } = left {
            let (left, _, _) = ast_utils::unwrap_nonnull_lhs_expr(inner.as_ref());
            if let ExpressionInner::Member {
                loc: member_loc,
                inner,
            } = left.deref()
            {
                let left_member = inner.as_ref();
                match left_member {
                    ast::expression::Member {
                        object,
                        property:
                            property @ (member::Property::PropertyIdentifier(_)
                            | member::Property::PropertyPrivateName(_)),
                        comments: _,
                    } if matches!(object.deref(), ExpressionInner::This { .. }) => {
                        match operator {
                            // given `this.x = e`, read e then write x
                            None => {
                                self.initialize_property(
                                    &match property {
                                        member::Property::PropertyIdentifier(id) => {
                                            public_property(member_loc, id).1
                                        }
                                        member::Property::PropertyPrivateName(id) => {
                                            private_property(member_loc, id).1
                                        }
                                        member::Property::PropertyExpression(_) => {
                                            unreachable!("match on expr makes this impossible")
                                        }
                                    },
                                    right,
                                )?;
                            }
                            // given `this.x += e`, read x then read e
                            Some(_) => {
                                self.member(member_loc, left_member)?;
                                self.expression(right)?;
                            }
                        }
                        // This expression technically also writes to x, but we don't model that
                        // here since in order for `this.x += e` to not cause an error, x must
                        // already be assigned anyway. Also, not writing to x here leads to
                        // more understandable error messages, as the write would mask the
                        // PropertyNotDefinitelyInitialized error.
                        return Ok(());
                    }
                    _ => {}
                }
            }
        }
        ast_visitor::assignment_default(self, loc, expr)
    }

    fn expression(
        &mut self,
        expr: &'ast ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        match expr.deref() {
            ExpressionInner::This { loc, .. } => {
                self.add_this_escape_error((
                    loc.dupe(),
                    PropertyAssignmentKind::ThisBeforeEverythingInitialized,
                    self.init_env.clone(),
                ));
            }
            _ => {}
        }
        ast_visitor::expression_default(self, expr)
    }

    fn call(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast ast::expression::Call<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        match expr.callee.deref() {
            ExpressionInner::Member {
                loc: member_loc,
                inner,
            } if matches!(inner.object.deref(), ExpressionInner::This { .. })
                && matches!(
                    &inner.property,
                    member::Property::PropertyIdentifier(_)
                        | member::Property::PropertyPrivateName(_)
                ) =>
            {
                let property = &inner.property;
                let name = ast_utils::name_of_ident(&match property {
                    member::Property::PropertyIdentifier(id) => public_property(member_loc, id).1,
                    member::Property::PropertyPrivateName(id) => private_property(member_loc, id).1,
                    member::Property::PropertyExpression(_) => {
                        unreachable!("match on expr.callee makes this impossible")
                    }
                })
                .dupe();
                let error = if self.property_names.contains(&name) {
                    PropertyAssignmentKind::PropertyFunctionCallBeforeEverythingInitialized
                } else {
                    PropertyAssignmentKind::MethodCallBeforeEverythingInitialized
                };
                self.add_this_escape_error((loc.dupe(), error, self.init_env.clone()));
            }
            _ => {}
        }
        ast_visitor::call_default(self, loc, expr)
    }

    // CONTROL FLOW

    fn if_statement(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::If<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::statement::If {
            test,
            consequent,
            alternate,
            ..
        } = stmt;
        self.expression(test)?;
        let env0 = self.init_env.clone();
        let then_completion_state = self.run_to_completion(|this| {
            this.if_consequent_statement(alternate.is_some(), consequent)
        });
        let env1 = self.init_env.clone();
        self.reset_env(&env0);
        let else_completion_state = self.run_to_completion(|this| {
            if let Some(alt) = alternate {
                this.statement(&alt.body)?;
            }
            Ok(())
        });
        self.merge_self_env(&env1);
        merge_completion_states(&then_completion_state, &[else_completion_state])?;
        Ok(())
    }

    fn while_(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::While<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let continues: Vec<_> = std::iter::once(AbruptCompletion::Continue(None))
                .chain(this.possible_labeled_continues.clone())
                .collect();
            let ast::statement::While {
                test,
                body,
                comments: _,
            } = stmt;
            this.expression(test)?;
            let env_after_test = this.init_env.clone();
            let loop_completion_state = this.run_to_completion(|this2| this2.statement(body));
            let loop_completion_state = this.run_to_completion(|this2| {
                this2.commit_abrupt_completion_matching(
                    |ac| AbruptCompletion::mem(&continues, ac),
                    &loop_completion_state,
                )
            });
            // Post-while = post-test env (test always runs at least once)
            this.reset_env(&env_after_test);
            let while_completion_states = (None, vec![loop_completion_state]);
            let completion_state = this.run_to_completion(|_| {
                merge_completion_states(&while_completion_states.0, &while_completion_states.1)
            });
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    fn do_while(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::DoWhile<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let continues: Vec<_> = std::iter::once(AbruptCompletion::Continue(None))
                .chain(this.possible_labeled_continues.clone())
                .collect();
            let ast::statement::DoWhile { body, test, .. } = stmt;
            let loop_completion_state = this.run_to_completion(|this2| this2.statement(body));
            let loop_completion_state = this.run_to_completion(|this2| {
                this2.commit_abrupt_completion_matching(
                    |ac| AbruptCompletion::mem(&continues, ac),
                    &loop_completion_state,
                )
            });
            match loop_completion_state {
                None => {
                    this.expression(test)?;
                }
                _ => {}
            }
            let do_while_completion_states = (loop_completion_state, vec![]);
            let completion_state = this.run_to_completion(|_| {
                merge_completion_states(
                    &do_while_completion_states.0,
                    &do_while_completion_states.1,
                )
            });
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    fn for_statement(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::For<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let continues: Vec<_> = std::iter::once(AbruptCompletion::Continue(None))
                .chain(this.possible_labeled_continues.clone())
                .collect();
            let ast::statement::For {
                init,
                test,
                update,
                body,
                comments: _,
            } = stmt;
            if let Some(init) = init {
                this.for_statement_init(init)?;
            }
            if let Some(test) = test {
                this.expression(test)?;
            }
            let env_after_test = this.init_env.clone();
            let loop_completion_state = this.run_to_completion(|this2| this2.statement(body));
            let loop_completion_state = this.run_to_completion(|this2| {
                this2.commit_abrupt_completion_matching(
                    |ac| AbruptCompletion::mem(&continues, ac),
                    &loop_completion_state,
                )
            });
            match loop_completion_state {
                None => {
                    if let Some(update) = update {
                        this.expression(update)?;
                    }
                }
                _ => {}
            }
            // Post-for = post-test env (test always runs at least once)
            this.reset_env(&env_after_test);
            let for_completion_states = (None, vec![loop_completion_state]);
            let completion_state = this.run_to_completion(|_| {
                merge_completion_states(&for_completion_states.0, &for_completion_states.1)
            });
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    fn for_in_statement(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::ForIn<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let continues: Vec<_> = std::iter::once(AbruptCompletion::Continue(None))
                .chain(this.possible_labeled_continues.clone())
                .collect();
            let ast::statement::ForIn {
                left,
                right,
                body,
                each: _,
                comments: _,
            } = stmt;
            this.expression(right)?;
            let env0 = this.init_env.clone();
            this.for_in_statement_lhs(left)?;
            let loop_completion_state = this.run_to_completion(|this2| this2.statement(body));
            let loop_completion_state = this.run_to_completion(|this2| {
                this2.commit_abrupt_completion_matching(
                    |ac| AbruptCompletion::mem(&continues, ac),
                    &loop_completion_state,
                )
            });
            this.reset_env(&env0);
            let for_in_completion_states = (None, vec![loop_completion_state]);
            let completion_state = this.run_to_completion(|_| {
                merge_completion_states(&for_in_completion_states.0, &for_in_completion_states.1)
            });
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    fn for_of_statement(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::ForOf<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let continues: Vec<_> = std::iter::once(AbruptCompletion::Continue(None))
                .chain(this.possible_labeled_continues.clone())
                .collect();
            let ast::statement::ForOf {
                left,
                right,
                body,
                await_: _,
                comments: _,
            } = stmt;
            this.expression(right)?;
            let env0 = this.init_env.clone();
            this.for_of_statement_lhs(left)?;
            let loop_completion_state = this.run_to_completion(|this2| this2.statement(body));
            let loop_completion_state = this.run_to_completion(|this2| {
                this2.commit_abrupt_completion_matching(
                    |ac| AbruptCompletion::mem(&continues, ac),
                    &loop_completion_state,
                )
            });
            this.reset_env(&env0);
            let for_of_completion_states = (None, vec![loop_completion_state]);
            let completion_state = this.run_to_completion(|_| {
                merge_completion_states(&for_of_completion_states.0, &for_of_completion_states.1)
            });
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    fn switch(
        &mut self,
        _loc: &'ast ALoc,
        switch: &'ast ast::statement::Switch<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::statement::Switch {
            discriminant,
            cases,
            comments: _,
            exhaustive_out: _,
        } = switch;
        self.expression(discriminant)?;
        self.expecting_abrupt_completions(|this| {
            let initial_env = unreachable_env(&this.init_env);
            // List.fold_left (fun acc stuff -> ... this#handle_switch_case acc case)
            let mut acc = (initial_env, vec![]);
            for case in cases.iter() {
                acc = this.handle_switch_case(acc.0, acc.1, case)?;
            }
            let (env, case_completion_states) = acc;
            this.merge_self_env(&env);
            let switch_completion_states = (None, case_completion_states);
            let completion_state = this.run_to_completion(|_| {
                merge_completion_states(&switch_completion_states.0, &switch_completion_states.1)
            });
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    fn try_catch(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::Try<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let ast::statement::Try {
                block: (block_loc, block),
                handler,
                finalizer,
                comments: _,
            } = stmt;
            let pre_env = this.init_env.clone();
            let try_completion_state =
                this.run_to_completion(|this2| this2.block(block_loc, block));
            let env1 = this.init_env.clone();
            let (catch_completion_state_opt, env2) = match handler {
                Some(clause) => {
                    // In the catch block, an exception could have been thrown at any point
                    // in the try block. Conservatively, reset to pre-try env: properties
                    // initialized before the try are still initialized, but properties
                    // initialized during the try might not be.
                    this.reset_env(&pre_env);
                    let catch_completion_state =
                        this.run_to_completion(|this2| this2.catch_clause(clause));
                    (vec![catch_completion_state], this.init_env.clone())
                }
                None => (vec![], unreachable_env(&this.init_env)),
            };
            this.init_env = merge_envs(&env1, &env2);
            let try_catch_completion_states = (try_completion_state, catch_completion_state_opt);
            let completion_state = this.run_to_completion(|_| {
                merge_completion_states(
                    &try_catch_completion_states.0,
                    &try_catch_completion_states.1,
                )
            });
            this.commit_abrupt_completion_matching(AbruptCompletion::all, &completion_state)?;
            match finalizer {
                Some((_, finalizer_block)) => {
                    // Finally block runs regardless; conservatively reset to pre-try env
                    this.reset_env(&pre_env);
                    this.block(block_loc, finalizer_block)?;
                }
                None => {}
            }
            from_completion(completion_state)
        })
    }

    fn logical(
        &mut self,
        _loc: &'ast ALoc,
        expr: &'ast ast::expression::Logical<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::expression::Logical {
            operator: _,
            left,
            right,
            comments: _,
        } = expr;
        self.expression(left)?;
        let env1 = self.init_env.clone();
        self.expression(right)?;
        self.merge_self_env(&env1);
        Ok(())
    }

    fn conditional(
        &mut self,
        _loc: &'ast ALoc,
        expr: &'ast ast::expression::Conditional<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::expression::Conditional {
            test,
            consequent,
            alternate,
            comments: _,
        } = expr;
        self.expression(test)?;
        let env0 = self.init_env.clone();
        self.expression(consequent)?;
        let env1 = self.init_env.clone();
        self.reset_env(&env0);
        self.expression(alternate)?;
        self.merge_self_env(&env1);
        Ok(())
    }

    // ABRUPT COMPLETION RAISES
    fn break_(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::Break<ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::statement::Break { label, comments: _ } = stmt;
        self.raise_abrupt_completion(AbruptCompletion::break_(label.as_ref()))
    }

    fn continue_(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::Continue<ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::statement::Continue { label, comments: _ } = stmt;
        self.raise_abrupt_completion(AbruptCompletion::continue_(label.as_ref()))
    }

    fn return_(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::Return<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::statement::Return {
            argument,
            comments: _,
            return_out: _,
        } = stmt;
        if let Some(argument) = argument {
            self.expression(argument)?;
        }
        self.raise_abrupt_completion(AbruptCompletion::return_())
    }

    fn throw(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::Throw<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::statement::Throw {
            argument,
            comments: _,
        } = stmt;
        self.expression(argument)?;
        self.raise_abrupt_completion(AbruptCompletion::throw())
    }

    // LABELED STATEMENTS
    fn labeled_statement(
        &mut self,
        _loc: &'ast ALoc,
        stmt: &'ast ast::statement::Labeled<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let ast::statement::Labeled {
                label,
                body,
                comments: _,
            } = stmt;
            this.possible_labeled_continues
                .push(AbruptCompletion::continue_(Some(label)));
            let completion_state = this.run_to_completion(|this2| this2.statement(body));
            this.possible_labeled_continues.clear();
            this.commit_abrupt_completion_matching(
                |ac| AbruptCompletion::mem(&[AbruptCompletion::break_(Some(label))], ac),
                &completion_state,
            )
        })
    }

    fn statement(
        &mut self,
        stmt: &'ast ast::statement::Statement<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        match stmt.deref() {
            StatementInner::While { .. }
            | StatementInner::DoWhile { .. }
            | StatementInner::For { .. }
            | StatementInner::ForIn { .. }
            | StatementInner::ForOf { .. }
            | StatementInner::Labeled { .. } => {}
            _ => self.possible_labeled_continues.clear(),
        }
        ast_visitor::statement_default(self, stmt)
    }

    fn function_(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.visit_lambda(|this| ast_visitor::function_default(this, loc, expr));
        Ok(())
    }

    fn class_expression(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast ast::class::Class<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.visit_lambda(|this| ast_visitor::class_expression_default(this, loc, expr));
        Ok(())
    }
}

pub fn eval_property_assignment(
    class_body: &[ast::class::BodyElement<ALoc, ALoc>],
) -> Errors<ALoc> {
    let property_declarations: Vec<(
        (&ALoc, ast::Identifier<ALoc, ALoc>),
        &ast::class::property::Value<ALoc, ALoc>,
    )> = class_body
        .iter()
        .filter_map(|element| match element {
            ast::class::BodyElement::Property(prop)
                if !prop.static_
                    && matches!(&prop.key, ast::expression::object::Key::Identifier(_)) =>
            {
                if let ast::expression::object::Key::Identifier(id) = &prop.key {
                    Some((
                        public_property(ast_utils::loc_of_ident(id), id),
                        &prop.value,
                    ))
                } else {
                    None
                }
            }
            ast::class::BodyElement::PrivateField(field) if !field.static_ => {
                Some((private_property(&field.key.loc, &field.key), &field.value))
            }
            _ => None,
        })
        .collect();
    let ctor_body: Option<&ast::statement::Block<ALoc, ALoc>> =
        class_body.iter().find_map(|element| match element {
            ast::class::BodyElement::Method(method)
                if matches!(method.kind, ast::class::MethodKind::Constructor) =>
            {
                let (_, func) = &method.value;
                match &func.body {
                    ast::function::Body::BodyBlock((_, block)) => Some(block),
                    _ => None,
                }
            }
            _ => None,
        });
    let default_block = ast::statement::Block {
        body: std::sync::Arc::from([]),
        comments: None,
    };
    let ctor_body = ctor_body.unwrap_or(&default_block);

    let properties: Vec<&ast::Identifier<ALoc, ALoc>> = property_declarations
        .iter()
        .map(|((_, id), _)| id)
        .collect();

    let property_names: BTreeSet<FlowSmolStr> = properties
        .iter()
        .map(|id| ast_utils::name_of_ident(id).dupe())
        .collect();

    let mut walk = PropertyAssignment::new(&property_names, &properties);

    let none_loc = ALoc::default();
    walk.expecting_return_or_throw(|walk| {
        for ((_, property_id), value) in property_declarations.iter() {
            match value {
                ast::class::property::Value::Initialized(default_initializer) => {
                    walk.initialize_property(property_id, default_initializer)?;
                }
                _ => {}
            }
        }
        walk.block(&none_loc, ctor_body)?;
        Ok(())
    });

    let uninitialized_properties: Vec<(Error<ALoc>, FlowSmolStr)> = properties
        .iter()
        .filter(|id| {
            let name = ast_utils::name_of_ident(id);
            !walk.init_env().get(name).copied().unwrap_or(false)
        })
        .map(|id| {
            (
                Error {
                    loc: ast_utils::loc_of_ident(id).dupe(),
                    desc: PropertyAssignmentKind::PropertyNotDefinitelyInitialized,
                },
                ast_utils::name_of_ident(id).dupe(),
            )
        })
        .collect();

    let read_before_initialized: Vec<(Error<ALoc>, FlowSmolStr)> = walk
        .read_errors()
        .iter()
        .map(|(read_loc, name)| {
            (
                Error {
                    loc: read_loc.dupe(),
                    desc: PropertyAssignmentKind::ReadFromUninitializedProperty,
                },
                name.dupe(),
            )
        })
        .collect();

    let filter_uninitialized_in_env = |env: &InitEnv| -> Vec<&&ast::Identifier<ALoc, ALoc>> {
        properties
            .iter()
            .filter(|id| {
                let name = ast_utils::name_of_ident(id);
                matches!(env.get(name), Some(false))
            })
            .collect()
    };

    let this_errors: Vec<(Error<ALoc>, Vec1<FlowSmolStr>)> = walk
        .this_escape_errors()
        .iter()
        .filter_map(|(loc, desc, env)| {
            let uninitialized: Vec<FlowSmolStr> = filter_uninitialized_in_env(env)
                .iter()
                .map(|id| ast_utils::name_of_ident(id).dupe())
                .collect();
            Vec1::try_from_vec(uninitialized).ok().map(|uninit_props| {
                (
                    Error {
                        loc: loc.dupe(),
                        desc: desc.clone(),
                    },
                    uninit_props,
                )
            })
        })
        .collect();

    let add_to_errors =
        |errors: &mut Errors<ALoc>, error: Error<ALoc>, prefixed_name: &FlowSmolStr| {
            let prefixed_name_str: &str = prefixed_name.as_str();
            if let Some(stripped) = prefixed_name_str.strip_prefix("this.#") {
                let key = FlowSmolStr::from(stripped);
                errors
                    .private_property_errors
                    .entry(key)
                    .or_default()
                    .push(error);
            } else if let Some(stripped) = prefixed_name_str.strip_prefix("this.") {
                let key = FlowSmolStr::from(stripped);
                errors
                    .public_property_errors
                    .entry(key)
                    .or_default()
                    .push(error);
            }
        };

    let mut errors = Errors::default();
    for (error, prefixed_name) in uninitialized_properties {
        add_to_errors(&mut errors, error, &prefixed_name);
    }
    for (error, prefixed_name) in read_before_initialized {
        add_to_errors(&mut errors, error, &prefixed_name);
    }
    for (error, names) in this_errors {
        for name in names.iter() {
            add_to_errors(&mut errors, error.clone(), name);
        }
    }
    errors
}
