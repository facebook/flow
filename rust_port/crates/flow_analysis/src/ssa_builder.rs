/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! SSA (Static Single Assignment) builder module.
//!
//! This module is responsible for building a mapping from variable reads to the
//! writes those reads. This is used in type checking to determine if a variable is
//! const-like, but the name_resolver is used to build the type checking environment.
//! The name_resolver copied much of the implementation here, but with sufficient divergence
//! to warrant forking the implementation.
//! If you're here to add support for a new syntax feature, you'll likely
//! need to modify the name_resolver as well, but not necessarily with identical changes.

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::hash::Hash;
use std::ops::Deref;
use std::rc::Rc;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_common::reason::Name;
use flow_common::reason::VirtualReason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::mk_reason;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast_utils;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;

use crate::bindings::Bindings;
use crate::hoister::Hoister;
use crate::hoister::LexicalHoister;
use crate::scope_builder;
use crate::scope_builder::WithBindings;
use crate::ssa_api::Values;
use crate::ssa_api::WriteLoc;

/// For every read of a variable x, we are interested in tracking writes to x
/// that can reach that read. Ultimately the writes are going to be represented
/// as a list of locations, where each location corresponds to a "single static
/// assignment" of the variable in the code. But for the purposes of analysis, it
/// is useful to represent these writes with a data type that contains either a
/// single write, or a "join" of writes (in compiler terminology, a PHI node), or
/// a reference to something that is unknown at a particular point in the AST
/// during traversal, but will be known by the time traversal is complete.
mod val {
    use std::sync::atomic::AtomicU32;
    use std::sync::atomic::Ordering;

    use dupe::IterDupedExt;

    use super::*;

    static CURR_ID: AtomicU32 = AtomicU32::new(0);

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    enum RefState<L: Dupe> {
        /// Different unresolved vars are distinguished by their ids, which enables using
        /// structural equality for computing normal forms.
        Unresolved(u32),
        Resolved(WriteState<L>),
    }

    #[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
    enum WriteState<L: Dupe> {
        Uninitialized,
        Loc(VirtualReason<L>),
        Phi(Rc<[WriteState<L>]>),
        Ref(Rc<RefCell<RefState<L>>>),
    }

    #[derive(Debug, Clone, Dupe)]
    pub(super) struct Val<L: Dupe> {
        id: u32,
        write_state: WriteState<L>,
    }

    pub(super) fn new_id() -> u32 {
        CURR_ID.fetch_add(1, Ordering::Relaxed)
    }

    impl<L: Dupe + Clone + Ord> Val<L> {
        fn mk_with_write_state(write_state: WriteState<L>) -> Self {
            let id = new_id();
            Self { id, write_state }
        }

        pub(super) fn mk_unresolved(ref_id: u32) -> Self {
            Self::mk_with_write_state(WriteState::Ref(Rc::new(RefCell::new(
                RefState::Unresolved(ref_id),
            ))))
        }

        pub(super) fn empty() -> Self {
            Self::mk_with_write_state(WriteState::Phi(Rc::from([])))
        }

        pub(super) fn uninitialized() -> Self {
            Self::mk_with_write_state(WriteState::Uninitialized)
        }

        fn join<'a>(states: impl IntoIterator<Item = &'a WriteState<L>>) -> WriteState<L>
        where
            L: 'a,
        {
            let mut iter = states.into_iter();
            let Some(first) = iter.next() else {
                return WriteState::Phi(Rc::from([]));
            };
            if let Some(second) = iter.next() {
                let mut states = vec![first.dupe(), second.dupe()];
                states.extend(iter.duped());
                WriteState::Phi(Rc::from(states))
            } else {
                first.dupe()
            }
        }

        fn normalize(t: &WriteState<L>) -> BTreeSet<WriteState<L>> {
            match t {
                WriteState::Uninitialized | WriteState::Loc(_) => {
                    let mut set = BTreeSet::new();
                    set.insert(t.dupe());
                    set
                }
                WriteState::Ref(r) => {
                    let borrowed = r.borrow();
                    match &*borrowed {
                        RefState::Unresolved(_) => {
                            drop(borrowed);
                            let mut set = BTreeSet::new();
                            set.insert(t.dupe());
                            set
                        }
                        RefState::Resolved(resolved) => {
                            let resolved = resolved.dupe();
                            drop(borrowed);
                            let vals = Self::normalize(&resolved);
                            let joined = Self::join(&vals);
                            *r.borrow_mut() = RefState::Resolved(joined);
                            vals
                        }
                    }
                }
                WriteState::Phi(ts) => ts
                    .iter()
                    .flat_map(|t| Self::normalize(t).into_iter())
                    .collect(),
            }
        }

        pub(super) fn merge(t1: &Self, t2: &Self) -> Self {
            if t1.id == t2.id {
                t1.dupe()
            } else {
                // Merging can easily lead to exponential blowup in size of terms if we're not careful.
                // We amortize costs by computing normal forms as sets of "atomic" terms, so that
                // merging would correspond to set union.
                // (Atomic terms include Uninitialized, Loc _, and REF { contents = Unresolved _ }.)
                // Note that normal forms might change over time, as unresolved refs become resolved;
                // thus, we do not shortcut normalization of previously normalized terms.
                let vals1 = Self::normalize(&t1.write_state);
                let vals2 = Self::normalize(&t2.write_state);
                Self::mk_with_write_state(Self::join(vals1.union(&vals2)))
            }
        }

        pub(super) fn one(reason: VirtualReason<L>) -> Self {
            Self::mk_with_write_state(WriteState::Loc(reason))
        }

        pub(super) fn all(locs: impl IntoIterator<Item = VirtualReason<L>>) -> Self {
            let states: Vec<_> = locs.into_iter().map(WriteState::Loc).collect();
            Self::mk_with_write_state(Self::join(&states))
        }

        /// Resolving unresolved to t essentially models an equation of the form
        /// unresolved = t, where unresolved is a reference to an unknown and t is the
        /// known. Since the only non-trivial operation in t is joining, it is OK to
        /// erase any occurrences of unresolved in t: if t = unresolved | t' then
        /// unresolved = t is the same as unresolved = t'.
        pub(super) fn resolve(unresolved: &Self, t: &Self) {
            if let WriteState::Ref(r) = &unresolved.write_state {
                let r_borrowed = r.borrow();
                if let RefState::Unresolved(_) = *r_borrowed {
                    drop(r_borrowed);
                    let erased = Self::erase(r, &t.write_state);
                    *r.borrow_mut() = RefState::Resolved(erased);
                    return;
                }
            }
            panic!("Only an unresolved REF can be resolved")
        }

        fn erase(r: &Rc<RefCell<RefState<L>>>, t: &WriteState<L>) -> WriteState<L> {
            match t {
                WriteState::Uninitialized | WriteState::Loc(_) => t.dupe(),
                WriteState::Phi(ts) => {
                    let erased: Vec<_> = ts.iter().map(|t| Self::erase(r, t)).collect();
                    WriteState::Phi(Rc::from(erased))
                }
                WriteState::Ref(r2) => {
                    if Rc::ptr_eq(r, r2) {
                        WriteState::Phi(Rc::from([]))
                    } else {
                        let borrowed = r2.borrow();
                        match &*borrowed {
                            RefState::Unresolved(_) => t.dupe(),
                            RefState::Resolved(resolved) => {
                                let erased = Self::erase(r, resolved);
                                drop(borrowed);
                                *r2.borrow_mut() = RefState::Resolved(erased);
                                t.dupe()
                            }
                        }
                    }
                }
            }
        }

        /// Simplify a Val to a list of WriteLoc.
        pub(super) fn simplify(&self) -> Vec<WriteLoc<L>>
        where
            L: Dupe + Clone + Eq + Hash,
        {
            let vals = Self::normalize(&self.write_state);
            vals.into_iter()
                .map(|ws| match ws {
                    WriteState::Uninitialized => WriteLoc::Uninitialized,
                    WriteState::Loc(r) => WriteLoc::Write(r),
                    WriteState::Ref(r) => {
                        let borrowed = r.borrow();
                        match &*borrowed {
                            RefState::Unresolved(_) => {
                                panic!("An unresolved REF cannot be simplified")
                            }
                            RefState::Resolved(_) => {
                                panic!("A normalized value cannot be a resolved REF")
                            }
                        }
                    }
                    WriteState::Phi(_) => panic!("A normalized value cannot be a PHI"),
                })
                .collect()
        }
    }
}

use val::Val;

/// An environment is a map from variables to values.
type Env<L> = BTreeMap<FlowSmolStr, Val<L>>;

/// Abrupt completions induce control flows, so modeling them accurately is
/// necessary for soundness.
#[derive(Debug, Clone, Dupe, PartialEq, Eq)]
pub enum AbruptCompletion {
    Break(Option<FlowSmolStr>),
    Continue(Option<FlowSmolStr>),
    Return,
    Throw,
}

impl AbruptCompletion {
    pub fn break_(label: Option<&ast::Identifier<impl Dupe, impl Dupe>>) -> Self {
        AbruptCompletion::Break(label.map(|id| id.name.dupe()))
    }

    pub fn continue_(label: Option<&ast::Identifier<impl Dupe, impl Dupe>>) -> Self {
        AbruptCompletion::Continue(label.map(|id| id.name.dupe()))
    }

    pub fn return_() -> Self {
        AbruptCompletion::Return
    }

    pub fn throw() -> Self {
        AbruptCompletion::Throw
    }

    /// Match particular abrupt completions.
    pub fn mem(list: &[AbruptCompletion], t: &AbruptCompletion) -> bool {
        list.contains(t)
    }

    /// Match all abrupt completions.
    pub fn all(_t: &AbruptCompletion) -> bool {
        true
    }
}

/// An abrupt completion carries an environment, which is the current
/// environment at the point where the abrupt completion is "raised." This
/// environment is merged wherever the abrupt completion is "handled."
type AbruptCompletionEnv<L> = (AbruptCompletion, Env<L>);

/// Collect all values assigned to a variable, as a conservative fallback when we
/// don't have precise information.
#[derive(Debug, Clone, Dupe)]
struct Havoc<L: Dupe> {
    unresolved: Val<L>,
    /// always Ref
    locs: Rc<RefCell<Vec<VirtualReason<L>>>>,
}

/// SSA state for a variable.
/// NOTE: val_ref uses Rc<RefCell<...>> to match OCaml's `Val.t ref` semantics -
/// when ssa_env is cloned, all copies share the same underlying Val cell.
/// This is critical for correct behavior in push_ssa_env/pop_ssa_env.
#[derive(Debug, Clone)]
struct Ssa<L: Dupe> {
    val_ref: Rc<RefCell<Val<L>>>,
    havoc: Havoc<L>,
}

pub struct SsaBuilder<Loc: Dupe + Clone + Eq + Ord + Hash + Default> {
    enable_enums: bool,
    /// We maintain a map of read locations to raw Val.t terms, which are
    /// simplified to lists of write locations once the analysis is done.
    values: BTreeMap<Loc, Val<Loc>>,
    unbound_names: BTreeSet<FlowSmolStr>,
    unresolved_ref_id_counter: u32,
    /// Utils to manipulate single-static-assignment (SSA) environments.
    ///
    /// TODO: These low-level operations should probably be replaced by
    /// higher-level "control-flow-graph" operations that can be implemented using
    /// them, e.g., those that deal with branches and loops.
    ssa_env: FlowOrdMap<FlowSmolStr, Ssa<Loc>>,
    /// List of (abrupt_completion, environment) pairs.When an abrupt completion is raised, it falls through any subsequent
    /// straight-line code, until it reaches a merge point in the control-flow
    /// graph. At that point, it can be re-raised if and only if all other reaching
    /// control-flow paths also raise the same abrupt completion.
    ///
    /// When re-raising is not possible, we have to save the abrupt completion and
    /// the current environment in a list, so that we can merge such environments
    /// later (when that abrupt completion and others like it are handled).

    /// Even when raising is possible, we still have to save the current
    /// environment, since the current environment will have to be cleared to model
    /// that the current values of all variables are unreachable.
    ///
    /// NOTE that raising is purely an optimization: we can have more precise
    /// results with raising, but even if we never raised we'd still be sound.
    abrupt_completion_envs: Vec<AbruptCompletionEnv<Loc>>,
    /// Track the list of labels that might describe a loop. Used to detect which
    /// labeled continues need to be handled by the loop.
    ///
    /// The idea is that a labeled statement adds its label to the list before
    /// entering its child, and if the child is not a loop or another labeled
    /// statement, the list will be cleared. A loop will consume the list, so we
    /// also clear the list on our way out of any labeled statement.
    possible_labeled_continues: Vec<AbruptCompletion>,
}

impl<Loc: Dupe + Clone + Eq + Ord + Hash + Default> SsaBuilder<Loc> {
    fn new(enable_enums: bool) -> Self {
        Self {
            enable_enums,
            unresolved_ref_id_counter: 0,
            values: BTreeMap::new(),
            unbound_names: BTreeSet::new(),
            ssa_env: FlowOrdMap::new(),
            abrupt_completion_envs: Vec::new(),
            possible_labeled_continues: Vec::new(),
        }
    }

    fn values(&self) -> Values<Loc> {
        let simplified: BTreeMap<_, _> = self
            .values
            .iter()
            .map(|(k, v)| (k.dupe(), v.simplify()))
            .collect();
        Values(simplified)
    }

    fn mk_unresolved(&mut self) -> Val<Loc> {
        self.unresolved_ref_id_counter += 1;
        Val::mk_unresolved(self.unresolved_ref_id_counter)
    }

    fn ssa_env(&self) -> Env<Loc> {
        self.ssa_env
            .iter()
            .map(|(k, ssa)| (k.dupe(), ssa.val_ref.borrow().dupe()))
            .collect()
    }

    fn merge_remote_ssa_env(&self, env: &Env<Loc>) {
        // NOTE: env might have more keys than ssa_env, since the environment it
        // describes might be nested inside the current environment
        for (x, ssa) in &self.ssa_env {
            let remote_val = env.get(x).unwrap();
            let current = ssa.val_ref.borrow().dupe();
            *ssa.val_ref.borrow_mut() = Val::merge(&current, remote_val);
        }
    }

    fn merge_ssa_env(&self, env1: &Env<Loc>, env2: &Env<Loc>) {
        for (key, ssa) in &self.ssa_env {
            let val1 = env1.get(key).unwrap();
            let val2 = env2.get(key).unwrap();
            *ssa.val_ref.borrow_mut() = Val::merge(val1, val2);
        }
    }

    fn merge_self_ssa_env(&self, env: &Env<Loc>) {
        for (x, ssa) in &self.ssa_env {
            if let Some(env_val) = env.get(x) {
                let current = ssa.val_ref.borrow().clone();
                *ssa.val_ref.borrow_mut() = Val::merge(&current, env_val);
            }
        }
    }

    fn reset_ssa_env(&self, env0: &Env<Loc>) {
        for (x, ssa) in &self.ssa_env {
            if let Some(env_val) = env0.get(x) {
                *ssa.val_ref.borrow_mut() = env_val.clone();
            }
        }
    }

    fn fresh_ssa_env(&mut self) -> Env<Loc> {
        let keys: Vec<_> = self.ssa_env.keys().map(|k| k.dupe()).collect();
        keys.into_iter()
            .map(|k| (k, self.mk_unresolved()))
            .collect()
    }

    fn assert_ssa_env(&mut self, env0: &Env<Loc>) {
        for (x, ssa) in &self.ssa_env {
            if let Some(env_val) = env0.get(x) {
                Val::resolve(env_val, &ssa.val_ref.borrow());
            }
        }
    }

    fn empty_ssa_env(&self) -> Env<Loc> {
        self.ssa_env
            .keys()
            .map(|k| (k.dupe(), Val::empty()))
            .collect()
    }

    fn havoc_current_ssa_env(&self) {
        for ssa in self.ssa_env.values() {
            let current = ssa.val_ref.borrow().clone();
            *ssa.val_ref.borrow_mut() = Val::merge(&current, &ssa.havoc.unresolved);
        }
    }

    fn havoc_uninitialized_ssa_env(&self) {
        for ssa in self.ssa_env.values() {
            let uninit = Val::uninitialized();
            *ssa.val_ref.borrow_mut() = Val::merge(&uninit, &ssa.havoc.unresolved);
        }
    }

    /// Push new bindings onto the SSA environment.
    fn push_ssa_env(
        &mut self,
        bindings: &Bindings<Loc>,
    ) -> (Vec<FlowSmolStr>, Vec<(FlowSmolStr, Option<Ssa<Loc>>)>) {
        let binding_names: Vec<_> = bindings
            .to_assoc()
            .into_iter()
            .map(|(name, _)| name)
            .collect();
        let mut old_bindings = Vec::with_capacity(binding_names.len());
        for name in &binding_names {
            old_bindings.push((name.dupe(), self.ssa_env.get(name).cloned()));
            let ssa = Ssa {
                val_ref: Rc::new(RefCell::new(Val::uninitialized())),
                havoc: Havoc {
                    unresolved: self.mk_unresolved(),
                    locs: Rc::new(RefCell::new(Vec::new())),
                },
            };
            self.ssa_env.insert(name.dupe(), ssa);
        }
        (binding_names, old_bindings)
    }

    fn resolve_havocs(&mut self, binding_names: &[FlowSmolStr]) {
        for x in binding_names {
            if let Some(ssa) = self.ssa_env.get(x) {
                let all_locs = Val::all(ssa.havoc.locs.borrow().iter().duped());
                Val::resolve(&ssa.havoc.unresolved, &all_locs);
            }
        }
    }

    fn pop_ssa_env(
        &mut self,
        binding_names: Vec<FlowSmolStr>,
        old_bindings: Vec<(FlowSmolStr, Option<Ssa<Loc>>)>,
    ) {
        self.resolve_havocs(&binding_names);
        for (name, old_ssa) in old_bindings {
            match old_ssa {
                Some(old_ssa) => {
                    self.ssa_env.insert(name, old_ssa);
                }
                None => {
                    self.ssa_env.remove(&name);
                }
            }
        }
    }

    /// Run a computation, catching any abrupt completions; do some final work,
    /// and then re-raise any abrupt completions that were caught.
    fn run<R>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<R, AbruptCompletion>,
        finally: impl FnOnce(&mut Self),
    ) -> Result<R, AbruptCompletion> {
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

    fn run_to_completion(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<(), AbruptCompletion>,
    ) -> Option<AbruptCompletion> {
        f(self).err()
    }

    fn from_completion(completion: Option<AbruptCompletion>) -> Result<(), AbruptCompletion> {
        match completion {
            None => Ok(()),
            Some(abrupt) => Err(abrupt),
        }
    }

    fn raise_abrupt_completion<R>(
        &mut self,
        abrupt: AbruptCompletion,
    ) -> Result<R, AbruptCompletion> {
        let env = self.ssa_env();
        let empty = self.empty_ssa_env();
        self.reset_ssa_env(&empty);
        self.abrupt_completion_envs.push((abrupt.dupe(), env));
        Err(abrupt)
    }

    fn expecting_abrupt_completions<R>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<R, AbruptCompletion>,
    ) -> Result<R, AbruptCompletion> {
        let saved = std::mem::take(&mut self.abrupt_completion_envs);
        let result = f(self);
        let current = std::mem::take(&mut self.abrupt_completion_envs);
        self.abrupt_completion_envs = saved;
        self.abrupt_completion_envs.extend(current);
        result
    }

    /// Given multiple completion states, (re)raise if all of them are the same
    /// abrupt completion. This function is called at merge points.
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

    /// Given a filter for particular abrupt completions to expect, find the saved
    /// environments corresponding to them, and merge those environments with the
    /// current environment. This function is called when exiting ASTs that
    /// introduce (and therefore expect) particular abrupt completions.
    fn commit_abrupt_completion_matching(
        &mut self,
        filter: impl Fn(&AbruptCompletion) -> bool,
        completion_state: &Option<AbruptCompletion>,
    ) -> Result<(), AbruptCompletion> {
        let (matching, non_matching): (Vec<_>, Vec<_>) = self
            .abrupt_completion_envs
            .drain(..)
            .partition(|(abrupt, _)| filter(abrupt));

        if !matching.is_empty() {
            for (_, env) in matching {
                self.merge_remote_ssa_env(&env);
            }
            self.abrupt_completion_envs = non_matching;
            Ok(())
        } else {
            self.abrupt_completion_envs = non_matching;
            match completion_state {
                Some(abrupt) if !filter(abrupt) => Err(abrupt.clone()),
                _ => Ok(()),
            }
        }
    }

    // read
    fn any_identifier(&mut self, loc: Loc, name: &FlowSmolStr) {
        match self.ssa_env.get(name) {
            Some(ssa) => {
                self.values.insert(loc, ssa.val_ref.borrow().clone());
            }
            None => {
                self.unbound_names.insert(name.dupe());
            }
        }
    }

    fn scoped_for_statement(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::For<Loc, Loc>,
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
            let env1 = this.fresh_ssa_env();
            this.merge_self_ssa_env(&env1);
            if let Some(test) = test {
                this.expression(test)?;
            }
            let env2 = this.ssa_env();
            let loop_completion_state = this.run_to_completion(|this2| this2.statement(body));
            // Continue
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
            this.assert_ssa_env(&env1);
            this.reset_ssa_env(&env2);
            let for_completion_states = (None, vec![loop_completion_state]);
            let completion_state = this.run_to_completion(|_| {
                Self::merge_completion_states(&for_completion_states.0, &for_completion_states.1)
            });
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    /// Scoped for-in statement handler.
    fn scoped_for_in_statement(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::ForIn<Loc, Loc>,
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
            let env1 = this.fresh_ssa_env();
            this.merge_self_ssa_env(&env1);
            let env2 = this.ssa_env();
            this.for_in_statement_lhs(left)?;
            let loop_completion_state = this.run_to_completion(|this2| this2.statement(body));
            // Continue
            let loop_completion_state = this.run_to_completion(|this2| {
                this2.commit_abrupt_completion_matching(
                    |ac| AbruptCompletion::mem(&continues, ac),
                    &loop_completion_state,
                )
            });
            this.assert_ssa_env(&env1);
            this.reset_ssa_env(&env2);
            let for_in_completion_states = (None, vec![loop_completion_state]);
            let completion_state = this.run_to_completion(|_| {
                Self::merge_completion_states(
                    &for_in_completion_states.0,
                    &for_in_completion_states.1,
                )
            });
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    fn scoped_for_of_statement(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::ForOf<Loc, Loc>,
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
            let env1 = this.fresh_ssa_env();
            this.merge_self_ssa_env(&env1);
            let env2 = this.ssa_env();
            this.for_of_statement_lhs(left)?;
            let loop_completion_state = this.run_to_completion(|this2| this2.statement(body));
            // Continue
            let loop_completion_state = this.run_to_completion(|this2| {
                this2.commit_abrupt_completion_matching(
                    |ac| AbruptCompletion::mem(&continues, ac),
                    &loop_completion_state,
                )
            });
            this.assert_ssa_env(&env1);
            this.reset_ssa_env(&env2);
            let for_of_completion_states = (None, vec![loop_completion_state]);
            let completion_state = this.run_to_completion(|_| {
                Self::merge_completion_states(
                    &for_of_completion_states.0,
                    &for_of_completion_states.1,
                )
            });
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    fn for_in_or_of_left_declaration(
        &mut self,
        left: &(Loc, ast::statement::VariableDeclaration<Loc, Loc>),
    ) -> Result<(), AbruptCompletion> {
        let (_loc, decl) = left;
        let ast::statement::VariableDeclaration {
            declarations,
            kind,
            comments: _,
        } = decl;
        if let [declarator] = &declarations[..] {
            let id = &declarator.id;
            match id {
                ast::pattern::Pattern::Identifier { .. }
                | ast::pattern::Pattern::Object { .. }
                | ast::pattern::Pattern::Array { .. } => {
                    self.variable_declarator_pattern(*kind, id)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Handle switch cases (SSA-specific implementation).
    fn switch_cases(
        &mut self,
        _loc: &Loc,
        _discriminant: &ast::expression::Expression<Loc, Loc>,
        cases: &[ast::statement::switch::Case<Loc, Loc>],
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let mut env = this.empty_ssa_env();
            let mut case_completion_states: Vec<Option<AbruptCompletion>> = Vec::new();
            for case in cases {
                let (new_env, completion_state) = this.ssa_switch_case(&env, case);
                env = new_env;
                case_completion_states.push(completion_state);
            }

            this.merge_self_ssa_env(&env);

            // In general, cases are non-exhaustive
            let switch_completion_states = (None, case_completion_states);
            let completion_state = this.run_to_completion(|_| {
                Self::merge_completion_states(
                    &switch_completion_states.0,
                    &switch_completion_states.1,
                )
            });

            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    fn ssa_switch_case(
        &mut self,
        env: &Env<Loc>,
        case: &ast::statement::switch::Case<Loc, Loc>,
    ) -> (Env<Loc>, Option<AbruptCompletion>) {
        let ast::statement::switch::Case {
            test,
            case_test_loc: _,
            consequent,
            comments: _,
            loc: _,
        } = case;
        if let Some(test_expr) = test {
            let _ = self.expression(test_expr);
        }
        let env0 = self.ssa_env();
        self.merge_ssa_env(&env0, env);
        let case_completion_state = self.run_to_completion(|this| this.statement_list(consequent));
        let env_new = self.ssa_env();
        self.reset_ssa_env(&env0);
        (env_new, case_completion_state)
    }

    // We also havoc state when entering functions and exiting calls.
    fn lambda(
        &mut self,
        _is_arrow: bool,
        _fun_loc: &Loc,
        _generator_return_loc: Option<&Loc>,
        params: &ast::function::Params<Loc, Loc>,
        return_: &ast::function::ReturnAnnot<Loc, Loc>,
        predicate: Option<&ast::types::Predicate<Loc, Loc>>,
        body: &ast::function::Body<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let enable_enums = self.enable_enums;
        self.expecting_abrupt_completions(|this| {
            let env = this.ssa_env();
            this.run(
                |this2| {
                    this2.havoc_uninitialized_ssa_env();
                    let completion_state = this2.run_to_completion(|this3| {
                        scope_builder::lambda(
                            this3,
                            enable_enums,
                            true, // with_types
                            _is_arrow,
                            _fun_loc,
                            _generator_return_loc,
                            params,
                            return_,
                            predicate,
                            body,
                        )
                    });
                    this2.commit_abrupt_completion_matching(
                        |ac| matches!(ac, AbruptCompletion::Return | AbruptCompletion::Throw),
                        &completion_state,
                    )
                },
                |this2| this2.reset_ssa_env(&env),
            )
        })
    }

    // We also havoc state when entering components
    fn component_body_with_params(
        &mut self,
        body: &(Loc, ast::statement::Block<Loc, Loc>),
        params: &ast::statement::component_params::Params<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let enable_enums = self.enable_enums;
        self.expecting_abrupt_completions(|this| {
            let env = this.ssa_env();
            this.run(
                |this2| {
                    this2.havoc_uninitialized_ssa_env();
                    let completion_state = this2.run_to_completion(|this3| {
                        scope_builder::component_body_with_params(
                            this3,
                            enable_enums,
                            true, // with_types
                            body,
                            params,
                        )
                    });
                    this2.commit_abrupt_completion_matching(
                        |ac| matches!(ac, AbruptCompletion::Return | AbruptCompletion::Throw),
                        &completion_state,
                    )
                },
                |this2| this2.reset_ssa_env(&env),
            )
        })
    }

    fn function_expression_without_name_(
        &mut self,
        is_arrow: bool,
        loc: &Loc,
        expr: &ast::function::Function<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let enable_enums = self.enable_enums;
        scope_builder::function_expression_without_name(
            self,
            enable_enums,
            true,
            is_arrow,
            loc,
            expr,
            &|v: &mut Self, f: &mut dyn FnMut(&mut Self) -> Result<(), AbruptCompletion>| {
                v.hoist_annotations(|v2| f(v2))
            },
            |v: &mut Self, loc: &Loc, has_this: bool, id: Option<&ast::Identifier<Loc, Loc>>| {
                v.this_binding_function_id_opt(loc, has_this, id)
            },
            |v: &mut Self,
             _enable_enums_inner: bool,
             _with_types: bool,
             is_arrow_inner: bool,
             fun_loc: &Loc,
             generator_return_loc: Option<&Loc>,
             params: &ast::function::Params<Loc, Loc>,
             return_: &ast::function::ReturnAnnot<Loc, Loc>,
             predicate: Option<&ast::types::Predicate<Loc, Loc>>,
             body: &ast::function::Body<Loc, Loc>| {
                v.lambda(
                    is_arrow_inner,
                    fun_loc,
                    generator_return_loc,
                    params,
                    return_,
                    predicate,
                    body,
                )
            },
        )
    }

    fn hoist_annotations(
        &mut self,
        mut f: impl FnMut(&mut Self) -> Result<(), AbruptCompletion>,
    ) -> Result<(), AbruptCompletion> {
        f(self)
    }

    fn this_binding_function_id_opt(
        &mut self,
        _fun_loc: &Loc,
        _has_this_annot: bool,
        ident: Option<&ast::Identifier<Loc, Loc>>,
    ) -> Result<(), AbruptCompletion> {
        if let Some(id) = ident {
            self.function_identifier(id)?;
        }
        Ok(())
    }

    fn class_identifier_opt(
        &mut self,
        _class_loc: &Loc,
        id: Option<&ast::Identifier<Loc, Loc>>,
    ) -> Result<(), AbruptCompletion> {
        if let Some(id) = id {
            self.class_identifier(id)?;
        }
        Ok(())
    }
}

impl<Loc: Dupe + Clone + Eq + Ord + Hash + Default, E> WithBindings<Loc, E> for SsaBuilder<Loc> {
    fn with_bindings<T>(
        &mut self,
        _lexical: bool,
        _loc: Loc,
        bindings: Bindings<Loc>,
        visit: impl FnOnce(&mut Self) -> Result<T, E>,
    ) -> Result<T, E> {
        let (bindings_map, old_ssa_env) = self.push_ssa_env(&bindings);
        let result = visit(self);
        self.pop_ssa_env(bindings_map, old_ssa_env);
        result
    }
}

impl<'ast, Loc: Dupe + Clone + Eq + Ord + Hash + Default>
    AstVisitor<'ast, Loc, Loc, &'ast Loc, AbruptCompletion> for SsaBuilder<Loc>
{
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    // write
    fn pattern_identifier(
        &mut self,
        _kind: Option<ast::VariableKind>,
        ident: &ast::Identifier<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        {
            let this = &mut *self;
            let loc: &Loc = &ident.loc;
            let name: &FlowSmolStr = &ident.name;
            let reason = mk_reason(
                VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                loc.dupe(),
            );
            match this.ssa_env.get_mut(name) {
                Some(ssa) => {
                    *ssa.val_ref.borrow_mut() = Val::one(reason.dupe());
                    ssa.havoc.locs.borrow_mut().push(reason);
                }
                None => {
                    this.unbound_names.insert(name.dupe());
                }
            }
        };
        Ok(())
    }

    fn identifier(&mut self, id: &ast::Identifier<Loc, Loc>) -> Result<(), AbruptCompletion> {
        self.any_identifier(id.loc.dupe(), &id.name);
        Ok(())
    }

    fn untyped_identifier(
        &mut self,
        id: &ast::Identifier<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        self.any_identifier(id.loc.dupe(), &id.name);
        Ok(())
    }

    fn jsx_element_name_identifier(
        &mut self,
        ident: &ast::jsx::Identifier<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        self.any_identifier(ident.loc.dupe(), &ident.name);
        Ok(())
    }

    fn jsx_element_name_namespaced(
        &mut self,
        _ns: &ast::jsx::NamespacedName<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        // TODO: what identifiers does `<foo:bar />` read?
        Ok(())
    }

    // Object property keys that are identifiers should not be tracked as reads
    fn object_key_identifier(
        &mut self,
        _id: &ast::Identifier<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    // don't rename the `foo` in `x.foo`
    fn member_property_identifier(
        &mut self,
        _id: &ast::Identifier<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    // don't rename the `foo` in `typeof x.foo`
    fn typeof_member_identifier(
        &mut self,
        _ident: &ast::Identifier<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    // don't rename the `ComponentType` in `React.ComponentType`
    fn member_type_identifier(
        &mut self,
        _id: &ast::Identifier<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    // don't rename the `foo` in `const {foo: bar} = x`
    fn pattern_object_property_identifier_key(
        &mut self,
        _kind: Option<ast::VariableKind>,
        _id: &ast::Identifier<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn match_object_pattern_property_key(
        &mut self,
        _key: &ast::match_pattern::object_pattern::Key<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn match_object_pattern_property(
        &mut self,
        prop: &ast::match_pattern::object_pattern::Property<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        match prop {
            ast::match_pattern::object_pattern::Property::Valid { .. } => {
                ast_visitor::match_object_pattern_property_default(self, prop)
            }
            ast::match_pattern::object_pattern::Property::InvalidShorthand { .. } => Ok(()),
        }
    }

    // don't rename the `Foo` in `enum E { Foo }`
    fn enum_member_identifier(
        &mut self,
        _id: &ast::Identifier<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn enum_declaration(
        &mut self,
        loc: &Loc,
        enum_decl: &ast::statement::EnumDeclaration<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        if !self.enable_enums {
            return Ok(());
        }
        ast_visitor::enum_declaration_default(self, loc, enum_decl)
    }

    // don't rename the `foo` in `component C(foo: number) {}`
    fn component_param_name(
        &mut self,
        _param_name: &ast::statement::component_params::ParamName<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn assignment(
        &mut self,
        _loc: &Loc,
        expr: &ast::expression::Assignment<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::expression::Assignment {
            operator,
            left,
            right,
            comments: _,
        } = expr;

        let (left, _) = ast_utils::unwrap_nonnull_lhs(left);

        match operator {
            None => {
                match left.as_ref() {
                    ast::pattern::Pattern::Identifier { .. }
                    | ast::pattern::Pattern::Object { .. }
                    | ast::pattern::Pattern::Array { .. } => {
                        // given `x = e`, read e then write x
                        self.expression(right)?;
                        self.assignment_pattern(&left)?;
                    }
                    ast::pattern::Pattern::Expression { .. } => {
                        // given `o.x = e`, read o then read e
                        self.assignment_pattern(&left)?;
                        self.expression(right)?;
                    }
                }
            }
            Some(_) => {
                match left.as_ref() {
                    ast::pattern::Pattern::Identifier { inner, .. } => {
                        // given `x += e`, read x then read e then write x
                        self.identifier(&inner.name)?;
                        self.expression(right)?;
                        self.assignment_pattern(&left)?;
                    }
                    ast::pattern::Pattern::Expression { .. } => {
                        // given `o.x += e`, read o then read e
                        self.assignment_pattern(&left)?;
                        self.expression(right)?;
                    }
                    ast::pattern::Pattern::Object { .. } | ast::pattern::Pattern::Array { .. } => {
                        // Invalid expression, still visit subexpressions
                        self.expression(right)?;
                    }
                }
            }
        }
        Ok(())
    }

    // Order of evaluation matters
    fn variable_declarator(
        &mut self,
        kind: ast::VariableKind,
        decl: &ast::statement::variable::Declarator<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let id = &decl.id;
        let init = &decl.init;

        match id {
            ast::pattern::Pattern::Identifier { inner, .. } => {
                match init {
                    Some(init_expr) => {
                        // given `var x = e`, read e then write x
                        self.expression(init_expr)?;
                        self.variable_declarator_pattern(kind, id)?;
                    }
                    None => {
                        // `var x;` is not a write of `x`, but there might be unbound names in annotation
                        self.type_annotation_hint(&inner.annot)?;
                    }
                }
            }
            ast::pattern::Pattern::Object { inner, .. } => match init {
                Some(init_expr) => {
                    self.expression(init_expr)?;
                    self.variable_declarator_pattern(kind, id)?;
                }
                None => {
                    self.type_annotation_hint(&inner.annot)?;
                }
            },
            ast::pattern::Pattern::Array { inner, .. } => match init {
                Some(init_expr) => {
                    self.expression(init_expr)?;
                    self.variable_declarator_pattern(kind, id)?;
                }
                None => {
                    self.type_annotation_hint(&inner.annot)?;
                }
            },
            ast::pattern::Pattern::Expression { .. } => {
                // Invalid expression, still visit init if present
                if let Some(init_expr) = init {
                    self.expression(init_expr)?;
                }
            }
        }
        Ok(())
    }

    // read and write (when the argument is an identifier)
    fn update_expression(
        &mut self,
        _loc: &Loc,
        expr: &ast::expression::Update<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::expression::Update {
            argument,
            operator: _,
            prefix: _,
            comments: _,
        } = expr;

        match argument.deref() {
            ExpressionInner::Identifier { loc: _, inner } => {
                // given `x++`, read x then write x
                self.identifier(inner)?;
                self.pattern_identifier(None, inner)?;
            }
            _ => {
                // given `o.x++`, read o
                self.expression(argument)?;
            }
        }
        Ok(())
    }

    fn break_(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::Break<Loc>,
    ) -> Result<(), AbruptCompletion> {
        self.raise_abrupt_completion(AbruptCompletion::break_(stmt.label.as_ref()))
    }

    fn continue_(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::Continue<Loc>,
    ) -> Result<(), AbruptCompletion> {
        self.raise_abrupt_completion(AbruptCompletion::continue_(stmt.label.as_ref()))
    }

    fn return_(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::Return<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        if let Some(ref argument) = stmt.argument {
            self.expression(argument)?;
        }
        self.raise_abrupt_completion(AbruptCompletion::return_())
    }

    fn throw(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::Throw<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        self.expression(&stmt.argument)?;
        self.raise_abrupt_completion(AbruptCompletion::throw())
    }

    // Control flow

    // We describe the effect on the environment of evaluating node n using Hoare
    // triples of the form [PRE] n [POST], where PRE is the environment before
    // and POST is the environment after the evaluation of node n. Environments
    // must be joined whenever a node is reachable from multiple nodes, as can
    // happen after a branch or before a loop.

    // Block statements create a new lexical scope for let/const bindings
    fn block(
        &mut self,
        loc: &Loc,
        stmt: &ast::statement::Block<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let mut lexical_hoist = LexicalHoister::new(self.enable_enums);
        let Ok(()) = lexical_hoist.block(loc, stmt);
        let lexical_bindings = lexical_hoist.into_bindings();
        self.with_bindings(true, loc.dupe(), lexical_bindings, |this| {
            ast_visitor::block_default(this, loc, stmt)
        })
    }

    // Function and component bodies bypass the block's lexical hoisting since
    // they have their own scoping handled in lambda/component methods
    fn function_body(
        &mut self,
        body: &(Loc, ast::statement::Block<Loc, Loc>),
    ) -> Result<(), AbruptCompletion> {
        let (loc, block) = body;
        ast_visitor::block_default(self, loc, block)
    }

    fn component_body(
        &mut self,
        body: &(Loc, ast::statement::Block<Loc, Loc>),
    ) -> Result<(), AbruptCompletion> {
        let (loc, block) = body;
        ast_visitor::block_default(self, loc, block)
    }

    // [PRE] if (e) { s1 } else { s2 } [POST]
    //    |
    //    e
    //   / \
    // s1   s2
    //   \./
    //    |
    // [PRE] e [ENV0]
    // [ENV0] s1 [ENV1]
    // [ENV0] s2 [ENV2]
    // POST = ENV1 | ENV2
    fn if_statement(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::If<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::statement::If {
            test,
            consequent,
            alternate,
            comments: _,
        } = stmt;
        self.expression(test)?;
        let env0 = self.ssa_env();

        // collect completions and environments of every branch
        let then_completion_state = self.run_to_completion(|this| {
            this.if_consequent_statement(alternate.is_some(), consequent)
        });
        let env1 = self.ssa_env();
        self.reset_ssa_env(&env0);
        let else_completion_state = self.run_to_completion(|this| {
            if let Some(alt) = alternate {
                this.statement(&alt.body)?;
            }
            Ok(())
        });
        // Merge environments
        self.merge_self_ssa_env(&env1);
        // Merge completions
        Self::merge_completion_states(&then_completion_state, &[else_completion_state])
    }

    fn match_<B>(
        &mut self,
        _loc: &'ast Loc,
        match_expr: &'ast ast::match_::Match<Loc, Loc, B>,
        mut on_case_body: impl FnMut(&mut Self, &'ast B) -> Result<(), AbruptCompletion>,
    ) -> Result<(), AbruptCompletion> {
        let ast::match_::Match {
            arg,
            cases,
            match_keyword_loc: _,
            comments: _,
        } = match_expr;
        self.expression(arg)?;
        let env0 = self.ssa_env();
        let mut env = self.empty_ssa_env();
        let mut completion_states: Vec<Option<AbruptCompletion>> = Vec::new();
        for case in cases.iter() {
            self.reset_ssa_env(&env0);
            let completion_state =
                self.run_to_completion(|this| this.match_case(case, &mut on_case_body));
            self.merge_self_ssa_env(&env);
            env = self.ssa_env();
            completion_states.push(completion_state);
        }
        match &completion_states[..] {
            [] => Ok(()),
            [completion_state] => Self::from_completion(completion_state.dupe()),
            [first, rest @ ..] => Self::merge_completion_states(first, rest),
        }
    }

    fn match_case<B>(
        &mut self,
        case: &'ast ast::match_::Case<Loc, Loc, B>,
        on_case_body: &mut impl FnMut(&mut Self, &'ast B) -> Result<(), AbruptCompletion>,
    ) -> Result<(), AbruptCompletion> {
        let enable_enums = self.enable_enums;
        scope_builder::match_case(self, enable_enums, case, on_case_body)
    }

    // [PRE] while (e) { s } [POST]
    //    |
    //    e <-.
    //   / \ /
    //  |   s
    //   \
    //    |
    // PRE = ENV0
    // [ENV0 | ENV1] e [ENV2]
    // [ENV2] s [ENV1]
    // POST = ENV2
    fn while_(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::While<Loc, Loc>,
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

            // Placeholder for environment at the end of the loop body
            let env1 = this.fresh_ssa_env();
            this.merge_self_ssa_env(&env1);
            this.expression(test)?;
            let env2 = this.ssa_env();

            let loop_completion_state = this.run_to_completion(|this2| this2.statement(body));

            // Continue exits
            let loop_completion_state = this.run_to_completion(|this2| {
                this2.commit_abrupt_completion_matching(
                    |ac| AbruptCompletion::mem(&continues, ac),
                    &loop_completion_state,
                )
            });

            // End of loop body
            this.assert_ssa_env(&env1);

            // Out of the loop - this always happens right after evaluating the loop test
            this.reset_ssa_env(&env2);

            // We might also never enter the loop body
            let while_completion_states = (None, vec![loop_completion_state]);
            let completion_state = this.run_to_completion(|_| {
                Self::merge_completion_states(
                    &while_completion_states.0,
                    &while_completion_states.1,
                )
            });

            // Completion_state = None
            // Break exits
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    // [PRE] do { s } while (e) [POST]
    //    |
    //    s <-.
    //     \ /
    //      e
    //      |
    // PRE = ENV0
    // [ENV0 | ENV1] s; e [ENV1]
    // POST = ENV1
    fn do_while(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::DoWhile<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let continues: Vec<_> = std::iter::once(AbruptCompletion::Continue(None))
                .chain(this.possible_labeled_continues.clone())
                .collect();
            let ast::statement::DoWhile {
                body,
                test,
                comments: _,
            } = stmt;
            let env1 = this.fresh_ssa_env();
            this.merge_self_ssa_env(&env1);
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
            this.assert_ssa_env(&env1);
            let do_while_completion_states = (loop_completion_state, vec![]);
            let completion_state = this.run_to_completion(|_| {
                Self::merge_completion_states(
                    &do_while_completion_states.0,
                    &do_while_completion_states.1,
                )
            });
            // completion_state = loop_completion_state
            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(None)),
                &completion_state,
            )
        })
    }

    // [PRE] for (e; e1; e2) { s } [POST]
    //    |
    //    e
    //    |
    //   e1 <---.
    //   / \    |
    //  |   s   |
    //  |    \ /
    //  |    e2
    //   \
    //    |
    // [PRE] e [ENV0]
    // [ENV0 | ENV1] e1 [ENV2]
    // [ENV2] s; e2 [ENV1]
    // POST = ENV2
    fn for_statement(
        &mut self,
        loc: &Loc,
        stmt: &ast::statement::For<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let enable_enums = self.enable_enums;
        scope_builder::for_statement(self, enable_enums, loc, stmt, Self::scoped_for_statement)
    }

    // [PRE] for (e1 in e2) { s } [POST]
    //    |
    //    e2
    //    |
    //    . <---.
    //   / \    |
    //  |   e1  |
    //  |    \ /
    //  |     s
    //   \
    //    |
    // [PRE] e2 [ENV0]
    // ENV2 = ENV0 | ENV1
    // [ENV2] e2 [ENV0]
    // [ENV0 | ENV1] e1; s [ENV1]
    // POST = ENV2
    fn for_in_statement(
        &mut self,
        loc: &Loc,
        stmt: &ast::statement::ForIn<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let enable_enums = self.enable_enums;
        scope_builder::for_in_statement(
            self,
            enable_enums,
            loc,
            stmt,
            Self::scoped_for_in_statement,
        )
    }

    // [PRE] for (e1 of e2) { s } [POST]
    //    |
    //    e2
    //    |
    //    . <---.
    //   / \    |
    //  |   e1  |
    //  |    \ /
    //  |     s
    //   \
    //    |
    // [PRE] e2 [ENV0]
    // ENV2 = ENV0 | ENV1
    // [ENV2] e2 [ENV0]
    // [ENV0 | ENV1] e1; s [ENV1]
    // POST = ENV2
    fn for_of_statement(
        &mut self,
        loc: &Loc,
        stmt: &ast::statement::ForOf<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let enable_enums = self.enable_enums;
        scope_builder::for_of_statement(
            self,
            enable_enums,
            loc,
            stmt,
            Self::scoped_for_of_statement,
        )
    }

    fn for_in_left_declaration(
        &mut self,
        left: &(Loc, ast::statement::VariableDeclaration<Loc, Loc>),
    ) -> Result<(), AbruptCompletion> {
        self.for_in_or_of_left_declaration(left)
    }

    fn for_of_left_declaration(
        &mut self,
        left: &(Loc, ast::statement::VariableDeclaration<Loc, Loc>),
    ) -> Result<(), AbruptCompletion> {
        self.for_in_or_of_left_declaration(left)
    }

    // [PRE] switch (e) { case e1: s1 ... case eN: sN } [POST]
    //     |
    //     e
    //    /
    //   e1
    //   | \
    //   .  s1
    //   |   |
    //   ei  .
    //   | \ |
    //   .  si
    //   |   |
    //   eN  .
    //   | \ |
    //   |  sN
    //    \  |
    //      \|
    //       |
    // [PRE] e [ENV0]
    // ENV0' = empty
    // \forall i = 0..N-1:
    //   [ENVi] ei+1 [ENVi+1]
    //   [ENVi+1 | ENVi'] si+1 [ENVi+1']
    // POST = ENVN | ENVN'
    fn switch(
        &mut self,
        loc: &Loc,
        switch: &ast::statement::Switch<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::statement::Switch {
            discriminant,
            cases,
            comments: _,
            exhaustive_out: _,
        } = switch;
        self.expression(discriminant)?;
        let mut lexical_hoist = LexicalHoister::new(self.enable_enums);
        for case in cases.iter() {
            let Ok(()) = lexical_hoist.statement_list(&case.consequent);
        }
        let lexical_bindings = lexical_hoist.into_bindings();

        self.with_bindings(true, loc.dupe(), lexical_bindings, |this| {
            this.switch_cases(loc, discriminant, cases)
        })
    }

    fn catch_clause(
        &mut self,
        clause: &ast::statement::try_::CatchClause<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let enable_enums = self.enable_enums;
        scope_builder::catch_clause(self, enable_enums, clause, |this, clause| {
            ast_visitor::catch_clause_default(this, clause)
        })
    }

    // [PRE] try { s1 } catch { s2 } [POST]
    //    |
    //    s1 ..~
    //    |    |
    //    |   s2
    //     \./
    //      |
    // [PRE] s1 [ENV1]
    // [HAVOC] s2 [ENV2 ]
    // POST = ENV1 | ENV2
    //
    // [PRE] try { s1 } catch { s2 } finally { s3 } [POST]
    //    |
    //    s1 ..~
    //    |    |
    //    |   s2 ..~
    //     \./     |
    //      |______|
    //             |
    //            s3
    //             |
    // [PRE] s1 [ENV1]
    // [HAVOC] s2 [ENV2 ]
    // [HAVOC] s3 [ENV3 ]
    // POST = ENV3
    fn try_catch(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::Try<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let ast::statement::Try {
                block,
                handler,
                finalizer,
                comments: _,
            } = stmt;
            let pre_env = this.ssa_env();
            let try_completion_state =
                this.run_to_completion(|this2| this2.block(&block.0, &block.1));
            let env1 = this.ssa_env();
            let (catch_completion_state_opt, env2) = if let Some(handler) = handler {
                // NOTE: Havoc-ing the state when entering the handler is probably
                // overkill. We can be more precise but still correct by collecting all
                // possible writes in the try-block and merging them with the state when
                // entering the try-block.
                // We havoc on top of the pre-env because the try-block may have initialized
                // some variables, and we want to make sure we model the fact that they may
                // still be uninitialized in the catch block.
                this.reset_ssa_env(&pre_env);
                this.havoc_current_ssa_env();
                let catch_completion_state =
                    this.run_to_completion(|this2| this2.catch_clause(handler));
                (vec![catch_completion_state], this.ssa_env())
            } else {
                (vec![], this.empty_ssa_env())
            };
            this.merge_ssa_env(&env1, &env2);
            let try_catch_completion_states = (try_completion_state, catch_completion_state_opt);
            let completion_state = this.run_to_completion(|_| {
                Self::merge_completion_states(
                    &try_catch_completion_states.0,
                    &try_catch_completion_states.1,
                )
            });
            this.commit_abrupt_completion_matching(|_| true, &completion_state)?;
            if let Some(finalizer) = finalizer {
                // NOTE: Havoc-ing the state when entering the finalizer is probably
                // overkill. We reset to the pre-env before havocing so that variables
                // that are uninitialized before the try/catch blocks are not thought to
                // be definitely initialized when entering the finally block.
                this.reset_ssa_env(&pre_env);
                this.havoc_current_ssa_env();
                this.block(&finalizer.0, &finalizer.1)?;
            }

            Self::from_completion(completion_state)
        })
    }

    // Branching expressions
    fn logical(
        &mut self,
        _loc: &Loc,
        expr: &ast::expression::Logical<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::expression::Logical {
            operator: _,
            left,
            right,
            comments: _,
        } = expr;

        self.expression(left)?;
        let env0 = self.ssa_env();
        self.expression(right)?;
        self.merge_self_ssa_env(&env0);
        Ok(())
    }

    fn conditional(
        &mut self,
        _loc: &Loc,
        expr: &ast::expression::Conditional<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::expression::Conditional {
            test,
            consequent,
            alternate,
            comments: _,
        } = expr;

        self.expression(test)?;
        let env0 = self.ssa_env();
        self.expression(consequent)?;
        let env1 = self.ssa_env();
        self.reset_ssa_env(&env0);
        self.expression(alternate)?;
        self.merge_self_ssa_env(&env1);
        Ok(())
    }

    fn function_declaration(
        &mut self,
        loc: &Loc,
        expr: &ast::function::Function<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let enable_enums = self.enable_enums;
        scope_builder::function_declaration(
            self,
            enable_enums,
            true,
            loc,
            expr,
            &|v: &mut Self, f: &mut dyn FnMut(&mut Self) -> Result<(), AbruptCompletion>| {
                v.hoist_annotations(|v2| f(v2))
            },
            |v: &mut Self, loc: &Loc, has_this: bool, id: Option<&ast::Identifier<Loc, Loc>>| {
                v.this_binding_function_id_opt(loc, has_this, id)
            },
            |v: &mut Self,
             _enable_enums_inner: bool,
             _with_types: bool,
             is_arrow_inner: bool,
             fun_loc: &Loc,
             generator_return_loc: Option<&Loc>,
             params: &ast::function::Params<Loc, Loc>,
             return_: &ast::function::ReturnAnnot<Loc, Loc>,
             predicate: Option<&ast::types::Predicate<Loc, Loc>>,
             body: &ast::function::Body<Loc, Loc>| {
                v.lambda(
                    is_arrow_inner,
                    fun_loc,
                    generator_return_loc,
                    params,
                    return_,
                    predicate,
                    body,
                )
            },
        )
    }

    fn function_(
        &mut self,
        loc: &Loc,
        expr: &ast::function::Function<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        self.function_expression_without_name_(false, loc, expr)
    }

    fn arrow_function(
        &mut self,
        loc: &Loc,
        expr: &ast::function::Function<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        self.function_expression_without_name_(true, loc, expr)
    }

    fn class_expression(
        &mut self,
        loc: &Loc,
        cls: &ast::class::Class<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::class_expression(self, loc, cls, |this, loc, cls| {
            scope_builder::class_(this, true, loc, cls, |v, loc, id| {
                v.class_identifier_opt(loc, id)
            })
        })
    }

    fn class_(
        &mut self,
        loc: &Loc,
        cls: &ast::class::Class<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::class_(self, true, loc, cls, |v, loc, id| {
            v.class_identifier_opt(loc, id)
        })
    }

    fn component_declaration(
        &mut self,
        loc: &Loc,
        component: &ast::statement::ComponentDeclaration<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::component_declaration(
            self,
            true,
            loc,
            component,
            &|v: &mut Self, f: &mut dyn FnMut(&mut Self) -> Result<(), AbruptCompletion>| {
                v.hoist_annotations(|v2| f(v2))
            },
            |v: &mut Self,
             body: &(Loc, ast::statement::Block<Loc, Loc>),
             params: &ast::statement::component_params::Params<Loc, Loc>| {
                v.component_body_with_params(body, params)
            },
        )
    }

    fn declare_module(
        &mut self,
        _loc: &Loc,
        m: &ast::statement::DeclareModule<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        let ast::statement::DeclareModule {
            id: _,
            body,
            comments: _,
        } = m;
        let (loc, body_block) = body;
        let bindings = {
            let mut hoist = Hoister::new(self.enable_enums, true);
            let Ok(()) = hoist.block(loc, body_block);
            hoist.into_bindings()
        };
        self.with_bindings(true, loc.dupe(), bindings, |this| {
            this.block(loc, body_block)
        })
    }

    fn call(
        &mut self,
        loc: &Loc,
        expr: &ast::expression::Call<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        ast_visitor::call_default(self, loc, expr)?;
        self.havoc_current_ssa_env();
        Ok(())
    }

    fn new(
        &mut self,
        loc: &Loc,
        expr: &ast::expression::New<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        ast_visitor::new_default(self, loc, expr)?;
        self.havoc_current_ssa_env();
        Ok(())
    }

    fn unary_expression(
        &mut self,
        loc: &Loc,
        expr: &ast::expression::Unary<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        ast_visitor::unary_expression_default(self, loc, expr)?;
        if expr.operator == ast::expression::UnaryOperator::Await {
            self.havoc_current_ssa_env();
        }
        Ok(())
    }

    fn yield_(
        &mut self,
        loc: &Loc,
        expr: &ast::expression::Yield<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        ast_visitor::yield_default(self, loc, expr)?;
        self.havoc_current_ssa_env();
        Ok(())
    }

    // Labeled statements handle labeled breaks, but also push labeled continues
    // that are expected to be handled by immediately nested loops.
    fn labeled_statement(
        &mut self,
        _loc: &Loc,
        stmt: &ast::statement::Labeled<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let ast::statement::Labeled {
                label,
                body,
                comments: _,
            } = stmt;

            this.possible_labeled_continues
                .push(AbruptCompletion::Continue(Some(label.name.dupe())));
            let completion_state = this.run_to_completion(|this2| this2.statement(body));
            this.possible_labeled_continues.clear();

            this.commit_abrupt_completion_matching(
                |ac| matches!(ac, AbruptCompletion::Break(Some(l)) if *l == label.name),
                &completion_state,
            )
        })
    }

    fn statement(
        &mut self,
        stmt: &ast::statement::Statement<Loc, Loc>,
    ) -> Result<(), AbruptCompletion> {
        match stmt.deref() {
            StatementInner::Labeled { .. }
            | StatementInner::While { .. }
            | StatementInner::DoWhile { .. }
            | StatementInner::For { .. }
            | StatementInner::ForIn { .. }
            | StatementInner::ForOf { .. } => {}
            _ => {
                self.possible_labeled_continues.clear();
            }
        }
        ast_visitor::statement_default(self, stmt)
    }

    fn statement_list(
        &mut self,
        stmts: &[ast::statement::Statement<Loc, Loc>],
    ) -> Result<(), AbruptCompletion> {
        // Function and component declarations are hoisted to the top of a block, so that they may be considered
        // initialized before they are read.
        let (function_decls, other_stmts): (Vec<_>, Vec<_>) =
            stmts.iter().partition(|stmt| match &***stmt {
                StatementInner::FunctionDeclaration { .. } => true,
                StatementInner::ComponentDeclaration { .. } => true,
                _ => false,
            });

        for stmt in function_decls {
            self.statement(stmt)?;
        }
        for stmt in other_stmts {
            self.statement(stmt)?;
        }
        Ok(())
    }
}

/// Result type for program_with_scope_and_jsx_pragma.
/// Contains (completion_state, (values, unbound_names)).
/// completion_state is None if the program completed normally,
/// or Some(abrupt) if there was an abrupt completion (break/continue/return/throw).
pub type ProgramWithScopeResult<Loc> = (
    Option<AbruptCompletion>,
    (Values<Loc>, BTreeSet<FlowSmolStr>),
);

/// Run SSA analysis on a program with JSX pragma support.
///
/// Before we introduce bindings for the top levels, we must read every
/// identifier in the jsx_pragma so that we can record them as unbound names.
pub fn program_with_scope_and_jsx_pragma<Loc: Dupe + Clone + Eq + Ord + Hash + Default>(
    enable_enums: bool,
    jsx_ast: Option<&ast::expression::Expression<Loc, Loc>>,
    program: &ast::Program<Loc, Loc>,
) -> ProgramWithScopeResult<Loc> {
    let loc = &program.loc;
    let mut builder = SsaBuilder::new(enable_enums);

    // Before we introduce bindings for the top levels, we must read every
    // identifier in the jsx_pragma so that we can record them as unbound names
    if let Some(jsx_expr) = jsx_ast {
        let _ = builder.run_to_completion(|builder| builder.expression(jsx_expr));
    }

    let bindings = {
        let mut hoist = Hoister::new(enable_enums, true);
        let Ok(()) = hoist.program(program);
        hoist.into_bindings()
    };

    let completion_state = builder.run_to_completion(|builder| {
        builder.with_bindings(false, loc.dupe(), bindings, |builder| {
            builder.program(program)
        })
    });

    (completion_state, (builder.values(), builder.unbound_names))
}

pub fn program_with_scope<Loc: Dupe + Clone + Eq + Ord + Hash + Default>(
    enable_enums: bool,
    program: &ast::Program<Loc, Loc>,
) -> ProgramWithScopeResult<Loc> {
    program_with_scope_and_jsx_pragma(enable_enums, None, program)
}

pub fn program<Loc: Dupe + Clone + Eq + Ord + Hash + Default>(
    enable_enums: bool,
    program: &ast::Program<Loc, Loc>,
) -> Values<Loc> {
    let (_, (values, _)) = program_with_scope(enable_enums, program);
    values
}
