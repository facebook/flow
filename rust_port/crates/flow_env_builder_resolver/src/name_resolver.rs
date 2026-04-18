/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! This module is responsible for building a mapping from variable reads to the
//! writes and refinements that reach those reads. It is based on the implementation of the
//! ssa_builder, but with enough divergent behavior that the ssa_builder and name_resolver don't
//! actually share much code. If you're here to add support for a new syntax feature, you'll likely
//! need to modify the ssa_builder as well, but not necessarily with identical changes.

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use env_api::Refinement;
use env_api::TypeGuardConsistencyMaps;
use flow_aloc::ALoc;
use flow_aloc::ALocSet;
use flow_analysis::bindings::Bindings;
use flow_analysis::bindings::Kind as BindingsKind;
use flow_analysis::hoister::LexicalHoister;
use flow_analysis::scope_builder;
use flow_analysis::scope_builder::WithBindings;
use flow_common::options::JsxMode;
use flow_common::refinement_invalidation::RefinementInvalidation;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_data_structure_wrapper::vector::FlowVector;
use flow_env_builder::env_api;
use flow_env_builder::invalidation_api;
use flow_env_builder::provider_api;
use flow_env_builder::refinement_key;
use flow_env_builder::ssa_val;
use flow_parser::ast::IdentifierInner;
use flow_parser::ast::expression::ArgList;
use flow_parser::ast::expression::CallTypeArgs;
use flow_parser::ast::expression::Expression;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::expression::LogicalOperator;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast::types::TypeInner;
use flow_parser::ast_utils;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc_sig::LocSig;
use flow_typing_errors::error_message::BindingError;
use flow_typing_errors::error_message::EAssignConstLikeBindingData;
use flow_typing_errors::error_message::EInvalidDeclarationData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::error_message::MatchErrorKind;
use flow_typing_errors::error_message::MatchInvalidPatternReferenceData;
use flow_typing_errors::error_message::NullWrite;
use flow_typing_errors::intermediate_error_types::AssignedConstLikeBindingType;
use refinement_key::Lookup;
use refinement_key::Proj;
use ssa_val::Val;
use ssa_val::ValCache;
use vec1::Vec1;

use crate::dependency_sigs::Context;
use crate::dependency_sigs::Flow;
use crate::super_call_in_derived_ctor_checker;

const MAYBE_EXHAUSTIVELY_CHECKED_VAR_NAME: &str = "<maybe_exhaustively_checked>";
static MAYBE_EXHAUSTIVELY_CHECKED_VAR_NAME_STR: std::sync::LazyLock<FlowSmolStr> =
    std::sync::LazyLock::new(|| FlowSmolStr::new(MAYBE_EXHAUSTIVELY_CHECKED_VAR_NAME));
const NEXT_VAR_NAME: &str = "<next>";

#[derive(Debug, Clone, Dupe)]
pub enum RefinementChain {
    Base(Refinement<ALoc>),
    And(usize, usize),
    Or(usize, usize),
    Not(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CondContext {
    SwitchTest,
    OtherTest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OptionalChainingRefinement {
    CanApplyPropTruthyRefi,
    CanApplyPropNonNullishRefi,
    CanApplyPropNonVoidRefi,
    CanApplyPropIsExactlyNullRefi,
}

type LookupMap = FlowOrdMap<Lookup, Val<ALoc>>;

type HeapRefinementMap = FlowOrdMap<FlowVector<Proj>, Result<Val<ALoc>, RefinementInvalidation>>;

fn empty_heap_refinements() -> Rc<RefCell<HeapRefinementMap>> {
    thread_local! {
        static CACHED: HeapRefinementMap = FlowOrdMap::new();
    }
    Rc::new(RefCell::new(CACHED.with(|c| c.clone())))
}

fn empty_refining_locs() -> FlowOrdSet<ALoc> {
    thread_local! {
        static CACHED: FlowOrdSet<ALoc> = FlowOrdSet::new();
    }
    CACHED.with(|c| c.clone())
}

#[derive(Debug, Clone)]
struct EnvValInner {
    val_ref: Rc<RefCell<Val<ALoc>>>,
    havoc: Val<ALoc>,
    writes_by_closure_provider_val: Option<Val<ALoc>>,
    def_loc: Option<ALoc>,
    heap_refinements: Rc<RefCell<HeapRefinementMap>>,
    kind: BindingsKind,
    // Per-loc kind for every declaration site of this name. This preserves the
    // per-loc kind info that to_map collapses (first-kind-wins). Used to judge
    // whether a specific declaration site is e.g. an interface vs. a type alias.
    kind_at_loc: BTreeMap<ALoc, BindingsKind>,
}

#[derive(Debug, Clone, Dupe)]
struct EnvVal(Rc<EnvValInner>);

impl EnvVal {
    fn new(inner: EnvValInner) -> Self {
        Self(Rc::new(inner))
    }
}

impl std::ops::Deref for EnvVal {
    type Target = EnvValInner;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone)]
struct ReadEntry {
    def_loc: Option<ALoc>,
    val_binding_kind: ssa_val::ValBindingKind,
    value: Val<ALoc>,
    name: Option<FlowSmolStr>,
}

/// A partial environment is a map from variables to environment value snapshots.
/// This environment contains only the set of values that might change under the current scope.
mod partial_env_snapshot {
    use super::*;

    #[derive(Debug, Clone, Dupe)]
    pub(super) struct Entry {
        pub(super) env_val: Val<ALoc>,
        pub(super) heap_refinements: HeapRefinementMap,
        pub(super) def_loc: Option<ALoc>,
    }

    impl Entry {
        pub(super) fn of_env_val(env_val: &EnvVal) -> Self {
            Entry {
                env_val: env_val.val_ref.borrow().dupe(),
                heap_refinements: env_val.heap_refinements.borrow().dupe(),
                def_loc: env_val.def_loc.dupe(),
            }
        }

        pub(super) fn reset_val_with_entry(&self, env_val: &EnvVal) {
            *env_val.val_ref.borrow_mut() = self.env_val.dupe();
            *env_val.heap_refinements.borrow_mut() = self.heap_refinements.dupe();
        }
    }

    pub(super) type PartialEnvSnapshot = FlowOrdMap<FlowSmolStr, Entry>;

    pub(super) fn read_with_fallback<F>(
        x: &FlowSmolStr,
        env: &PartialEnvSnapshot,
        fallback: F,
    ) -> Entry
    where
        F: Fn(&FlowSmolStr) -> Entry,
    {
        match env.get(x) {
            Some(v) => v.dupe(),
            None => fallback(x),
        }
    }

    pub(super) fn merge_env<Acc, V, F>(
        env: Acc,
        s1: &PartialEnvSnapshot,
        s2: &PartialEnvSnapshot,
        mut combine: F,
    ) -> (Acc, BTreeMap<FlowSmolStr, V>)
    where
        F: FnMut(Acc, &FlowSmolStr, Option<&Entry>, Option<&Entry>) -> (Acc, Option<V>),
    {
        // First: fold over s2, looking up in s1
        let mut map = BTreeMap::new();
        let mut env = env;
        for (key, v2) in s2 {
            let v1opt = s1.get(key);
            let (new_env, vopt) = combine(env, key, v1opt, Some(v2));
            env = new_env;
            if let Some(v) = vopt {
                map.insert(key.dupe(), v);
            }
        }
        // Second: fold over s1, but only for keys not in s2
        for (key, v1) in s1 {
            let v2opt = s2.get(key);
            match v2opt {
                None => {
                    let (new_env, vopt) = combine(env, key, Some(v1), None);
                    env = new_env;
                    if let Some(v) = vopt {
                        map.insert(key.dupe(), v);
                    }
                }
                Some(_) => {}
            }
        }
        (env, map)
    }
}

use partial_env_snapshot::Entry as PartialEnvEntry;
use partial_env_snapshot::PartialEnvSnapshot;

/// In this module, we maintain a mapping from names to mutable environment values that
/// represent the state of the program. Instead of a simple name -> value mapping, we have
/// multiple hierarchies.
///
/// e.g.
/// ```js
/// const foo = 1; /* v1 */
/// () => {
///   const foo = 1; /* v2 */
///   { const foo = 1; /* v3 */ }
/// }
/// ```
///
/// In the inner most scope, a full environment will be:
/// ```text
/// -----------------------
/// {"foo" => [v3; v2]}
/// -----------------------
/// {"foo" => [v1]}
/// -----------------------
/// ```
///
/// Note that we organize the environment into two kinds of stacks:
/// - function scope stack (separated by --- in the ASCII art)
/// - local shadowing stack (separated by ; within [] in the ASCII art)
///
/// We push a new stack function scope entry when we enter the function body.
/// Everything below the entry will be frozen. aka no mutations are possible on
/// any value below the entry will be possible. This encodes the idea that, for
/// type checking purposes, no effect happening inside the function body can leak
/// outside.
///
/// The local shadowing stack represents shadowing that might happen when we enter
/// a new lexical scope. We still need to keep around all the shadowed entries so
/// that refinement invalidation can still be done on those entries.
///
/// In addition to entries that are defined locally in the function scope, we need
/// to also keep tracked of refinements done on captured value. Therefore, in each
/// function scope, we also maintain a lazily populated `captured` map.
///
/// The module also contains some function that merges environment with snapshots.
/// We do not have the invariant that environment and snapshot has the same key set.
/// Instead, the current environment's keyset is a super set of snapshot's key set,
/// since the captured value map is monotonously increasing in the current environment.
mod full_env {
    use dupe::OptionDupedExt;

    use super::*;

    #[derive(Debug, Clone, Dupe)]
    pub(super) struct FunctionScope {
        pub(super) local_stacked_env: FlowOrdMap<FlowSmolStr, FlowVector<EnvVal>>,
        pub(super) captured: Rc<RefCell<BTreeMap<FlowSmolStr, EnvVal>>>,
    }

    impl FunctionScope {
        fn new() -> Self {
            thread_local! {
                static CACHED: FlowOrdMap<FlowSmolStr, FlowVector<EnvVal>> = FlowOrdMap::new();
            }
            FunctionScope {
                local_stacked_env: CACHED.with(|c| c.clone()),
                captured: Rc::new(RefCell::new(BTreeMap::new())),
            }
        }
    }

    impl Default for FunctionScope {
        fn default() -> Self {
            Self::new()
        }
    }

    pub(super) struct FunctionScopeLocalEnvSnapshot(Vec<(FlowSmolStr, Option<FlowVector<EnvVal>>)>);

    fn copy_env_val_from_env_below(
        should_havoc: impl Fn(&EnvVal) -> bool,
        env_val: &EnvVal,
    ) -> EnvVal {
        if should_havoc(env_val) {
            EnvVal::new(EnvValInner {
                val_ref: Rc::new(RefCell::new(env_val.havoc.dupe())),
                havoc: env_val.havoc.dupe(),
                writes_by_closure_provider_val: env_val.writes_by_closure_provider_val.dupe(),
                def_loc: env_val.def_loc.dupe(),
                heap_refinements: empty_heap_refinements(),
                kind: env_val.kind,
                kind_at_loc: env_val.kind_at_loc.clone(),
            })
        } else {
            EnvVal::new(EnvValInner {
                val_ref: Rc::new(RefCell::new(env_val.val_ref.borrow().dupe())),
                havoc: env_val.havoc.dupe(),
                writes_by_closure_provider_val: env_val.writes_by_closure_provider_val.dupe(),
                def_loc: env_val.def_loc.dupe(),
                heap_refinements: empty_heap_refinements(),
                kind: env_val.kind,
                kind_at_loc: env_val.kind_at_loc.clone(),
            })
        }
    }

    fn function_scope_read(x: &FlowSmolStr, scope: &FunctionScope) -> Option<EnvVal> {
        if let Some(stack) = scope.local_stacked_env.get(x) {
            return stack.back().duped();
        }
        scope.captured.borrow().get(x).duped()
    }

    fn map_function_scope_into_partial_env_entries<F>(
        scope: &FunctionScope,
        f: F,
    ) -> PartialEnvSnapshot
    where
        F: Fn(&FlowSmolStr, &EnvVal) -> PartialEnvEntry,
    {
        let mut acc = FlowOrdMap::new();
        for (x, stack) in &scope.local_stacked_env {
            if let Some(last) = stack.back() {
                acc.insert(x.dupe(), f(x, last));
            }
        }
        for (x, v) in scope.captured.borrow().iter() {
            if !acc.contains_key(x) {
                acc.insert(x.dupe(), f(x, v));
            }
        }
        acc
    }

    fn iter_function_scope<F>(scope: &FunctionScope, mut f: F)
    where
        F: FnMut(&FlowSmolStr, &EnvVal),
    {
        for (x, stack) in &scope.local_stacked_env {
            if let Some(last) = stack.back() {
                f(x, last);
            }
        }
        for (x, v) in scope.captured.borrow().iter() {
            if !scope.local_stacked_env.contains_key(x) {
                f(x, v);
            }
        }
    }

    #[derive(Debug, Clone, Dupe)]
    pub(super) struct FullEnv {
        scopes: FlowVector<FunctionScope>,
    }

    impl FullEnv {
        pub(super) fn init(globals: HashMap<FlowSmolStr, EnvVal>) -> Self {
            let local_stacked_env = globals
                .into_iter()
                .map(|(k, v)| (k, FlowVector::unit(v)))
                .collect();
            FullEnv {
                scopes: FlowVector::unit(FunctionScope {
                    local_stacked_env,
                    captured: Rc::new(RefCell::new(BTreeMap::new())),
                }),
            }
        }

        pub(super) fn fold_current_function_scope_values<A, F>(&self, init: A, mut f: F) -> A
        where
            F: FnMut(A, &EnvVal) -> A,
        {
            let scope = self.scopes.back().unwrap();
            let mut acc = init;
            for stack in scope.local_stacked_env.values() {
                for v in stack {
                    acc = f(acc, v);
                }
            }
            for v in scope.captured.borrow().values() {
                acc = f(acc, v);
            }
            acc
        }

        pub(super) fn env_read_opt(
            &self,
            should_havoc: impl Fn(&EnvVal) -> bool,
            x: &FlowSmolStr,
        ) -> Option<EnvVal> {
            let curr_scope = self.scopes.back().unwrap();
            if let Some(v) = function_scope_read(x, curr_scope) {
                return Some(v);
            }
            let len = self.scopes.len();
            for i in (0..len.saturating_sub(1)).rev() {
                if let Some(env_val) = function_scope_read(x, &self.scopes[i]) {
                    let copy = copy_env_val_from_env_below(&should_havoc, &env_val);
                    curr_scope
                        .captured
                        .borrow_mut()
                        .insert(x.dupe(), copy.dupe());
                    return Some(copy);
                }
            }
            None
        }

        pub(super) fn env_read(
            &self,
            should_havoc: impl Fn(&EnvVal) -> bool,
            x: &FlowSmolStr,
        ) -> EnvVal {
            self.env_read_opt(should_havoc, x)
                .unwrap_or_else(|| panic!("Missing env entry: {}", x))
        }

        pub(super) fn env_read_from_below(
            &self,
            should_havoc: impl Fn(&EnvVal) -> bool,
            x: &FlowSmolStr,
        ) -> EnvVal {
            let len = self.scopes.len();
            for i in (0..len.saturating_sub(1)).rev() {
                if let Some(env_val) = function_scope_read(x, &self.scopes[i]) {
                    return copy_env_val_from_env_below(&should_havoc, &env_val);
                }
            }
            panic!("Missing env entry from below: {}", x)
        }

        pub(super) fn env_read_entry_from_below(
            &self,
            should_havoc: impl Fn(&EnvVal) -> bool,
            x: &FlowSmolStr,
        ) -> PartialEnvEntry {
            PartialEnvEntry::of_env_val(&self.env_read_from_below(should_havoc, x))
        }

        /// Push new bindings that might shadow bindings in the current function scope.
        /// Returns a snapshot to restore old state.
        pub(super) fn push_new_bindings(
            &mut self,
            bindings: BTreeMap<FlowSmolStr, EnvVal>,
        ) -> FunctionScopeLocalEnvSnapshot {
            let scope = self.scopes.last_mut().unwrap();
            let mut snapshot = Vec::with_capacity(bindings.len());
            for (x, v) in bindings {
                snapshot.push((x.dupe(), scope.local_stacked_env.get(&x).duped()));
                match scope.local_stacked_env.get(&x) {
                    Some(stack) => {
                        let mut new_stack = stack.dupe();
                        new_stack.push_back(v);
                        scope.local_stacked_env.insert(x, new_stack);
                    }
                    None => {
                        scope.local_stacked_env.insert(x, FlowVector::unit(v));
                    }
                }
            }
            FunctionScopeLocalEnvSnapshot(snapshot)
        }

        pub(super) fn pop_bindings(&mut self, snapshot: FunctionScopeLocalEnvSnapshot) {
            let scope = self.scopes.last_mut().unwrap();
            for (name, old_stack) in snapshot.0 {
                match old_stack {
                    Some(old_stack) => {
                        scope.local_stacked_env.insert(name, old_stack);
                    }
                    None => {
                        scope.local_stacked_env.remove(&name);
                    }
                }
            }
        }

        pub(super) fn push_new_function_scope(&mut self) {
            self.scopes.push(FunctionScope::new());
        }

        pub(super) fn pop_function_scope(&mut self) {
            self.scopes.pop().unwrap();
        }

        pub(super) fn to_partial_env_snapshot<F>(&self, f: F) -> PartialEnvSnapshot
        where
            F: Fn(&FlowSmolStr, &EnvVal) -> PartialEnvEntry,
        {
            map_function_scope_into_partial_env_entries(self.scopes.back().unwrap(), f)
        }

        pub(super) fn update_env<F>(&self, f: F)
        where
            F: FnMut(&FlowSmolStr, &EnvVal),
        {
            iter_function_scope(self.scopes.back().unwrap(), f);
        }

        pub(super) fn reset_to_unreachable_env(&self, cache: &mut ValCache<ALoc>) {
            let empty_val = ssa_val::empty(cache);
            iter_function_scope(self.scopes.back().unwrap(), |_, env_val| {
                *env_val.val_ref.borrow_mut() = empty_val.dupe();
                let keys: Vec<_> = env_val.heap_refinements.borrow().keys().duped().collect();
                *env_val.heap_refinements.borrow_mut() = keys
                    .into_iter()
                    .map(|k| (k, Ok(empty_val.dupe())))
                    .collect();
            });
        }

        pub(super) fn update_env_with_partial_env_snapshot<F>(
            &self,
            should_havoc: impl Fn(&EnvVal) -> bool,
            partial_env: &PartialEnvSnapshot,
            mut f: F,
        ) where
            F: FnMut(&FlowSmolStr, &PartialEnvEntry, &EnvVal),
        {
            iter_function_scope(self.scopes.back().unwrap(), |x, env_val| {
                let env_entry = partial_env_snapshot::read_with_fallback(x, partial_env, |x| {
                    self.env_read_entry_from_below(&should_havoc, x)
                });
                f(x, &env_entry, env_val)
            });
        }

        pub(super) fn merge_env_with_partial_env_snapshots<F>(
            &self,
            should_havoc: impl Fn(&EnvVal) -> bool,
            env1: &PartialEnvSnapshot,
            env2: &PartialEnvSnapshot,
            mut f: F,
        ) where
            F: FnMut(&PartialEnvEntry, &PartialEnvEntry, &EnvVal),
        {
            iter_function_scope(self.scopes.back().unwrap(), |x, env_val| {
                let v1 = partial_env_snapshot::read_with_fallback(x, env1, |x| {
                    self.env_read_entry_from_below(&should_havoc, x)
                });
                let v2 = partial_env_snapshot::read_with_fallback(x, env2, |x| {
                    self.env_read_entry_from_below(&should_havoc, x)
                });
                f(&v1, &v2, env_val);
            });
        }

        pub(super) fn current_scope_mut(&mut self) -> &mut FunctionScope {
            self.scopes.last_mut().unwrap()
        }

        /// Get all names in the current scope
        pub(super) fn all_names(&self) -> Vec<FlowSmolStr> {
            let mut names = Vec::new();
            iter_function_scope(self.scopes.back().unwrap(), |name, _| {
                names.push(name.dupe());
            });
            names
        }

        /// Get an env_val by name from the current scope
        pub(super) fn env_val_by_name(&self, name: &FlowSmolStr) -> Option<EnvVal> {
            function_scope_read(name, self.scopes.back().unwrap())
        }
    }
}

use full_env::FullEnv;

fn heap_map_find(x: &FlowVector<Proj>, t: &HeapRefinementMap) -> Val<ALoc> {
    match t.get(x) {
        Some(Ok(r)) => r.dupe(),
        Some(Err(_)) | None => panic!("Heap entry missing in map"),
    }
}

/// Abrupt completions induce control flows, so modeling them accurately is
/// necessary for soundnes
mod abrupt_completion {
    use super::*;

    #[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash)]
    pub struct Label(FlowSmolStr);

    impl std::ops::Deref for Label {
        type Target = FlowSmolStr;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl From<FlowSmolStr> for Label {
        fn from(s: FlowSmolStr) -> Self {
            Label(s)
        }
    }

    impl From<Label> for FlowSmolStr {
        fn from(l: Label) -> Self {
            l.0
        }
    }

    #[derive(Debug, Clone, Dupe, PartialEq, Eq)]
    pub enum AbruptCompletion {
        Break(Option<Label>),
        Continue(Option<Label>),
        Return,
        Throw,
    }

    impl AbruptCompletion {
        /// Check if completion matches any in the list
        pub(super) fn mem(&self, list: &[AbruptCompletion]) -> bool {
            list.contains(self)
        }

        /// Match all completions
        pub(super) fn all(_: &AbruptCompletion) -> bool {
            true
        }
    }

    /// An abrupt completion carries an environment, which is the current
    /// environment at the point where the abrupt completion is "raised." This
    /// environment is merged wherever the abrupt completion is "handled."
    pub(super) type AbruptEnv = (AbruptCompletion, PartialEnvSnapshot);
}

pub use abrupt_completion::AbruptCompletion;
use abrupt_completion::AbruptEnv;

// =============================================================================
// Types used within the Make functor (name_resolver class state)
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct RefinementId(pub usize);

#[derive(Default, Debug, Clone, Dupe)]
struct Changeset(LookupMap);

impl std::ops::Deref for Changeset {
    type Target = LookupMap;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Changeset {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl IntoIterator for Changeset {
    type Item = (Lookup, Val<ALoc>);
    type IntoIter = flow_data_structure_wrapper::ord_map::OrdMapConsumingIter<(Lookup, Val<ALoc>)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, Clone)]
enum RefinementPropInner {
    Refinements {
        applied: FlowOrdMap<usize, (Lookup, RefinementId)>,
        changeset: FlowOrdMap<Lookup, Val<ALoc>>,
    },
    Not(RefinementProp),
    And(RefinementProp, RefinementProp),
    Or(RefinementProp, RefinementProp),
}

#[derive(Debug, Clone, Dupe)]
struct RefinementProp(Rc<RefinementPropInner>);

impl RefinementProp {
    fn new(inner: RefinementPropInner) -> Self {
        RefinementProp(Rc::new(inner))
    }

    fn refinements(
        applied: FlowOrdMap<usize, (Lookup, RefinementId)>,
        changeset: FlowOrdMap<Lookup, Val<ALoc>>,
    ) -> Self {
        Self::new(RefinementPropInner::Refinements { applied, changeset })
    }

    fn not(prop: RefinementProp) -> Self {
        Self::new(RefinementPropInner::Not(prop))
    }

    fn and(left: RefinementProp, right: RefinementProp) -> Self {
        Self::new(RefinementPropInner::And(left, right))
    }

    fn or(left: RefinementProp, right: RefinementProp) -> Self {
        Self::new(RefinementPropInner::Or(left, right))
    }
}

impl std::ops::Deref for RefinementProp {
    type Target = RefinementPropInner;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// The `applied` and `changeset` elements of this record describe the refinements and
// values that have presently been applied to the environment, when this is on the
// latest_refinement stack (see below). The `total` describes the same changeset and
// refinements, but in a propositional form that can be negated. We must maintain the invariant
// that normalizing the `total` (using the normalize_total_refinements method below) produces the
// applied and the changeset. *)
#[derive(Default, Debug, Clone, Dupe)]
struct RefinementMaps {
    applied: FlowOrdMap<usize, (Lookup, RefinementId)>,
    changeset: Changeset,
    total: Option<RefinementProp>,
}

impl RefinementMaps {
    fn empty() -> Self {
        Self::default()
    }
}

#[derive(Debug, Clone)]
struct TypeGuardNameInfoInner {
    loc: ALoc,
    name: FlowSmolStr,
    id: usize,
    havoced: RefCell<Option<FlowOrdSet<ALoc>>>,
    inferred: bool,
}

#[derive(Debug, Clone, Dupe)]
struct TypeGuardNameInfo(Rc<TypeGuardNameInfoInner>);

impl Deref for TypeGuardNameInfo {
    type Target = TypeGuardNameInfoInner;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

use env_api::EnvEntry;
use env_api::EnvMap;

/// State for the name resolver analysis
#[derive(Debug, Clone)]
struct NameResolverState {
    /// We maintain a map of read locations to raw Val.t and their def locs terms, which are
    /// simplified to lists of write locations once the analysis is done.
    values: FlowOrdMap<ALoc, ReadEntry>,
    /// Should not be used for normal checking. It's not comprehensive but it would be helpful
    /// to tell user on hover with certainty that certain values they want to stay refined are
    /// invalidated.
    refinement_invalidation_info: FlowOrdMap<ALoc, RefinementInvalidation>,
    /// We also maintain a list of all write locations, for use in populating the env with types.
    write_entries: EnvMap<ALoc, EnvEntry<ALoc>>,
    type_guard_consistency_maps: TypeGuardConsistencyMaps<ALoc>,
    curr_id: usize,
    /// Maps refinement ids to refinements. This mapping contains _all_ the refinements reachable at
    /// any point in the code. The latest_refinement maps keep track of which entries to read.
    /// Wrapped in RefCell for interior mutability, allowing independent borrowing from env field.
    refinement_heap: RefCell<FlowOrdMap<RefinementId, RefinementChain>>,
    latest_refinements: FlowVector<RefinementMaps>,
    env: FullEnv,
    /// A set of names that have to be excluded from binding.
    /// This set is always empty when we are checking normal code. It will only be possibly non-empty
    /// when we are checking libdef code. In libdefs, this set represents a list of names that we
    /// have already added to the globals. When we read a name in this set, we will ignore the local
    /// binding and treat it as a read of global.
    exclude_syms: FlowOrdSet<FlowSmolStr>,
    /// When an abrupt completion is raised, it falls through any subsequent
    /// straight-line code, until it reaches a merge point in the control-flow
    /// graph. At that point, it can be re-raised if and only if all other reaching
    /// control-flow paths also raise the same abrupt completion.
    ///
    /// When re-raising is not possible, we have to save the abrupt completion and
    /// the current environment in a list, so that we can merge such environments
    /// later (when that abrupt completion and others like it are handled).
    ///
    /// Even when raising is possible, we still have to save the current
    /// environment, since the current environment will have to be cleared to model
    /// that the current values of all variables are unreachable.
    ///
    /// NOTE that raising is purely an optimization: we can have more precise
    /// results with raising, but even if we never raised we'd still be sound.
    abrupt_completion_envs: FlowVector<AbruptEnv>,
    /// Track the list of labels that might describe a loop. Used to detect which
    /// labeled continues need to be handled by the loop.
    ///
    /// The idea is that a labeled statement adds its label to the list before
    /// entering its child, and if the child is not a loop or another labeled
    /// statement, the list will be cleared. A loop will consume the list, so we
    /// also clear the list on our way out of any labeled statement.
    possible_labeled_continues: FlowVector<AbruptCompletion>,
    type_guard_name: Option<TypeGuardNameInfo>,
    inferred_type_guard_candidate: Option<(ALoc, FlowSmolStr)>,
    visiting_hoisted_type: bool,
    in_conditional_type_extends: bool,
    interface_merge_conflicts: BTreeMap<ALoc, Vec<ALoc>>,
    jsx_base_name: Option<FlowSmolStr>,
    pred_func_map: FlowOrdMap<ALoc, env_api::PredFuncInfo<ALoc>>,
    /// Track parameter binding def_locs currently being processed, so that we can
    /// error when these appear in the corresponding annotation.
    current_bindings: FlowOrdMap<ALoc, FlowSmolStr>,
    /// Track when we're visiting a parameter default expression, so we can
    /// produce the appropriate error message for self-references.
    in_param_default: bool,
}

impl NameResolverState {
    fn new(env: FullEnv, exclude_syms: FlowOrdSet<FlowSmolStr>) -> Self {
        thread_local! {
            static CACHED_VALUES: FlowOrdMap<ALoc, ReadEntry> = FlowOrdMap::new();
            static CACHED_REFI_INFO: FlowOrdMap<ALoc, RefinementInvalidation> = FlowOrdMap::new();
            static CACHED_REFI_HEAP: FlowOrdMap<RefinementId, RefinementChain> = FlowOrdMap::new();
            static CACHED_PRED: FlowOrdMap<ALoc, env_api::PredFuncInfo<ALoc>> = FlowOrdMap::new();
            static CACHED_BINDINGS: FlowOrdMap<ALoc, FlowSmolStr> = FlowOrdMap::new();
        }
        NameResolverState {
            values: CACHED_VALUES.with(|c| c.clone()),
            refinement_invalidation_info: CACHED_REFI_INFO.with(|c| c.clone()),
            write_entries: EnvMap::empty(),
            type_guard_consistency_maps: TypeGuardConsistencyMaps::new(),
            curr_id: 0,
            refinement_heap: RefCell::new(CACHED_REFI_HEAP.with(|c| c.clone())),
            latest_refinements: FlowVector::new(),
            env,
            exclude_syms,
            abrupt_completion_envs: FlowVector::new(),
            possible_labeled_continues: FlowVector::new(),
            type_guard_name: None,
            inferred_type_guard_candidate: None,
            visiting_hoisted_type: false,
            in_conditional_type_extends: false,
            interface_merge_conflicts: BTreeMap::new(),
            jsx_base_name: None,
            pred_func_map: CACHED_PRED.with(|c| c.clone()),
            current_bindings: CACHED_BINDINGS.with(|c| c.clone()),
            in_param_default: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PatternWriteKind {
    VarBinding,
    LetBinding,
    ClassBinding,
    ConstBinding,
    FunctionBinding,
    ComponentBinding,
    AssignmentWrite,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ThisSuperBindingEnv {
    FunctionEnv,
    ClassInstanceEnv,
    ClassStaticEnv,
    IllegalThisEnv,
}

fn variable_declaration_binding_kind_to_pattern_write_kind(
    kind: Option<flow_parser::ast::VariableKind>,
) -> PatternWriteKind {
    use flow_parser::ast::VariableKind;
    match kind {
        None => PatternWriteKind::AssignmentWrite,
        Some(VariableKind::Var) => PatternWriteKind::VarBinding,
        Some(VariableKind::Let) => PatternWriteKind::LetBinding,
        Some(VariableKind::Const) => PatternWriteKind::ConstBinding,
    }
}

fn error_for_assignment_kind(
    name: &FlowSmolStr,
    assignment_loc: ALoc,
    def_loc_opt: Option<ALoc>,
    stored_binding_kind: flow_analysis::bindings::Kind,
    pattern_write_kind: PatternWriteKind,
    v: &Val<ALoc>,
    enable_const_params: bool,
) -> Option<ErrorMessage<ALoc>> {
    use flow_analysis::bindings::Kind as BK;
    use flow_common::reason::Name;
    use flow_common::reason::VirtualReason;
    use flow_common::reason::VirtualReasonDesc;

    fn is_bundled_in_core_dot_js(name: &str) -> bool {
        matches!(
            name,
            "ReadonlyArray"
                | "ReadonlyMap"
                | "ReadonlySet"
                | "$ReadOnlyMap"
                | "$ReadOnlySet"
                | "$ReadOnlyArray"
        )
    }

    fn binding_error(
        error: BindingError,
        assignment_loc: ALoc,
        name: FlowSmolStr,
        def_loc: ALoc,
    ) -> ErrorMessage<ALoc> {
        ErrorMessage::EBindingError(Box::new((error, assignment_loc, Name::new(name), def_loc)))
    }

    fn const_like_binding_error(
        assignment_loc: ALoc,
        name: &FlowSmolStr,
        def_loc: ALoc,
        binding_kind: AssignedConstLikeBindingType,
    ) -> ErrorMessage<ALoc> {
        let definition = VirtualReason::new(
            VirtualReasonDesc::RIdentifier(Name::new(name.to_string())),
            def_loc,
        );
        ErrorMessage::EAssignConstLikeBinding(Box::new(EAssignConstLikeBindingData {
            loc: assignment_loc,
            definition,
            binding_kind,
        }))
    }

    // Identifiers with no binding can never reintroduce "cannot reassign binding" errors
    let def_loc = def_loc_opt?;

    if is_bundled_in_core_dot_js(name.as_str()) {
        return Some(binding_error(
            BindingError::ENameAlreadyBoundInCoreJs,
            assignment_loc,
            name.dupe(),
            def_loc,
        ));
    }

    match (stored_binding_kind, pattern_write_kind) {
        (BK::Const | BK::DeclaredConst, PatternWriteKind::AssignmentWrite) => Some(binding_error(
            BindingError::EConstReassigned,
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        (BK::Parameter, PatternWriteKind::AssignmentWrite) if enable_const_params => {
            Some(binding_error(
                BindingError::EConstParamReassigned,
                assignment_loc,
                name.dupe(),
                def_loc,
            ))
        }
        (BK::ComponentParameter, PatternWriteKind::AssignmentWrite) => Some(binding_error(
            BindingError::EConstParamReassigned,
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        (BK::Class | BK::DeclaredClass, PatternWriteKind::AssignmentWrite) => {
            Some(const_like_binding_error(
                assignment_loc,
                name,
                def_loc,
                AssignedConstLikeBindingType::ClassNameBinding,
            ))
        }
        (BK::Function, PatternWriteKind::AssignmentWrite) => Some(const_like_binding_error(
            assignment_loc,
            name,
            def_loc,
            AssignedConstLikeBindingType::FunctionNameBinding,
        )),
        (BK::Component, PatternWriteKind::AssignmentWrite) => Some(const_like_binding_error(
            assignment_loc,
            name,
            def_loc,
            AssignedConstLikeBindingType::ComponentNameBinding,
        )),
        (BK::Record, PatternWriteKind::AssignmentWrite) => Some(const_like_binding_error(
            assignment_loc,
            name,
            def_loc,
            AssignedConstLikeBindingType::RecordNameBinding,
        )),
        (BK::Import | BK::TsImport, PatternWriteKind::AssignmentWrite) => Some(binding_error(
            BindingError::EImportReassigned,
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        (BK::DeclaredFunction, PatternWriteKind::AssignmentWrite) => {
            Some(const_like_binding_error(
                assignment_loc,
                name,
                def_loc,
                AssignedConstLikeBindingType::DeclaredFunctionNameBinding,
            ))
        }
        (
            BK::Var | BK::DeclaredVar,
            PatternWriteKind::LetBinding
            | PatternWriteKind::ClassBinding
            | PatternWriteKind::ConstBinding
            | PatternWriteKind::FunctionBinding
            | PatternWriteKind::ComponentBinding,
        ) => Some(binding_error(
            BindingError::ENameAlreadyBound,
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        // We ban var redeclaration on top of other JS illegal rebinding rules.
        (BK::Var | BK::DeclaredVar, PatternWriteKind::VarBinding) if assignment_loc != def_loc => {
            Some(binding_error(
                BindingError::EVarRedeclaration,
                assignment_loc,
                name.dupe(),
                def_loc,
            ))
        }
        (
            BK::Const
            | BK::Let
            | BK::Class
            | BK::Record
            | BK::Enum
            | BK::Function
            | BK::Component
            | BK::Import
            | BK::TsImport
            | BK::Interface { .. },
            PatternWriteKind::VarBinding
            | PatternWriteKind::LetBinding
            | PatternWriteKind::ClassBinding
            | PatternWriteKind::ConstBinding
            | PatternWriteKind::FunctionBinding
            | PatternWriteKind::ComponentBinding,
        ) if !ssa_val::is_undeclared(v) => Some(binding_error(
            BindingError::ENameAlreadyBound,
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        (
            BK::Type { .. } | BK::Interface { .. },
            PatternWriteKind::VarBinding
            | PatternWriteKind::LetBinding
            | PatternWriteKind::ClassBinding
            | PatternWriteKind::ConstBinding
            | PatternWriteKind::FunctionBinding
            | PatternWriteKind::ComponentBinding,
        ) if !ssa_val::is_undeclared(v) => Some(binding_error(
            BindingError::ENameAlreadyBound,
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        (
            BK::DeclaredClass | BK::DeclaredConst | BK::DeclaredLet,
            PatternWriteKind::VarBinding
            | PatternWriteKind::LetBinding
            | PatternWriteKind::ClassBinding
            | PatternWriteKind::ConstBinding
            | PatternWriteKind::FunctionBinding
            | PatternWriteKind::ComponentBinding,
        ) if assignment_loc != def_loc => Some(binding_error(
            BindingError::ENameAlreadyBound,
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        (
            BK::DeclaredFunction,
            PatternWriteKind::VarBinding
            | PatternWriteKind::LetBinding
            | PatternWriteKind::ClassBinding
            | PatternWriteKind::ConstBinding
            | PatternWriteKind::ComponentBinding,
        ) => Some(binding_error(
            BindingError::ENameAlreadyBound,
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        (BK::Enum, PatternWriteKind::AssignmentWrite) => Some(binding_error(
            BindingError::EEnumReassigned,
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        (
            BK::Type {
                imported,
                type_only_namespace,
            }
            | BK::Interface {
                imported,
                type_only_namespace,
            },
            PatternWriteKind::AssignmentWrite,
        ) => Some(binding_error(
            BindingError::ETypeInValuePosition {
                imported,
                type_only_namespace,
                name: name.dupe(),
            },
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        (
            BK::Parameter | BK::ComponentParameter,
            PatternWriteKind::VarBinding
            | PatternWriteKind::LetBinding
            | PatternWriteKind::ClassBinding
            | PatternWriteKind::ConstBinding
            | PatternWriteKind::FunctionBinding
            | PatternWriteKind::ComponentBinding,
        ) if (enable_const_params || matches!(stored_binding_kind, BK::ComponentParameter))
            && !ssa_val::is_undeclared(v) =>
        {
            Some(binding_error(
                BindingError::ENameAlreadyBound,
                assignment_loc,
                name.dupe(),
                def_loc,
            ))
        }
        (
            BK::Parameter | BK::ComponentParameter,
            PatternWriteKind::LetBinding
            | PatternWriteKind::ClassBinding
            | PatternWriteKind::ConstBinding
            | PatternWriteKind::FunctionBinding
            | PatternWriteKind::ComponentBinding,
        ) if !ssa_val::is_undeclared(v) => Some(binding_error(
            BindingError::ENameAlreadyBound,
            assignment_loc,
            name.dupe(),
            def_loc,
        )),
        _ => None,
    }
}

const MODULE_SCOPED_VARS: &[&str] = &["eval", "arguments"];

fn initialize_globals(
    is_lib: bool,
    exclude_syms: &FlowOrdSet<FlowSmolStr>,
    unbound_names: &BTreeSet<FlowSmolStr>,
    program_loc: ALoc,
    cache: &mut ValCache<ALoc>,
) -> HashMap<FlowSmolStr, EnvVal> {
    use flow_common::reason::VirtualReason;
    use flow_common::reason::VirtualReasonDesc;
    use refinement_key::Proj;

    let mut globals = HashMap::with_capacity(unbound_names.len() + exclude_syms.len() + 10);

    for name in unbound_names {
        let global_val = ssa_val::global(cache, name.dupe());
        let entry = EnvVal::new(EnvValInner {
            val_ref: Rc::new(RefCell::new(global_val.dupe())),
            havoc: global_val,
            writes_by_closure_provider_val: None,
            def_loc: None,
            heap_refinements: empty_heap_refinements(),
            kind: BindingsKind::Var,
            kind_at_loc: BTreeMap::new(),
        });
        globals.insert(name.dupe(), entry);
    }

    for name in exclude_syms {
        let global_val = ssa_val::global(cache, name.dupe());
        let entry = EnvVal::new(EnvValInner {
            val_ref: Rc::new(RefCell::new(global_val.dupe())),
            havoc: global_val,
            writes_by_closure_provider_val: None,
            def_loc: None,
            heap_refinements: empty_heap_refinements(),
            kind: BindingsKind::Var,
            kind_at_loc: BTreeMap::new(),
        });
        globals.insert(name.dupe(), entry);
    }

    // this has to come later, since this can be thought to be unbound names
    // in SSA builder when it's used as a type.
    {
        let desc = VirtualReasonDesc::RCustom(FlowSmolStr::new("global object"));
        let reason = VirtualReason::new(desc, program_loc.dupe());
        let v = ssa_val::global_this(cache, reason);
        let entry = EnvVal::new(EnvValInner {
            val_ref: Rc::new(RefCell::new(v.dupe())),
            havoc: v,
            writes_by_closure_provider_val: None,
            def_loc: None,
            heap_refinements: empty_heap_refinements(),
            kind: BindingsKind::Var,
            kind_at_loc: BTreeMap::new(),
        });
        globals.insert(FlowSmolStr::new_inline("this"), entry);
    }

    if !is_lib {
        let module_name = FlowSmolStr::new_inline("module");
        let exports_name = FlowSmolStr::new_inline("exports");
        let module_val = ssa_val::global(cache, module_name.dupe());
        let exports_val = ssa_val::global(cache, exports_name);

        let mut heap_refinements = FlowOrdMap::new();
        heap_refinements.insert(
            FlowVector::unit(Proj::Prop(FlowSmolStr::new_inline("exports"))),
            Ok(exports_val),
        );

        let entry = EnvVal::new(EnvValInner {
            val_ref: Rc::new(RefCell::new(module_val.dupe())),
            havoc: module_val,
            writes_by_closure_provider_val: None,
            def_loc: None,
            heap_refinements: Rc::new(RefCell::new(heap_refinements)),
            kind: BindingsKind::Var,
            kind_at_loc: BTreeMap::new(),
        });
        globals.insert(module_name, entry);
    }

    for &name in MODULE_SCOPED_VARS {
        let name_str = FlowSmolStr::new_inline(name);
        let module_scoped_val = ssa_val::module_scoped(cache, name_str.dupe());
        let entry = EnvVal::new(EnvValInner {
            val_ref: Rc::new(RefCell::new(module_scoped_val.dupe())),
            havoc: module_scoped_val,
            writes_by_closure_provider_val: None,
            def_loc: None,
            heap_refinements: empty_heap_refinements(),
            kind: BindingsKind::Var,
            kind_at_loc: BTreeMap::new(),
        });
        globals.insert(name_str, entry);
    }

    globals
}

/// statement.rs tries to extract the name and traverse at the location of the
/// jsx element if it's an identifier, otherwise it just traverses the
/// jsx_pragma expression
fn extract_jsx_basename(
    expr: &flow_parser::ast::expression::Expression<flow_aloc::ALoc, flow_aloc::ALoc>,
) -> Option<FlowSmolStr> {
    use flow_parser::ast::expression::ExpressionInner;
    match expr.deref() {
        ExpressionInner::Identifier { inner, .. } => Some(inner.name.dupe()),
        _ => None,
    }
}

fn initial_env<Cx: Context>(
    cx: &Cx,
    is_lib: bool,
    exclude_syms: &FlowOrdSet<FlowSmolStr>,
    unbound_names: &BTreeSet<FlowSmolStr>,
    program_loc: ALoc,
    cache: &mut ValCache<ALoc>,
) -> (full_env::FullEnv, Option<FlowSmolStr>) {
    let mut globals = initialize_globals(is_lib, exclude_syms, unbound_names, program_loc, cache);

    let exhaustive_val = ssa_val::undeclared(
        cache,
        MAYBE_EXHAUSTIVELY_CHECKED_VAR_NAME_STR.dupe(),
        ALoc::none(),
    );
    let exhaustive_entry = EnvVal::new(EnvValInner {
        val_ref: Rc::new(RefCell::new(exhaustive_val.dupe())),
        havoc: exhaustive_val,
        writes_by_closure_provider_val: None,
        def_loc: None,
        heap_refinements: empty_heap_refinements(),
        kind: BindingsKind::Internal,
        kind_at_loc: BTreeMap::new(),
    });
    globals.insert(
        MAYBE_EXHAUSTIVELY_CHECKED_VAR_NAME_STR.dupe(),
        exhaustive_entry,
    );

    // We need to make sure that the base name for jsx is always in scope.
    // statement.rs is going to read these identifiers at jsx calls, even if
    // they haven't been declared locally.
    let jsx_base_name = match cx.jsx() {
        JsxMode::JsxReact => Some(FlowSmolStr::new_inline("React")),
        JsxMode::JsxPragma(_, ast) => extract_jsx_basename(&ast),
    };

    if let Some(ref jsx_name) = jsx_base_name {
        // We use a global here so that if the base name is never created locally
        // we first check the globals before emitting an error
        let global_val = ssa_val::global(cache, jsx_name.dupe());
        let env_val = EnvVal::new(EnvValInner {
            val_ref: Rc::new(RefCell::new(global_val.dupe())),
            havoc: global_val,
            writes_by_closure_provider_val: None,
            def_loc: None,
            heap_refinements: empty_heap_refinements(),
            kind: BindingsKind::Var,
            kind_at_loc: BTreeMap::new(),
        });
        globals.insert(jsx_name.dupe(), env_val);
    }

    (full_env::FullEnv::init(globals), jsx_base_name)
}

fn conj_total(t1: Option<RefinementProp>, t2: Option<RefinementProp>) -> Option<RefinementProp> {
    match (t1, t2) {
        (Some(t1), Some(t2)) => Some(RefinementProp::and(t1, t2)),
        (Some(t), None) | (None, Some(t)) => Some(t),
        (None, None) => None,
    }
}

fn empty_refinements() -> RefinementMaps {
    thread_local! {
        static CACHED: RefinementMaps = RefinementMaps {
            applied: FlowOrdMap::new(),
            changeset: Changeset::default(),
            total: None,
        };
    }
    CACHED.with(|c| c.clone())
}

use flow_parser::ast::Program;

struct NameResolver<'a, Cx: Context, Fl: Flow<Cx = Cx>> {
    cx: &'a Cx,
    class_stack: FlowVector<ALoc>,
    env_state: NameResolverState,
    cache: Rc<RefCell<ValCache<ALoc>>>,
    simplify_cache: RefCell<HashMap<usize, Vec<env_api::WriteLoc<ALoc>>>>,
    enable_enums: bool,
    is_ts: bool,
    enable_const_params: bool,
    provider_info: Rc<provider_api::Info<ALoc>>,
    prepass_info: &'a flow_analysis::scope_api::ScopeInfo<ALoc>,
    prepass_values: &'a flow_analysis::ssa_api::Values<ALoc>,
    invalidation_caches: RefCell<invalidation_api::InvalidationCaches<ALoc>>,
    _phantom: std::marker::PhantomData<Fl>,
}

impl<'a, Cx: Context, Fl: Flow<Cx = Cx>> WithBindings<ALoc, AbruptCompletion>
    for NameResolver<'a, Cx, Fl>
{
    fn with_bindings<T>(
        &mut self,
        _lexical: bool,
        _loc: ALoc,
        bindings: Bindings<ALoc>,
        visit: impl FnOnce(&mut Self) -> Result<T, AbruptCompletion>,
    ) -> Result<T, AbruptCompletion> {
        self.with_scoped_bindings(ThisSuperBindingEnv::FunctionEnv, &bindings, visit)
    }
}

impl<'a, Cx: Context, Fl: Flow<Cx = Cx>> NameResolver<'a, Cx, Fl> {
    fn new(
        cx: &'a Cx,
        is_lib: bool,
        exclude_syms: FlowOrdSet<FlowSmolStr>,
        prepass: (
            &'a flow_analysis::scope_api::ScopeInfo<ALoc>,
            &'a flow_analysis::ssa_api::Values<ALoc>,
            &BTreeSet<FlowSmolStr>,
        ),
        provider_info: Rc<provider_api::Info<ALoc>>,
        program_loc: ALoc,
    ) -> Self {
        let (prepass_info, prepass_values, unbound_names) = prepass;
        let enable_enums = cx.enable_enums();
        let is_ts = flow_common::files::has_ts_ext(&cx.file());
        let enable_const_params = cx.enable_const_params();
        let cache = Rc::new(RefCell::new(ValCache::new()));
        let (env, jsx_base_name) = initial_env(
            cx,
            is_lib,
            &exclude_syms,
            unbound_names,
            program_loc,
            &mut cache.borrow_mut(),
        );
        let mut env_state = NameResolverState::new(env, exclude_syms);
        env_state.jsx_base_name = jsx_base_name;
        NameResolver {
            cx,
            env_state,
            cache,
            simplify_cache: RefCell::new(HashMap::new()),
            enable_enums,
            is_ts,
            enable_const_params,
            provider_info,
            prepass_info,
            prepass_values,
            invalidation_caches: RefCell::new(invalidation_api::InvalidationCaches::new()),
            class_stack: FlowVector::new(),
            _phantom: std::marker::PhantomData,
        }
    }

    fn valid_declaration_check(&self, name: &FlowSmolStr, loc: &ALoc) {
        use flow_common::reason::Name;
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;
        use invalidation_api::InitializationValid;
        use invalidation_api::declaration_validity;

        let declaration = VirtualReason::new(
            VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
            loc.dupe(),
        );

        let error =
            |null_write: Option<ALoc>,
             possible_generic_escape_locs: &flow_data_structure_wrapper::ord_set::FlowOrdSet<
                ALoc,
            >| {
                let null_write = null_write.map(|null_loc| NullWrite {
                    null_loc: null_loc.dupe(),
                    initialized: loc == &null_loc,
                });
                Fl::add_output(
                    self.cx,
                    ErrorMessage::EInvalidDeclaration(Box::new(EInvalidDeclarationData {
                        declaration: declaration.dupe(),
                        null_write,
                        possible_generic_escape_locs: possible_generic_escape_locs
                            .iter()
                            .duped()
                            .collect(),
                    })),
                );
            };

        match declaration_validity(
            self.prepass_info,
            self.prepass_values,
            &self.provider_info,
            loc,
        ) {
            InitializationValid::Valid => {}
            InitializationValid::NotWritten {
                possible_generic_escape_locs,
            } => {
                error(None, &possible_generic_escape_locs);
            }
            InitializationValid::NullWritten {
                null_provider_loc,
                possible_generic_escape_locs,
            } => {
                error(Some(null_provider_loc), &possible_generic_escape_locs);
            }
        }
    }

    fn values(&self) -> env_api::Values<ALoc> {
        let mut result = env_api::Values::new();
        for (loc, read_entry) in &self.env_state.values {
            let read = ssa_val::simplify(
                &mut *self.simplify_cache.borrow_mut(),
                read_entry.def_loc.dupe(),
                read_entry.val_binding_kind,
                read_entry.name.dupe(),
                &read_entry.value,
            );
            result.insert(loc.dupe(), read);
        }
        result
    }

    fn jsx_base_name(&self) -> Option<FlowSmolStr> {
        self.env_state.jsx_base_name.dupe()
    }

    fn take_write_entries(&mut self) -> EnvMap<ALoc, EnvEntry<ALoc>> {
        std::mem::replace(&mut self.env_state.write_entries, EnvMap::empty())
    }

    fn refinement_invalidation_info(&self) -> FlowOrdMap<ALoc, RefinementInvalidation> {
        self.env_state.refinement_invalidation_info.dupe()
    }

    fn take_type_guard_consistency_maps(&mut self) -> TypeGuardConsistencyMaps<ALoc> {
        std::mem::replace(
            &mut self.env_state.type_guard_consistency_maps,
            TypeGuardConsistencyMaps::new(),
        )
    }

    fn pred_func_map(&self) -> FlowOrdMap<ALoc, env_api::PredFuncInfo<ALoc>> {
        self.env_state.pred_func_map.dupe()
    }

    fn interface_merge_conflicts(&self) -> FlowOrdMap<ALoc, Vec<ALoc>> {
        self.env_state
            .interface_merge_conflicts
            .iter()
            .map(|(k, v)| (k.dupe(), v.clone()))
            .collect()
    }

    fn is_assigning_write(&self, key: &env_api::EnvKey<ALoc>) -> bool {
        self.env_state
            .write_entries
            .get(key)
            .is_some_and(|entry| match entry {
                EnvEntry::AssigningWrite(_) | EnvEntry::GlobalWrite(_) => true,
                EnvEntry::NonAssigningWrite => false,
            })
    }

    fn merge_vals_with_havoc(
        &self,
        havoc: &Val<ALoc>,
        def_loc: &Option<ALoc>,
        v1: &Val<ALoc>,
        v2: &Val<ALoc>,
    ) -> Val<ALoc> {
        let can_merge_with_havoc = |v_havoc: &Val<ALoc>, v_other: &Val<ALoc>| -> bool {
            ssa_val::id_of_val(v_havoc) == ssa_val::id_of_val(havoc)
                && ssa_val::writes_of_uninitialized(
                    |id| self.refinement_may_be_undefined(RefinementId(id)),
                    v_other,
                )
                .is_empty()
        };
        match def_loc {
            Some(loc) => match self.provider_info.providers_of_def(loc) {
                Some(def_providers)
                    if matches!(
                        def_providers.state,
                        flow_env_builder::find_providers::State::AnnotatedVar { .. }
                            | flow_env_builder::find_providers::State::InitializedVar
                    ) && (can_merge_with_havoc(v1, v2) || can_merge_with_havoc(v2, v1)) =>
                {
                    havoc.dupe()
                }
                _ => ssa_val::merge(&mut self.cache.borrow_mut(), v1.dupe(), v2.dupe()),
            },
            None => ssa_val::merge(&mut self.cache.borrow_mut(), v1.dupe(), v2.dupe()),
        }
    }

    fn is_excluded_ordinary_name(&self, name: &FlowSmolStr) -> bool {
        self.env_state.exclude_syms.contains(name)
    }

    fn new_id(&mut self) -> usize {
        let new_id = self.env_state.curr_id;
        self.env_state.curr_id += 1;
        new_id
    }

    fn should_invalidate(&self, all: bool, def_loc: &Option<ALoc>) -> bool {
        match def_loc {
            None => true,
            Some(loc) => invalidation_api::should_invalidate(
                all,
                &mut self.invalidation_caches.borrow_mut(),
                self.prepass_info,
                self.prepass_values,
                loc.dupe(),
            ),
        }
    }

    fn env_read(&self, x: &FlowSmolStr) -> EnvVal {
        self.env_state
            .env
            .env_read(|env_val| self.should_havoc_val_to_initialized(env_val), x)
    }

    fn env_read_opt(&self, x: &FlowSmolStr) -> Option<EnvVal> {
        self.env_state
            .env
            .env_read_opt(|env_val| self.should_havoc_val_to_initialized(env_val), x)
    }

    fn env_read_into_snapshot_from_below(&self, x: &FlowSmolStr) -> PartialEnvEntry {
        self.env_state
            .env
            .env_read_entry_from_below(|env_val| self.should_havoc_val_to_initialized(env_val), x)
    }

    fn partial_env_snapshot_read(
        &self,
        x: &FlowSmolStr,
        env: &PartialEnvSnapshot,
    ) -> PartialEnvEntry {
        partial_env_snapshot::read_with_fallback(x, env, |x| {
            self.env_read_into_snapshot_from_below(x)
        })
    }

    fn env_snapshot(&self) -> PartialEnvSnapshot {
        self.env_state
            .env
            .to_partial_env_snapshot(|_, v| partial_env_snapshot::Entry::of_env_val(v))
    }

    /// We often want to merge the refinement scopes and writes of two environments with
    /// different strategies, especially in logical refinement scopes. In order to do that, we
    /// need to be able to get the writes in our env without the refinement writes. Then we
    /// can merge the refinements from two environments using either AND or OR, and then we can
    /// merge the writes and reapply the merged refinement if the ssa_id in unchanged.
    ///
    /// An alternative implementation here might have just used PHI nodes to model disjunctions
    /// and successive refinement writes to model conjunctions, but it's not clear that that
    /// approach is simpler than this one.
    fn env_snapshot_without_latest_refinements(&self) -> PartialEnvSnapshot {
        fn refinements_by_key(
            refinement_maps: &RefinementMaps,
        ) -> BTreeMap<refinement_key::Lookup, BTreeSet<usize>> {
            let mut result = BTreeMap::new();
            for (lookup_key, refinement_id) in refinement_maps.applied.values() {
                result
                    .entry(lookup_key.dupe())
                    .or_insert_with(BTreeSet::new)
                    .insert(refinement_id.0);
            }
            result
        }

        fn unrefine(
            refinements_by_key: &BTreeMap<refinement_key::Lookup, BTreeSet<usize>>,
            lookup_key: &refinement_key::Lookup,
            v: Val<ALoc>,
        ) -> Val<ALoc> {
            match refinements_by_key.get(lookup_key) {
                Some(refinement_ids) => {
                    let mut result = v;
                    for &ref_id in refinement_ids {
                        result = ssa_val::unrefine(ref_id, result);
                    }
                    result
                }
                None => v,
            }
        }

        let head = self.env_state.latest_refinements.last().unwrap();
        if head.applied.is_empty() {
            return self.env_snapshot();
        }
        let refinements_by_key = refinements_by_key(head);
        self.env_state.env.to_partial_env_snapshot(|name, env_val| {
            let lookup_key = refinement_key::Lookup::of_name(name.dupe());
            let unrefined_env_val = unrefine(
                &refinements_by_key,
                &lookup_key,
                env_val.val_ref.borrow().dupe(),
            );
            let unrefined_heap_refinements = env_val
                .heap_refinements
                .borrow()
                .iter()
                .map(|(projections, heap_val)| {
                    let new_val = match heap_val {
                        Err(invalidation_info) => Err(invalidation_info.dupe()),
                        Ok(v) => {
                            let lookup_key = refinement_key::Lookup::of_name_with_projections(
                                name.dupe(),
                                projections.dupe(),
                            );
                            Ok(unrefine(&refinements_by_key, &lookup_key, v.dupe()))
                        }
                    };
                    (projections.dupe(), new_val)
                })
                .collect();
            partial_env_snapshot::Entry {
                env_val: unrefined_env_val,
                heap_refinements: unrefined_heap_refinements,
                def_loc: env_val.def_loc.dupe(),
            }
        })
    }

    fn merge_heap_refinements(
        &self,
        hr1: &HeapRefinementMap,
        hr2: &HeapRefinementMap,
    ) -> HeapRefinementMap {
        let mut result = FlowOrdMap::new();

        // When we merge the heap refinements from two branches we cannot include
        // keys that did not appear on both sides. Take this example:
        // let obj = {};
        // if (true) {
        //   obj.foo = 3;
        // } else {
        //   obj.bar = 4;
        // }
        // (obj.foo: 3); // Should fail because the else branch does not add this refinement
        for (key, v1) in hr1 {
            if let Some(v2) = hr2.get(key) {
                let merged = match (v1, v2) {
                    (Ok(val1), Ok(val2)) => Ok(ssa_val::merge(
                        &mut *self.cache.borrow_mut(),
                        val1.dupe(),
                        val2.dupe(),
                    )),
                    (Err(info), Ok(_)) | (Ok(_), Err(info)) => Err(info.dupe()),
                    (Err(info1), Err(info2)) => {
                        let mut merged_info = info1.dupe();
                        for (k, v) in info2.iter() {
                            if merged_info.get(k).is_none() {
                                merged_info.insert(k.dupe(), *v);
                            }
                        }
                        Err(merged_info)
                    }
                };
                result.insert(key.dupe(), merged);
            }
        }

        result
    }

    fn merge_remote_env(&self, env: &PartialEnvSnapshot) {
        // NOTE: env might have more keys than env_state.env, since the environment it
        // describes might be nested inside the current environment
        self.env_state.env.update_env(|x, env_val| {
            let entry = self.partial_env_snapshot_read(x, env);
            if env_val.def_loc == entry.def_loc {
                let merged_val = self.merge_vals_with_havoc(
                    &env_val.havoc,
                    &env_val.def_loc,
                    &env_val.val_ref.borrow(),
                    &entry.env_val,
                );
                *env_val.val_ref.borrow_mut() = merged_val;
                let merged_hr = self.merge_heap_refinements(
                    &env_val.heap_refinements.borrow(),
                    &entry.heap_refinements,
                );
                *env_val.heap_refinements.borrow_mut() = merged_hr;
            }
        });
    }

    fn merge_env(&self, env1: &PartialEnvSnapshot, env2: &PartialEnvSnapshot) {
        self.env_state.env.merge_env_with_partial_env_snapshots(
            |env_val| self.should_havoc_val_to_initialized(env_val),
            env1,
            env2,
            |e1, e2, env_val| {
                let merged_val = self.merge_vals_with_havoc(
                    &env_val.havoc,
                    &env_val.def_loc,
                    &e1.env_val,
                    &e2.env_val,
                );
                *env_val.val_ref.borrow_mut() = merged_val;
                let merged_hr =
                    self.merge_heap_refinements(&e1.heap_refinements, &e2.heap_refinements);
                *env_val.heap_refinements.borrow_mut() = merged_hr;
            },
        );
    }

    fn merge_self_env(&self, other_env: &PartialEnvSnapshot) {
        self.env_state.env.update_env_with_partial_env_snapshot(
            |env_val| self.should_havoc_val_to_initialized(env_val),
            other_env,
            |_x, entry, env_val| {
                let merged_val = self.merge_vals_with_havoc(
                    &env_val.havoc,
                    &env_val.def_loc,
                    &env_val.val_ref.borrow(),
                    &entry.env_val,
                );
                *env_val.val_ref.borrow_mut() = merged_val;
                let merged_hr = self.merge_heap_refinements(
                    &env_val.heap_refinements.borrow(),
                    &entry.heap_refinements,
                );
                *env_val.heap_refinements.borrow_mut() = merged_hr;
            },
        );
    }

    fn reset_env(&self, env0: &PartialEnvSnapshot) {
        self.env_state.env.update_env_with_partial_env_snapshot(
            |env_val| self.should_havoc_val_to_initialized(env_val),
            env0,
            |_x, entry, env_val| {
                entry.reset_val_with_entry(env_val);
            },
        );
    }

    fn reset_to_unreachable_env(&self) {
        self.env_state
            .env
            .reset_to_unreachable_env(&mut self.cache.borrow_mut());
    }

    /// This method applies a function over the value stored with a refinement key. It is
    /// mostly just a convenient helper so that the process of deconstructing the
    /// key and finding the appropriate Val.t does not have to be repeated in
    /// every method that needs to update an entry. The create_val_for_heap argument can
    /// be used to specify what value to apply the function to if the heap entry
    /// does not yet exist.
    ///
    /// In addition to updating the Val.t, if the callback Val.t updating function
    /// is called (i.e. if we find a value to update), we'll return any additional
    /// data that the function itself returns (as the first element of its return
    /// type tuple). In addition, if we end up creating a new val.t as part of a heap
    /// refinement, we'll return that val.t as well. In cases where this information
    /// isn't necessary, callers can call `map_val_with_lookup` below instead.
    fn map_val_with_lookup_result<R, F>(
        &mut self,
        lookup: &refinement_key::Lookup,
        create_val_for_heap: Option<impl FnOnce(&mut Self) -> Val<ALoc>>,
        f: F,
    ) -> Option<(R, Option<Val<ALoc>>)>
    where
        F: FnOnce(&mut Self, Val<ALoc>) -> (R, Val<ALoc>),
    {
        let base = &lookup.base;
        let projections = &lookup.projections;

        let env_val = self.env_read_opt(base)?;

        if projections.is_empty() {
            let val = env_val.val_ref.borrow().dupe();
            let (res, new_val) = f(self, val);
            let env_val = self.env_read_opt(base)?;
            *env_val.val_ref.borrow_mut() = new_val;
            Some((res, None))
        } else {
            let heap_refinements_ref = env_val.heap_refinements.borrow();
            let find_result = heap_refinements_ref.get(projections);

            let (res, new_heap_refinements) = match find_result {
                Some(Ok(heap_val)) => {
                    let val = heap_val.dupe();
                    drop(heap_refinements_ref);
                    let (res, new_val) = f(self, val);
                    let env_val = self.env_read_opt(base)?;
                    let mut new_hr = env_val.heap_refinements.borrow().dupe();
                    new_hr.insert(projections.dupe(), Ok(new_val));
                    (Some((res, None)), new_hr)
                }
                None | Some(Err(_)) => {
                    drop(heap_refinements_ref);
                    match create_val_for_heap {
                        Some(create_default) => {
                            let default = create_default(self);
                            let (res, new_val) = f(self, default.dupe());
                            let env_val = self.env_read_opt(base)?;
                            let mut new_hr = env_val.heap_refinements.borrow().dupe();
                            new_hr.insert(projections.dupe(), Ok(new_val));
                            (Some((res, Some(default))), new_hr)
                        }
                        None => {
                            let env_val = self.env_read_opt(base)?;
                            let hr = env_val.heap_refinements.borrow().dupe();
                            (None, hr)
                        }
                    }
                }
            };
            let env_val = self.env_read_opt(base)?;
            *env_val.heap_refinements.borrow_mut() = new_heap_refinements;
            res
        }
    }

    fn map_val_with_lookup(
        &mut self,
        lookup: &refinement_key::Lookup,
        create_val_for_heap: Option<impl FnOnce(&mut Self) -> Val<ALoc>>,
        f: impl FnOnce(&mut Self, Val<ALoc>) -> Val<ALoc>,
    ) {
        let _: Option<((), Option<Val<ALoc>>)> =
            self.map_val_with_lookup_result(lookup, create_val_for_heap, |resolver, v| {
                ((), f(resolver, v))
            });
    }

    /// None -> no val for expr
    /// Some (Ok v) -> val for exp
    /// Some (Error info) -> no val for expr, but we would get refinements
    /// if we don't aggressively invalidate heap refinements. info contains
    /// information on the invalidation
    fn get_val_of_expression_with_invalidated_refinement_snapshot(
        &self,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Option<Result<Val<ALoc>, flow_common::refinement_invalidation::RefinementInvalidation>>
    {
        let key = refinement_key::RefinementKey::of_expression(expr)?;
        let base = &key.lookup.base;
        let projections = &key.lookup.projections;
        let env_val = self.env_read_opt(base)?;
        let entry = partial_env_snapshot::Entry::of_env_val(&env_val);
        if projections.is_empty() {
            Some(Ok(entry.env_val))
        } else {
            match entry.heap_refinements.get(projections) {
                None => None,
                Some(Ok(v)) => {
                    if ssa_val::contains_bare_projection(v) {
                        None
                    } else {
                        Some(Ok(v.dupe()))
                    }
                }
                Some(Err(info)) => Some(Err(info.dupe())),
            }
        }
    }

    fn get_val_of_expression(
        &self,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Option<Val<ALoc>> {
        match self.get_val_of_expression_with_invalidated_refinement_snapshot(expr) {
            None => None,
            Some(Err(_)) => None,
            Some(Ok(v)) => Some(v),
        }
    }

    fn havoc_heap_refinements(heap_refinements: &Rc<RefCell<HeapRefinementMap>>) {
        if heap_refinements.borrow().is_empty() {
            return;
        }
        thread_local! {
            static CACHED: HeapRefinementMap = FlowOrdMap::new();
        }
        *heap_refinements.borrow_mut() = CACHED.with(|c| c.clone());
    }

    fn invalidate_heap_refinements(
        heap_refinements: &Rc<RefCell<HeapRefinementMap>>,
        invalidation_info: flow_common::refinement_invalidation::RefinementInvalidation,
    ) {
        let map = heap_refinements.borrow();
        if map.is_empty() {
            return;
        }
        let new_map: HeapRefinementMap = map
            .iter()
            .map(|(k, _)| (k.dupe(), Err(invalidation_info.dupe())))
            .collect();
        drop(map);
        *heap_refinements.borrow_mut() = new_map;
    }

    fn invalidate_all_heap_refinements_for_property_assignment(&mut self, loc: ALoc) {
        use flow_common::refinement_invalidation::Reason;
        let invalidation_info =
            flow_common::refinement_invalidation::singleton(loc, Reason::PropertyAssignment);

        self.env_state.env.update_env(|_, env_val| {
            Self::invalidate_heap_refinements(&env_val.heap_refinements, invalidation_info.dupe());
        });
    }

    fn havoc_current_env(
        &mut self,
        invalidation_reason: flow_common::refinement_invalidation::Reason,
        loc: ALoc,
    ) {
        use flow_common::refinement_invalidation::Reason;

        let cache = &mut *self.cache.borrow_mut();
        let invalidate_all = matches!(invalidation_reason, Reason::Yield);
        let invalidation_info =
            flow_common::refinement_invalidation::singleton(loc.dupe(), invalidation_reason);

        let havoced_ids: BTreeSet<usize> = self.env_state.env.fold_current_function_scope_values(
            BTreeSet::new(),
            |mut acc, env_val| {
                if env_val.kind == BindingsKind::Internal {
                    return acc;
                }

                if self.should_invalidate(invalidate_all, &env_val.def_loc) {
                    let val_is_undeclared_or_skipped =
                        ssa_val::is_undeclared_or_skipped(&env_val.val_ref.borrow());

                    let havoc_ref = if val_is_undeclared_or_skipped {
                        env_val.val_ref.borrow().dupe()
                    } else {
                        let uninitialized_writes = || {
                            ssa_val::writes_of_uninitialized(
                                |id| self.refinement_may_be_undefined(RefinementId(id)),
                                &env_val.val_ref.borrow(),
                            )
                        };

                        let havoc = match (invalidate_all, &env_val.writes_by_closure_provider_val)
                        {
                            (false, Some(writes_by_closure_provider_val)) => ssa_val::merge(
                                cache,
                                env_val.val_ref.borrow().dupe(),
                                writes_by_closure_provider_val.dupe(),
                            ),
                            _ => env_val.havoc.dupe(),
                        };
                        uninitialized_writes()
                            .into_iter()
                            .fold(havoc, |acc, write| {
                                let write_val = ssa_val::of_write(cache, write);
                                ssa_val::merge(cache, acc, write_val)
                            })
                    };
                    Self::havoc_heap_refinements(&env_val.heap_refinements);
                    *env_val.val_ref.borrow_mut() = havoc_ref.dupe();
                    acc.insert(ssa_val::base_id_of_val(&havoc_ref));
                } else {
                    Self::invalidate_heap_refinements(
                        &env_val.heap_refinements,
                        invalidation_info.dupe(),
                    );
                }
                acc
            },
        );

        if let Some(ref tg_info) = self.env_state.type_guard_name {
            if havoced_ids.contains(&tg_info.id) {
                let mut havoced = tg_info.havoced.borrow_mut();
                match &mut *havoced {
                    Some(set) => {
                        set.insert(loc.dupe());
                    }
                    None => {
                        let mut set = empty_refining_locs();
                        set.insert(loc.dupe());
                        *havoced = Some(set);
                    }
                }
            }
        }

        let latest_refinements: FlowVector<RefinementMaps> = self
            .env_state
            .latest_refinements
            .iter()
            .map(|rm| {
                let applied: FlowOrdMap<usize, (Lookup, RefinementId)> = rm
                    .applied
                    .iter()
                    .filter(|(ssa_id, (lookup, _))| {
                        lookup.projections.is_empty() && !havoced_ids.contains(ssa_id)
                    })
                    .map(|(k, v)| (*k, (v.0.dupe(), v.1)))
                    .collect();

                let total = if !applied.is_empty() {
                    Some(RefinementProp::refinements(
                        applied
                            .iter()
                            .map(|(k, v)| (*k, (v.0.dupe(), v.1)))
                            .collect(),
                        FlowOrdMap::new(),
                    ))
                } else {
                    None
                };
                RefinementMaps {
                    applied,
                    changeset: Changeset::default(),
                    total,
                }
            })
            .collect();
        self.env_state.latest_refinements = latest_refinements;
    }

    fn should_havoc_val_to_initialized(&self, env_val: &EnvVal) -> bool {
        if env_val.kind == BindingsKind::Internal {
            return false;
        }
        self.should_invalidate(true, &env_val.def_loc)
            || !ssa_val::writes_of_uninitialized(
                |id| self.refinement_may_be_undefined(RefinementId(id)),
                env_val.val_ref.borrow().deref(),
            )
            .is_empty()
            || ssa_val::is_undeclared_or_skipped(env_val.val_ref.borrow().deref())
    }

    fn under_uninitialized_env<T, F: FnOnce(&mut Self) -> T>(&mut self, f: F) -> T {
        self.env_state.env.push_new_function_scope();
        let result = f(self);
        self.env_state.env.pop_function_scope();
        result
    }

    fn refinement_may_be_undefined(&self, id: RefinementId) -> bool {
        fn refine_undefined(kind: &env_api::RefinementKind<ALoc>) -> bool {
            use env_api::RefinementKind;
            match kind {
                RefinementKind::UndefinedR | RefinementKind::MaybeR => true,
                RefinementKind::NotR(r) => !refine_undefined(r),
                RefinementKind::OrR(r1, r2) => refine_undefined(r1) || refine_undefined(r2),
                RefinementKind::AndR(r1, r2) => refine_undefined(r1) && refine_undefined(r2),
                _ => false,
            }
        }
        let Refinement {
            refining_locs: _,
            kind,
        } = self.refinement_of_id(id);
        refine_undefined(&kind)
    }

    fn providers_of_def_loc(
        &self,
        def_loc: ALoc,
        provider_info: &provider_api::Info<ALoc>,
    ) -> (Val<ALoc>, Vec<provider_api::Provider<ALoc>>) {
        let providers = provider_info
            .providers_of_def(&def_loc)
            .map(|def_info| def_info.providers.clone())
            .unwrap_or_default();

        let val = if providers.is_empty() {
            ssa_val::uninitialized(&mut *self.cache.borrow_mut(), def_loc)
        } else {
            ssa_val::providers(&mut *self.cache.borrow_mut(), providers.clone())
        };

        (val, providers)
    }

    fn mk_env(
        &mut self,
        this_super_binding_env: ThisSuperBindingEnv,
        bindings: &BTreeMap<FlowSmolStr, (BindingsKind, Vec1<(ALoc, BindingsKind)>)>,
    ) -> BTreeMap<FlowSmolStr, EnvVal> {
        use env_api::DefLocType;
        use env_api::EnvKey;
        use flow_common::reason::Name;
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;

        let mut result = BTreeMap::new();

        for (name, (kind, entries)) in bindings {
            let (loc, _) = entries.first();
            let kind_at_loc: BTreeMap<ALoc, BindingsKind> =
                entries.iter().map(|(l, k)| (l.dupe(), *k)).collect();

            let env_val = match kind {
                BindingsKind::Type { .. } | BindingsKind::Interface { .. } => {
                    let desc = VirtualReasonDesc::RType(Name::new(name.dupe()));
                    let reason = VirtualReason::new(desc, loc.dupe());
                    self.env_state.write_entries.insert(
                        EnvKey::new(DefLocType::OrdinaryNameLoc, loc.dupe()),
                        EnvEntry::AssigningWrite(reason.dupe()),
                    );
                    let val = ssa_val::one(&mut *self.cache.borrow_mut(), reason.dupe());
                    EnvVal::new(EnvValInner {
                        val_ref: Rc::new(RefCell::new(val.dupe())),
                        havoc: val,
                        writes_by_closure_provider_val: None,
                        def_loc: Some(loc.dupe()),
                        heap_refinements: empty_heap_refinements(),
                        kind: *kind,
                        kind_at_loc: kind_at_loc.clone(),
                    })
                }
                BindingsKind::ThisAnnot => {
                    let desc = VirtualReasonDesc::RThis;
                    let reason = VirtualReason::new(desc, loc.dupe());
                    self.env_state.write_entries.insert(
                        EnvKey::new(DefLocType::OrdinaryNameLoc, loc.dupe()),
                        EnvEntry::AssigningWrite(reason.dupe()),
                    );
                    let val = ssa_val::one(&mut *self.cache.borrow_mut(), reason.dupe());
                    EnvVal::new(EnvValInner {
                        val_ref: Rc::new(RefCell::new(val.dupe())),
                        havoc: val,
                        writes_by_closure_provider_val: None,
                        def_loc: Some(loc.dupe()),
                        heap_refinements: empty_heap_refinements(),
                        kind: *kind,
                        kind_at_loc: kind_at_loc.clone(),
                    })
                }
                BindingsKind::DeclaredClass
                | BindingsKind::DeclaredVar
                | BindingsKind::DeclaredLet
                | BindingsKind::DeclaredConst => {
                    let desc = VirtualReasonDesc::RIdentifier(Name::new(name.dupe()));
                    let reason = VirtualReason::new(desc, loc.dupe());
                    self.env_state.write_entries.insert(
                        EnvKey::new(DefLocType::OrdinaryNameLoc, loc.dupe()),
                        EnvEntry::AssigningWrite(reason.dupe()),
                    );
                    let val = ssa_val::one(&mut *self.cache.borrow_mut(), reason.dupe());
                    EnvVal::new(EnvValInner {
                        val_ref: Rc::new(RefCell::new(val.dupe())),
                        havoc: val,
                        writes_by_closure_provider_val: None,
                        def_loc: Some(loc.dupe()),
                        heap_refinements: empty_heap_refinements(),
                        kind: *kind,
                        kind_at_loc: kind_at_loc.clone(),
                    })
                }
                BindingsKind::Class | BindingsKind::Enum | BindingsKind::Record => {
                    let (havoc, providers) =
                        self.providers_of_def_loc(loc.dupe(), &self.provider_info);
                    for provider in &providers {
                        let provider_loc = provider.reason.loc().dupe();
                        self.env_state.write_entries.insert(
                            EnvKey::new(DefLocType::OrdinaryNameLoc, provider_loc),
                            EnvEntry::AssigningWrite(provider.reason.dupe()),
                        );
                    }
                    let val =
                        ssa_val::undeclared(&mut *self.cache.borrow_mut(), name.dupe(), loc.dupe());
                    EnvVal::new(EnvValInner {
                        val_ref: Rc::new(RefCell::new(val)),
                        havoc,
                        writes_by_closure_provider_val: None,
                        def_loc: Some(loc.dupe()),
                        heap_refinements: empty_heap_refinements(),
                        kind: *kind,
                        kind_at_loc: kind_at_loc.clone(),
                    })
                }
                BindingsKind::DeclaredFunction => {
                    let (_, providers) = self.providers_of_def_loc(loc.dupe(), &self.provider_info);
                    for provider in &providers {
                        let provider_loc = provider.reason.loc().dupe();
                        self.env_state.write_entries.insert(
                            EnvKey::new(DefLocType::OrdinaryNameLoc, provider_loc),
                            EnvEntry::AssigningWrite(provider.reason.dupe()),
                        );
                    }
                    let declared_function =
                        ssa_val::declared_function(&mut *self.cache.borrow_mut(), loc.dupe());
                    EnvVal::new(EnvValInner {
                        val_ref: Rc::new(RefCell::new(declared_function.dupe())),
                        havoc: declared_function,
                        writes_by_closure_provider_val: None,
                        def_loc: Some(loc.dupe()),
                        heap_refinements: empty_heap_refinements(),
                        kind: *kind,
                        kind_at_loc: kind_at_loc.clone(),
                    })
                }
                BindingsKind::Import => {
                    let desc = VirtualReasonDesc::RIdentifier(Name::new(name.dupe()));
                    let reason = VirtualReason::new(desc, loc.dupe());
                    let kind = if self.is_ts {
                        BindingsKind::TsImport
                    } else {
                        *kind
                    };
                    let val =
                        ssa_val::undeclared(&mut *self.cache.borrow_mut(), name.dupe(), loc.dupe());
                    EnvVal::new(EnvValInner {
                        val_ref: Rc::new(RefCell::new(val)),
                        havoc: ssa_val::one(&mut *self.cache.borrow_mut(), reason),
                        writes_by_closure_provider_val: None,
                        def_loc: Some(loc.dupe()),
                        heap_refinements: empty_heap_refinements(),
                        kind,
                        kind_at_loc: kind_at_loc.clone(),
                    })
                }
                BindingsKind::Internal => {
                    let val =
                        ssa_val::undeclared(&mut *self.cache.borrow_mut(), name.dupe(), loc.dupe());
                    EnvVal::new(EnvValInner {
                        val_ref: Rc::new(RefCell::new(val.dupe())),
                        havoc: val,
                        writes_by_closure_provider_val: None,
                        def_loc: None,
                        heap_refinements: empty_heap_refinements(),
                        kind: *kind,
                        kind_at_loc: kind_at_loc.clone(),
                    })
                }
                BindingsKind::GeneratorNext => {
                    let desc = VirtualReasonDesc::RNext;
                    let reason = VirtualReason::new(desc, loc.dupe());
                    self.env_state.write_entries.insert(
                        EnvKey::new(DefLocType::OrdinaryNameLoc, loc.dupe()),
                        EnvEntry::AssigningWrite(reason.dupe()),
                    );
                    let val = ssa_val::one(&mut *self.cache.borrow_mut(), reason.dupe());
                    EnvVal::new(EnvValInner {
                        val_ref: Rc::new(RefCell::new(val.dupe())),
                        havoc: val,
                        writes_by_closure_provider_val: None,
                        def_loc: Some(loc.dupe()),
                        heap_refinements: empty_heap_refinements(),
                        kind: *kind,
                        kind_at_loc: kind_at_loc.clone(),
                    })
                }
                _ => {
                    let (initial_val, havoc, writes_by_closure_provider_val) = if name.as_str()
                        == "this"
                    {
                        let desc = VirtualReasonDesc::RThis;
                        let reason = VirtualReason::new(desc, loc.dupe());
                        let v = match this_super_binding_env {
                            ThisSuperBindingEnv::FunctionEnv => {
                                ssa_val::function_this(&mut *self.cache.borrow_mut(), reason.dupe())
                            }
                            ThisSuperBindingEnv::ClassStaticEnv => ssa_val::class_static_this(
                                &mut *self.cache.borrow_mut(),
                                reason.dupe(),
                            ),
                            ThisSuperBindingEnv::ClassInstanceEnv => ssa_val::class_instance_this(
                                &mut *self.cache.borrow_mut(),
                                reason.dupe(),
                            ),
                            ThisSuperBindingEnv::IllegalThisEnv => {
                                ssa_val::illegal_this(&mut *self.cache.borrow_mut(), reason.dupe())
                            }
                        };
                        (v.dupe(), v, None)
                    } else if name.as_str() == "super" {
                        let desc = VirtualReasonDesc::RSuper;
                        let reason = VirtualReason::new(desc, loc.dupe());
                        let v = match this_super_binding_env {
                            ThisSuperBindingEnv::FunctionEnv => {
                                panic!(
                                    "Env_invariant: Cannot bind super in function env at {:?}",
                                    loc
                                )
                            }
                            ThisSuperBindingEnv::ClassStaticEnv => ssa_val::class_static_super(
                                &mut *self.cache.borrow_mut(),
                                reason.dupe(),
                            ),
                            ThisSuperBindingEnv::ClassInstanceEnv => ssa_val::class_instance_super(
                                &mut *self.cache.borrow_mut(),
                                reason.dupe(),
                            ),
                            ThisSuperBindingEnv::IllegalThisEnv => {
                                ssa_val::illegal_this(&mut *self.cache.borrow_mut(), reason.dupe())
                            }
                        };
                        (v.dupe(), v, None)
                    } else {
                        let initial_val = match kind {
                            // let/const/enum all introduce errors if you try to access or assign them
                            // before syntactically encountering the declaration. All other bindings
                            // do not, so we don't set them to be undeclared
                            BindingsKind::Let
                            | BindingsKind::Const
                            | BindingsKind::Enum
                            | BindingsKind::Parameter
                            | BindingsKind::ComponentParameter
                            | BindingsKind::Function
                            | BindingsKind::Component => ssa_val::undeclared(
                                &mut *self.cache.borrow_mut(),
                                name.dupe(),
                                loc.dupe(),
                            ),
                            _ => ssa_val::uninitialized(&mut *self.cache.borrow_mut(), loc.dupe()),
                        };
                        let (havoc, providers) =
                            self.providers_of_def_loc(loc.dupe(), &self.provider_info);

                        let writes_by_closure = invalidation_api::written_by_closure(
                            self.prepass_info,
                            self.prepass_values,
                            loc,
                        );
                        let provider_locs: ALocSet =
                            providers.iter().map(|p| p.reason.loc().dupe()).collect();
                        let all_writes_are_providers = writes_by_closure
                            .iter()
                            .all(|wl| provider_locs.contains(wl));

                        let writes_by_closure_provider_val = if all_writes_are_providers {
                            let writes_by_closure_providers: Vec<_> = providers
                                .iter()
                                .filter(|p| writes_by_closure.contains(p.reason.loc()))
                                .duped()
                                .collect();
                            if !writes_by_closure_providers.is_empty() {
                                Some(ssa_val::providers(
                                    &mut *self.cache.borrow_mut(),
                                    writes_by_closure_providers,
                                ))
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        (initial_val, havoc, writes_by_closure_provider_val)
                    };

                    EnvVal::new(EnvValInner {
                        val_ref: Rc::new(RefCell::new(initial_val)),
                        havoc,
                        writes_by_closure_provider_val,
                        def_loc: Some(loc.dupe()),
                        heap_refinements: empty_heap_refinements(),
                        kind: *kind,
                        kind_at_loc: kind_at_loc.clone(),
                    })
                }
            };

            result.insert(name.dupe(), env_val);
        }

        result.retain(|name, _| !self.is_excluded_ordinary_name(name));
        result
    }

    fn push_env(
        &mut self,
        this_super_binding_env: ThisSuperBindingEnv,
        bindings: &flow_analysis::bindings::Bindings<ALoc>,
    ) -> full_env::FunctionScopeLocalEnvSnapshot {
        let bindings = bindings.to_map();
        let new_bindings = self.mk_env(this_super_binding_env, &bindings);
        self.env_state.env.push_new_bindings(new_bindings)
    }

    fn pop_env(&mut self, snapshot: full_env::FunctionScopeLocalEnvSnapshot) {
        self.env_state.env.pop_bindings(snapshot);
    }

    fn with_scoped_bindings<T, F: FnOnce(&mut Self) -> Result<T, AbruptCompletion>>(
        &mut self,
        this_super_binding_env: ThisSuperBindingEnv,
        bindings: &flow_analysis::bindings::Bindings<ALoc>,
        visit: F,
    ) -> Result<T, AbruptCompletion> {
        let snapshot = self.push_env(this_super_binding_env, bindings);
        match visit(self) {
            Ok(v) => {
                self.pop_env(snapshot);
                Ok(v)
            }
            Err(e) => {
                self.pop_env(snapshot);
                Err(e)
            }
        }
    }

    fn run<
        T,
        F: FnOnce(&mut Self) -> Result<T, AbruptCompletion>,
        G: FnOnce(&mut Self) -> Result<(), AbruptCompletion>,
    >(
        &mut self,
        f: F,
        finally: G,
    ) -> Result<T, AbruptCompletion> {
        match f(self) {
            Ok(result) => {
                finally(self)?;
                Ok(result)
            }
            Err(e) => {
                finally(self)?;
                Err(e)
            }
        }
    }

    fn run_to_completion<F: FnOnce(&mut Self) -> Result<(), AbruptCompletion>>(
        &mut self,
        f: F,
    ) -> Option<AbruptCompletion> {
        f(self).err()
    }

    fn from_completion(completion: Option<AbruptCompletion>) -> Result<(), AbruptCompletion> {
        match completion {
            None => Ok(()),
            Some(abrupt_completion) => Err(abrupt_completion),
        }
    }

    fn raise_abrupt_completion<T>(
        &mut self,
        abrupt_completion: AbruptCompletion,
    ) -> Result<T, AbruptCompletion> {
        let env = self.env_snapshot();
        self.reset_to_unreachable_env();
        self.env_state
            .abrupt_completion_envs
            .push((abrupt_completion.dupe(), env));
        Err(abrupt_completion)
    }

    fn expecting_abrupt_completions<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let saved = std::mem::take(&mut self.env_state.abrupt_completion_envs);
        let saved_latest_refinements_len = self.env_state.latest_refinements.len();
        let result = f(self);
        let mut new_envs = std::mem::take(&mut self.env_state.abrupt_completion_envs);
        new_envs.extend(saved);
        self.env_state.abrupt_completion_envs = new_envs;
        self.env_state
            .latest_refinements
            .truncate(saved_latest_refinements_len);
        result
    }

    fn merge_completion_state_values(
        first: Option<&AbruptCompletion>,
        second: Option<&AbruptCompletion>,
    ) -> Option<AbruptCompletion> {
        match (first, second) {
            (Some(c1), Some(c2)) if c1 == c2 => Some(c1.dupe()),
            (Some(AbruptCompletion::Throw), Some(AbruptCompletion::Return))
            | (Some(AbruptCompletion::Return), Some(AbruptCompletion::Throw)) => {
                Some(AbruptCompletion::Return)
            }
            (Some(AbruptCompletion::Break(opt1)), Some(AbruptCompletion::Continue(opt2)))
            | (Some(AbruptCompletion::Continue(opt1)), Some(AbruptCompletion::Break(opt2)))
                if opt1 == opt2 =>
            {
                Some(AbruptCompletion::Continue(opt1.dupe()))
            }
            _ => None,
        }
    }

    /// Given multiple completion states, (re)raise if all are the same abrupt completion
    /// This function is called at merge points.
    fn merge_completion_states(
        &mut self,
        hd: Option<&AbruptCompletion>,
        tl: &[Option<AbruptCompletion>],
    ) -> Result<(), AbruptCompletion> {
        match hd {
            None => {}
            Some(abrupt_completion) => {
                let mut acc = Some(abrupt_completion.dupe());
                for item in tl {
                    acc = Self::merge_completion_state_values(acc.as_ref(), item.as_ref())
                }
                Self::from_completion(acc)?;
            }
        }
        Ok(())
    }

    /// Given a filter for particular abrupt completions to expect, find the saved
    /// environments corresponding to them, and merge those environments with the
    /// current environment. This function is called when exiting ASTs that
    /// introduce (and therefore expect) particular abrupt completions.
    fn commit_abrupt_completion_matching(
        &mut self,
        filter: impl Fn(&AbruptCompletion) -> bool,
        completion_state: Option<&AbruptCompletion>,
    ) -> Result<(), AbruptCompletion> {
        let (matching, non_matching): (Vec<_>, Vec<_>) = self
            .env_state
            .abrupt_completion_envs
            .iter()
            .duped()
            .partition(|(completion, _)| filter(completion));

        if !matching.is_empty() {
            for (_, env) in &matching {
                self.merge_remote_env(env);
            }
            self.env_state.abrupt_completion_envs = non_matching.into_iter().collect();
        } else if let Some(abrupt_completion) = completion_state {
            if !filter(abrupt_completion) {
                return Err(abrupt_completion.dupe());
            }
        }
        Ok(())
    }

    fn extends_in_infer_type<F, T>(&mut self, visit_type: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let saved_in_conditional_type_extends = self.env_state.in_conditional_type_extends;
        self.env_state.in_conditional_type_extends = true;
        let result = visit_type(self);
        self.env_state.in_conditional_type_extends = saved_in_conditional_type_extends;
        result
    }

    fn binding_infer_type_identifier(&mut self, loc: ALoc, name: &FlowSmolStr) {
        use env_api::DefLocType;
        use env_api::EnvKey;
        use flow_common::reason::Name;
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;

        let desc = VirtualReasonDesc::RType(Name::new(name.dupe()));
        let reason = VirtualReason::new(desc, loc.dupe());
        self.env_state.write_entries.insert(
            EnvKey::new(DefLocType::OrdinaryNameLoc, loc),
            EnvEntry::AssigningWrite(reason),
        );
    }

    // We want to translate object pattern destructing {a:{b:{c}}} = o into o.a.b.c,
    // so the use of refinement can be recorded as a write.
    // We use acc to keep track of the current parent expr
    fn binding_pattern_track_object_destructuring(
        &mut self,
        kind: Option<flow_parser::ast::VariableKind>,
        acc: flow_parser::ast::expression::Expression<ALoc, ALoc>,
        expr: &flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;
        use flow_parser::ast::pattern::Pattern;
        use flow_parser::ast::pattern::object::Property;

        let ploc = expr.loc().dupe();

        match expr {
            Pattern::Object { inner, .. } => {
                self.env_state.write_entries.insert(
                    env_api::EnvKey::new(env_api::DefLocType::PatternLoc, ploc.dupe()),
                    env_api::EnvEntry::AssigningWrite(VirtualReason::new(
                        VirtualReasonDesc::RDestructuring,
                        ploc.dupe(),
                    )),
                );

                for prop in inner.properties.iter() {
                    match prop {
                        Property::RestElement(rest) => {
                            self.pattern_object_rest_property(kind, rest)?;
                        }
                        Property::NormalProperty(prop_inner) => {
                            let key = &prop_inner.key;
                            let pattern = &prop_inner.pattern;
                            let default = &prop_inner.default;

                            fn handle_prop_with_name<Cx: Context, Fl: Flow<Cx = Cx>>(
                                this: &mut NameResolver<'_, Cx, Fl>,
                                default: &Option<
                                    flow_parser::ast::expression::Expression<ALoc, ALoc>,
                                >,
                                pattern: &flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
                                acc: flow_parser::ast::expression::Expression<ALoc, ALoc>,
                                kind: Option<flow_parser::ast::VariableKind>,
                                loc: ALoc,
                                name: FlowSmolStr,
                            ) -> Result<(), AbruptCompletion> {
                                if let Some(default_expr) = default {
                                    this.expression(default_expr)?;
                                }
                                let acc = flow_parser::ast::expression::Expression::new(flow_parser::ast::expression::ExpressionInner::Member {
                                    loc: loc.dupe(),
                                    inner: Arc::new(flow_parser::ast::expression::Member {
                                        object: acc ,
                                        property: flow_parser::ast::expression::member::Property::PropertyIdentifier(
                                            flow_parser::ast::Identifier::new(IdentifierInner {
                                                loc: loc.dupe(),
                                                name: name.dupe(),
                                                comments: None,
                                            }),
                                        ),
                                        comments: None,
                                    }),
                                });

                                match pattern {
                                    Pattern::Identifier {
                                        inner: id_inner, ..
                                    } => {
                                        let id_loc = id_inner.name.loc.dupe();
                                        let id_name = &id_inner.name.name;
                                        this.recursively_record_member_read(&acc);
                                        match this.get_val_of_expression(&acc) {
                                            None => {
                                                this.pattern(kind, pattern)?;
                                            }
                                            Some(refined_v) => {
                                                this.type_annotation_hint(&id_inner.annot)?;
                                                let write_kind = variable_declaration_binding_kind_to_pattern_write_kind(kind);
                                                let refined_v = refined_v.dupe();
                                                this.bind_pattern_identifier_customized(
                                                    write_kind,
                                                    id_loc,
                                                    id_name,
                                                    |cache, reason| {
                                                        let base = ssa_val::one(cache, reason);
                                                        ssa_val::replace_refinement_base_write(
                                                            cache,
                                                            base,
                                                            refined_v.dupe(),
                                                        )
                                                    },
                                                );
                                            }
                                        }
                                    }
                                    _ => {
                                        this.binding_pattern_track_object_destructuring(
                                            kind, acc, pattern,
                                        )?;
                                    }
                                }
                                Ok(())
                            }

                            use flow_parser::ast::pattern::object::Key;
                            match key {
                                Key::Identifier(id) => {
                                    handle_prop_with_name(
                                        self,
                                        default,
                                        pattern,
                                        acc.clone(),
                                        kind,
                                        id.loc.dupe(),
                                        id.name.dupe(),
                                    )?;
                                }
                                Key::StringLiteral((loc, lit)) => {
                                    handle_prop_with_name(
                                        self,
                                        default,
                                        pattern,
                                        acc.clone(),
                                        kind,
                                        loc.dupe(),
                                        lit.value.dupe(),
                                    )?;
                                }
                                Key::NumberLiteral((loc, lit)) => {
                                    if flow_common::js_number::is_float_safe_integer(lit.value) {
                                        let name =
                                            flow_common::js_number::ecma_string_of_float(lit.value);
                                        handle_prop_with_name(
                                            self,
                                            default,
                                            pattern,
                                            acc.clone(),
                                            kind,
                                            loc.dupe(),
                                            name.into(),
                                        )?;
                                    } else {
                                        self.pattern_object_property(kind, prop_inner)?;
                                    }
                                }
                                Key::BigIntLiteral(_) | Key::Computed(_) => {
                                    self.pattern_object_property(kind, prop_inner)?;
                                }
                            }
                        }
                    }
                }

                self.with_current_pattern_bindings(expr, |this| {
                    this.type_annotation_hint(&inner.annot)
                })?;
            }
            Pattern::Identifier { inner, .. } => {
                self.pattern_identifier_with_annot_check(kind, ploc, &inner.name, &inner.annot)?;
            }
            Pattern::Array { .. } | Pattern::Expression { .. } => {
                self.pattern(kind, expr)?;
            }
        }
        Ok(())
    }

    fn record_pattern_loc_writes(&mut self, expr: &flow_parser::ast::pattern::Pattern<ALoc, ALoc>) {
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;
        use flow_parser::ast::pattern::Pattern;

        let ploc = expr.loc().dupe();

        let reason = VirtualReason::new(VirtualReasonDesc::RDestructuring, ploc.dupe());
        self.env_state.write_entries.insert(
            env_api::EnvKey::new(env_api::DefLocType::PatternLoc, ploc),
            env_api::EnvEntry::AssigningWrite(reason),
        );

        match expr {
            Pattern::Array { inner, .. } => {
                for element in inner.elements.iter() {
                    use flow_parser::ast::pattern::array::Element;
                    match element {
                        Element::Hole(_) => {}
                        Element::NormalElement(e) => {
                            self.record_pattern_loc_writes(&e.argument);
                        }
                        Element::RestElement(r) => {
                            self.record_pattern_loc_writes(&r.argument);
                        }
                    }
                }
            }
            Pattern::Object { inner, .. } => {
                for prop in inner.properties.iter() {
                    use flow_parser::ast::pattern::object::Property;
                    match prop {
                        Property::NormalProperty(p) => {
                            self.record_pattern_loc_writes(&p.pattern);
                        }
                        Property::RestElement(r) => {
                            self.record_pattern_loc_writes(&r.argument);
                        }
                    }
                }
            }
            Pattern::Identifier { .. } => {}
            Pattern::Expression { .. } => {}
        }
    }

    fn pattern_identifier_with_annot_check(
        &mut self,
        kind: Option<flow_parser::ast::VariableKind>,
        ploc: ALoc,
        name: &flow_parser::ast::Identifier<ALoc, ALoc>,
        annot: &flow_parser::ast::types::AnnotationOrHint<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;

        let reason = VirtualReason::new(VirtualReasonDesc::RDestructuring, ploc.dupe());
        self.env_state.write_entries.insert(
            env_api::EnvKey::new(env_api::DefLocType::PatternLoc, ploc),
            env_api::EnvEntry::AssigningWrite(reason),
        );

        self.pattern_identifier(kind, name)?;
        self.with_current_id_binding(name, |this| this.type_annotation_hint(annot))
    }

    fn bind_pattern_identifier_customized<F>(
        &mut self,
        kind: PatternWriteKind,
        loc: ALoc,
        x: &FlowSmolStr,
        get_assigned_val: F,
    ) where
        F: FnOnce(&mut ValCache<ALoc>, flow_common::reason::VirtualReason<ALoc>) -> Val<ALoc>,
    {
        use flow_common::reason::Name;
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;

        let reason = VirtualReason::new(
            VirtualReasonDesc::RIdentifier(Name::new(x.dupe())),
            loc.dupe(),
        );

        let env_val = self.env_read(x);
        let val_ref = env_val.val_ref.dupe();
        let stored_binding_kind = env_val.kind;
        let def_loc = env_val.def_loc.dupe();

        match kind {
            // Assignments to undeclared bindings that aren't part of declarations do not
            // initialize those bindings.
            PatternWriteKind::AssignmentWrite
                if ssa_val::is_undeclared_or_skipped(&val_ref.borrow()) =>
            {
                match def_loc {
                    None => {
                        panic!(
                            "Cannot have an undeclared or skipped binding without a def loc at {:?}",
                            loc
                        );
                    }
                    Some(def_loc) => {
                        Fl::add_output(
                            self.cx,
                            ErrorMessage::EBindingError(Box::new((
                                BindingError::EReferencedBeforeDeclaration,
                                loc.dupe(),
                                Name::new(x.dupe()),
                                def_loc,
                            ))),
                        );
                    }
                }
            }
            _ => {
                let err = error_for_assignment_kind(
                    x,
                    loc.dupe(),
                    def_loc.dupe(),
                    stored_binding_kind,
                    kind,
                    &val_ref.borrow(),
                    self.enable_const_params,
                );
                match err {
                    Some(err) => {
                        self.error_assignment(
                            loc.dupe(),
                            x,
                            reason,
                            stored_binding_kind,
                            kind,
                            err,
                            &val_ref,
                        );
                    }
                    None => {
                        Self::havoc_heap_refinements(&env_val.heap_refinements);
                        let current_val = val_ref.borrow().dupe();
                        if !ssa_val::is_declared_function(&current_val)
                            && !self.is_excluded_ordinary_name(x)
                        {
                            *val_ref.borrow_mut() =
                                get_assigned_val(&mut self.cache.borrow_mut(), reason.dupe());
                        }
                        let write_entry = if ssa_val::is_global(&current_val) {
                            env_api::EnvEntry::GlobalWrite(reason)
                        } else {
                            env_api::EnvEntry::AssigningWrite(reason)
                        };
                        self.env_state.write_entries.insert(
                            env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, loc.dupe()),
                            write_entry,
                        );
                    }
                }
            }
        }
    }

    fn error_assignment(
        &mut self,
        loc: ALoc,
        x: &FlowSmolStr,
        reason: flow_common::reason::VirtualReason<ALoc>,
        stored_binding_kind: BindingsKind,
        kind: PatternWriteKind,
        err: ErrorMessage<ALoc>,
        val_ref: &Rc<RefCell<Val<ALoc>>>,
    ) {
        Fl::add_output(self.cx, err);
        let write_entry = match kind {
            PatternWriteKind::ClassBinding => {
                // Record duplicate classes as assigning writes so that in class_identifier_opt
                // we can install entries for "this" and "super".
                if ssa_val::is_global(&val_ref.borrow()) {
                    env_api::EnvEntry::GlobalWrite(reason.dupe())
                } else {
                    env_api::EnvEntry::AssigningWrite(reason.dupe())
                }
            }
            _ => env_api::EnvEntry::NonAssigningWrite,
        };
        self.env_state.write_entries.insert(
            env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, loc.dupe()),
            write_entry,
        );
        // Give unsupported var redeclaration a write to avoid spurious errors like use of
        // possibly undefined variable. Essentially, we are treating var redeclaration as a
        // assignment with an any-typed value.
        if matches!(stored_binding_kind, BindingsKind::Var)
            && matches!(kind, PatternWriteKind::VarBinding)
            && !self.is_excluded_ordinary_name(x)
        {
            *val_ref.borrow_mut() = ssa_val::illegal_write(&mut *self.cache.borrow_mut(), reason);
        }
    }

    /// This method is called during every read of an identifier. We need to ensure that
    /// if the identifier is refined that we record the refiner as the write that reaches
    /// this read
    ///
    /// Note that we don't emit EBinding errors for referenced-before-declaration errors here.
    /// That is because we may read an UndeclaredTypeAndValue from a type position and the
    /// name_resolver doesn't keep track of whether we are in a type context or not.
    ///
    /// Instead of augmenting the name_resolver with those capabilities, we emit these errors
    /// in the new_env, which does know if it's querying a value or a type.
    fn any_identifier(&mut self, loc: ALoc, name: &FlowSmolStr) {
        let env_val = self.env_read(name);
        let v = if self.env_state.visiting_hoisted_type {
            env_val.havoc.dupe()
        } else {
            env_val.val_ref.borrow().dupe()
        };
        let entry = ReadEntry {
            def_loc: env_val.def_loc.dupe(),
            value: v,
            val_binding_kind: ssa_val::ValBindingKind::SourceLevelBinding(env_val.kind),
            name: Some(name.dupe()),
        };
        self.env_state.values.insert(loc, entry);
    }

    fn with_current_pattern_bindings<F, R>(
        &mut self,
        pattern: &flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
        f: F,
    ) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let bindings = flow_env_builder::pattern_helper::bindings_of_pattern(pattern);
        let current_bindings: FlowOrdMap<ALoc, FlowSmolStr> = bindings
            .into_iter()
            .map(|(name, (loc, _))| (loc, name))
            .collect();
        let old_val = std::mem::replace(&mut self.env_state.current_bindings, current_bindings);
        let result = f(self);
        self.env_state.current_bindings = old_val;
        result
    }

    fn with_current_id_binding<F, R>(
        &mut self,
        id: &flow_parser::ast::Identifier<ALoc, ALoc>,
        f: F,
    ) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let loc = id.loc.dupe();
        let name = id.name.dupe();
        let mut current_bindings = FlowOrdMap::new();
        current_bindings.insert(loc, name);
        let old_val = std::mem::replace(&mut self.env_state.current_bindings, current_bindings);
        let result = f(self);
        self.env_state.current_bindings = old_val;
        result
    }

    fn with_in_param_default<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let old_in_param_default = self.env_state.in_param_default;
        self.env_state.in_param_default = true;
        let result = f(self);
        self.env_state.in_param_default = old_in_param_default;
        result
    }

    fn visit_default_with_pattern_bindings(
        &mut self,
        pattern: &flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
        default_expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.with_current_pattern_bindings(pattern, |this| {
            this.with_in_param_default(|this| this.expression(default_expr))
        })
    }

    fn error_on_reference_to_currently_declared_id(
        &mut self,
        id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) {
        use flow_common::reason::Name;
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;

        let loc = id.loc.dupe();
        let name = &id.name;
        let env_val = self.env_read(name);
        let val_ref = env_val.val_ref.dupe();
        let def_loc_opt = env_val.def_loc.dupe();
        if let Some(def_loc) = def_loc_opt {
            match self.env_state.current_bindings.get(&def_loc) {
                None => {}
                Some(binding_name) => {
                    let reason = VirtualReason::new(
                        VirtualReasonDesc::RIdentifier(Name::new(binding_name.dupe())),
                        def_loc.dupe(),
                    );
                    self.env_state.write_entries.insert(
                        env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, def_loc.dupe()),
                        env_api::EnvEntry::NonAssigningWrite,
                    );
                    *val_ref.borrow_mut() =
                        ssa_val::illegal_write(&mut *self.cache.borrow_mut(), reason);
                    Fl::add_output(
                        self.cx,
                        ErrorMessage::EReferenceInAnnotation(Box::new((
                            def_loc,
                            binding_name.dupe(),
                            loc,
                        ))),
                    );
                }
            }
        }
    }

    fn error_on_reference_to_currently_declared_id_in_default(
        &mut self,
        id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) {
        let loc = id.loc.dupe();
        let name = &id.name;
        let env_val = self.env_read(name);
        let def_loc_opt = env_val.def_loc.dupe();
        if let Some(def_loc) = def_loc_opt {
            match self.env_state.current_bindings.get(&def_loc) {
                None => {}
                Some(binding_name) => {
                    Fl::add_output(
                        self.cx,
                        ErrorMessage::EReferenceInDefault(Box::new((
                            def_loc,
                            binding_name.dupe(),
                            loc,
                        ))),
                    );
                }
            }
        }
    }

    fn invalidate_heap_refinements_using_name(
        &mut self,
        name: &str,
        invalidation_loc: ALoc,
        private_: bool,
    ) {
        use flow_common::refinement_invalidation::Reason;

        let invalidation_info = flow_common::refinement_invalidation::singleton(
            invalidation_loc,
            Reason::PropertyAssignment,
        );

        self.env_state.env.update_env(|_, env_val| {
            let heap_refinements = env_val.heap_refinements.borrow();
            if heap_refinements.is_empty() {
                return;
            }
            let new_heap: FlowOrdMap<_, _> = heap_refinements
                .iter()
                .map(|(projections, entry)| {
                    let new_entry = match entry {
                        Err(existing_invalidation_info) => {
                            Err(flow_common::refinement_invalidation::union(
                                existing_invalidation_info.dupe(),
                                invalidation_info.dupe(),
                            ))
                        }
                        Ok(v) => {
                            if !refinement_key::Lookup::proj_uses_propname(
                                projections,
                                name,
                                private_,
                            ) {
                                Ok(v.dupe())
                            } else {
                                Err(invalidation_info.dupe())
                            }
                        }
                    };
                    (projections.dupe(), new_entry)
                })
                .collect();
            drop(heap_refinements);
            *env_val.heap_refinements.borrow_mut() = new_heap;
        });
    }

    // This function should be called _after_ a member expression is assigned a value.
    // It havocs other heap refinements depending on the name of the member and then adds
    // a write to the heap refinement entry for that member expression
    fn assign_expression(
        &mut self,
        assign_loc: ALoc,
        lhs: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        rhs: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;
        use flow_parser::ast::expression::ExpressionInner;

        match lhs.deref() {
            ExpressionInner::Member { loc, inner } => {
                let loc = loc.dupe();
                // Use super member to visit sub-expressions to avoid record a read of the member.
                ast_visitor::member_default(self, &loc, inner)?;
                self.expression(rhs)?;
                let reason = VirtualReason::new(VirtualReasonDesc::RSomeProperty, loc.dupe());
                let assigned_val = ssa_val::one(&mut *self.cache.borrow_mut(), reason.dupe());
                self.assign_member(
                    false,
                    inner,
                    assign_loc,
                    loc.dupe(),
                    assigned_val,
                    reason.dupe(),
                );
                match &**rhs {
                    ExpressionInner::ArrowFunction { loc: fun_loc, .. } => {
                        let reason = VirtualReason::new(
                            VirtualReasonDesc::RFunction(
                                flow_common::reason::ReasonDescFunction::RNormal,
                            ),
                            fun_loc.dupe(),
                        );
                        self.env_state.write_entries.insert(
                            env_api::EnvKey::new(
                                env_api::DefLocType::OrdinaryNameLoc,
                                fun_loc.dupe(),
                            ),
                            env_api::EnvEntry::AssigningWrite(reason),
                        );
                    }
                    _ => {}
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn assign_member(
        &mut self,
        delete: bool,
        lhs_member: &flow_parser::ast::expression::Member<ALoc, ALoc>,
        assign_loc: ALoc,
        lhs_loc: ALoc,
        assigned_val: Val<ALoc>,
        val_reason: flow_common::reason::VirtualReason<ALoc>,
    ) {
        use flow_parser::ast::expression::ExpressionInner;
        use flow_parser::ast::expression::member::Property;

        self.post_assignment_heap_refinement_havoc(assign_loc.dupe(), lhs_member);

        let lookup = refinement_key::Lookup::of_member(lhs_member, false);

        let property_matches = match &lhs_member.property {
            Property::PropertyIdentifier(_) | Property::PropertyPrivateName(_) => true,
            Property::PropertyExpression(expr) => matches!(
                expr.deref(),
                ExpressionInner::StringLiteral { .. } | ExpressionInner::NumberLiteral { .. }
            ),
        };

        match (property_matches, lookup) {
            (true, Some(lookup)) => {
                let assigned_val_for_heap = assigned_val.dupe();
                self.map_val_with_lookup(
                    &lookup,
                    Some(move |_: &mut Self| assigned_val_for_heap),
                    |_, _| assigned_val.dupe(),
                );
                if !delete {
                    self.env_state.write_entries.insert(
                        env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, lhs_loc),
                        env_api::EnvEntry::AssigningWrite(val_reason),
                    );
                }
            }
            _ => {}
        }
    }

    // This method is called after assigning a member expression but _before_ the refinement for
    // that assignment is recorded.
    fn post_assignment_heap_refinement_havoc(
        &mut self,
        assign_loc: ALoc,
        lhs: &flow_parser::ast::expression::Member<ALoc, ALoc>,
    ) {
        use flow_parser::ast::expression::member::Property;

        // match lhs with
        match &lhs.property {
            Property::PropertyPrivateName(pn) => {
                // Yes, we want to havoc using the PROPERTY name here. This is because we
                // do not do any alias tracking, so we want to have the following behavior:
                // let x = {};
                // let y = x;
                // x.foo = 3;
                // y.foo = 4;
                // (x.foo: 3) // MUST error!
                self.invalidate_heap_refinements_using_name(&pn.name, assign_loc, true);
            }
            Property::PropertyIdentifier(id) => {
                // As in the previous case, we can't know if this object is aliased nor what property
                // is being written. We are forced to conservatively havoc ALL heap refinements in this
                // situation.
                self.invalidate_heap_refinements_using_name(&id.name, assign_loc, false);
            }
            Property::PropertyExpression(_) => {
                self.invalidate_all_heap_refinements_for_property_assignment(assign_loc);
            }
        }
    }

    fn is_refining_read(&self, read: &env_api::Read<ALoc>) -> bool {
        read.write_locs
            .iter()
            .any(|wl| matches!(wl, env_api::WriteLoc::Refinement { .. }))
    }

    fn record_type_guard_maps(
        &mut self,
        tg_info: &TypeGuardNameInfo,
        return_reason: flow_common::reason::VirtualReason<ALoc>,
        ret_expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let guard_param_loc = tg_info.loc.dupe();
        let name = &tg_info.name;
        let havoced = tg_info.havoced.borrow();
        let inferred = tg_info.inferred;

        self.push_refinement_scope(RefinementMaps::empty());
        self.expression_refinement(ret_expr)?;
        let positive_read = self.synthesize_read(name);
        if !inferred || self.is_refining_read(&positive_read) {
            self.negate_new_refinements();
            let negative_read = self.synthesize_read(name);

            let entry = self
                .env_state
                .type_guard_consistency_maps
                .entry(guard_param_loc.dupe())
                .or_insert_with(|| (None, Vec::new()));

            match (&mut entry.0, &*havoced) {
                (None, Some(h)) => entry.0 = Some(h.dupe()),
                (Some(existing), Some(h)) => {
                    existing.extend(h.iter().duped());
                }
                _ => {}
            }

            entry.1.push(env_api::TypeGuardConsistencyEntry(
                ret_expr.clone(),
                return_reason,
                positive_read,
                negative_read,
            ));
        }

        self.pop_refinement_scope();
        Ok(())
    }

    fn visit_match_case<'ast, B, F>(
        &mut self,
        case: &'ast flow_parser::ast::match_::Case<ALoc, ALoc, B>,
        on_case_body: &mut F,
    ) -> (
        PartialEnvSnapshot,
        PartialEnvSnapshot,
        Option<AbruptCompletion>,
    )
    where
        F: FnMut(&mut Self, &'ast B) -> Result<(), AbruptCompletion>,
    {
        let pattern = &case.pattern;
        let body = &case.body;
        let guard = &case.guard;
        let case_match_root_loc = &case.case_match_root_loc;

        let env0 = self.env_snapshot();

        let mut lexical_hoist = LexicalHoister::new(self.enable_enums);
        let Ok(()) = lexical_hoist.match_pattern(pattern);
        let bindings = lexical_hoist.into_bindings();

        let completion_state = self
            .with_bindings(true, case_match_root_loc.dupe(), bindings, |r| {
                r.push_refinement_scope(empty_refinements());

                let arg = flow_parser::ast::expression::Expression::new(
                    flow_parser::ast::expression::ExpressionInner::Identifier {
                        loc: case_match_root_loc.dupe(),
                        inner: ast_utils::match_root_ident::<ALoc, ALoc>(
                            case_match_root_loc.dupe(),
                        ),
                    },
                );

                r.visit_match_pattern(&arg, pattern)?;

                match r.get_val_of_expression(&arg) {
                    Some(refined_value) => {
                        r.env_state.values.insert(
                            case_match_root_loc.dupe(),
                            ReadEntry {
                                def_loc: None,
                                value: refined_value,
                                val_binding_kind: ssa_val::ValBindingKind::InternalBinding,
                                name: None,
                            },
                        );
                    }
                    None => {}
                }
                let completion_state = r.run_to_completion(|resolver| {
                    if let Some(g) = guard {
                        resolver.expression_refinement(g)?;
                    }
                    on_case_body(resolver, body)?;
                    Ok(())
                });

                Ok(completion_state)
            })
            .unwrap_or(None);

        let body_env_no_refinements = self.env_snapshot_without_latest_refinements();
        let body_env_with_refinements = self.env_snapshot();
        self.pop_refinement_scope();
        self.reset_env(&env0);

        (
            body_env_no_refinements,
            body_env_with_refinements,
            completion_state,
        )
    }

    fn visit_match_pattern(
        &mut self,
        arg: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        root_pattern: &flow_parser::ast::match_pattern::MatchPattern<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;
        use flow_parser::ast::VariableKind;
        use flow_parser::ast::expression::Expression;
        use flow_parser::ast::expression::ExpressionInner;
        use flow_parser::ast::expression::Member;
        use flow_parser::ast::expression::member::Property as MemberProperty;
        use flow_parser::ast::match_pattern::MatchPattern;
        use flow_parser::ast::match_pattern::RestPattern;
        use flow_parser::ast::match_pattern::object_pattern;

        type BindingsMap =
            BTreeMap<FlowSmolStr, Vec<(VariableKind, flow_parser::ast::Identifier<ALoc, ALoc>)>>;

        {
            let root_loc = root_pattern.loc().dupe();
            let reason = flow_common::reason::mk_reason(
                flow_common::reason::VirtualReasonDesc::RMatchPattern,
                root_loc.dupe(),
            );
            let key = env_api::EnvKey::new(env_api::DefLocType::MatchCasePatternLoc, root_loc);
            self.env_state
                .write_entries
                .insert(key, env_api::EnvEntry::AssigningWrite(reason));
        }

        fn add_binding(
            kind: VariableKind,
            id: &flow_parser::ast::Identifier<ALoc, ALoc>,
            bindings: &mut BindingsMap,
        ) {
            let name = id.name.dupe();
            bindings
                .entry(name)
                .and_modify(|v| v.push((kind, id.dupe())))
                .or_insert_with(|| vec![(kind, id.dupe())]);
        }

        fn bindings_of_rest(bindings: &mut BindingsMap, rest: &Option<RestPattern<ALoc, ALoc>>) {
            if let Some(RestPattern {
                argument: Some((_, binding)),
                ..
            }) = rest
            {
                add_binding(binding.kind, &binding.id, bindings);
            }
        }

        fn needs_prop_exists_refi(pattern: &MatchPattern<ALoc, ALoc>) -> Option<ALoc> {
            match pattern {
                MatchPattern::WildcardPattern { loc, .. }
                | MatchPattern::BindingPattern { loc, .. }
                | MatchPattern::IdentifierPattern { loc, .. }
                | MatchPattern::MemberPattern { loc, .. }
                | MatchPattern::ObjectPattern { loc, .. }
                | MatchPattern::InstancePattern { loc, .. }
                | MatchPattern::ArrayPattern { loc, .. } => Some(loc.dupe()),
                MatchPattern::NumberPattern { .. }
                | MatchPattern::BigIntPattern { .. }
                | MatchPattern::StringPattern { .. }
                | MatchPattern::BooleanPattern { .. }
                | MatchPattern::NullPattern { .. }
                | MatchPattern::UnaryPattern { .. } => None,
                MatchPattern::AsPattern { inner, .. } => needs_prop_exists_refi(&inner.pattern),
                MatchPattern::OrPattern { inner, .. } => {
                    inner.patterns.iter().find_map(needs_prop_exists_refi)
                }
            }
        }

        struct LocalHelpers<'a, 'b, Cx: Context, Fl: Flow<Cx = Cx>> {
            resolver: &'a mut NameResolver<'b, Cx, Fl>,
        }

        impl<Cx: Context, Fl: Flow<Cx = Cx>> LocalHelpers<'_, '_, Cx, Fl> {
            fn eq_refinement(
                &mut self,
                acc: &Expression<ALoc, ALoc>,
                loc: ALoc,
                refis: BTreeMap<Lookup, (ALoc, Refinement<ALoc>)>,
            ) -> BTreeMap<Lookup, (ALoc, Refinement<ALoc>)> {
                match refinement_key::RefinementKey::of_expression(acc) {
                    Some(key) => {
                        let reason = flow_common::reason::mk_reason(key.reason_desc(), loc.dupe());
                        let entry_key =
                            env_api::EnvKey::new(env_api::DefLocType::ExpressionLoc, loc.dupe());
                        self.resolver
                            .env_state
                            .write_entries
                            .insert(entry_key, env_api::EnvEntry::AssigningWrite(reason));
                        let mut refining_locs = empty_refining_locs();
                        refining_locs.insert(loc.dupe());
                        self.resolver.extend_refinement(
                            &key,
                            refining_locs,
                            RefinementKind::EqR(loc),
                            refis,
                        )
                    }
                    None => refis,
                }
            }

            fn impossible_refinement(&mut self, acc: &Expression<ALoc, ALoc>, loc: ALoc) {
                if let Some(key) = refinement_key::RefinementKey::of_expression(acc) {
                    if key.lookup.projections.is_empty() {
                        let mut refining_locs = empty_refining_locs();
                        refining_locs.insert(loc);
                        self.resolver.add_single_refinement(
                            &key,
                            refining_locs,
                            RefinementKind::NotR(Rc::new(RefinementKind::ImpossibleR)),
                        );
                    }
                }
            }

            fn check_invalid_reference(
                &mut self,
                bindings: &BindingsMap,
                id: &flow_parser::ast::Identifier<ALoc, ALoc>,
            ) {
                let loc = id.loc.dupe();
                let name = &id.name;
                if let Some(binding_list) = bindings.get(name) {
                    if let Some((_, first_binding)) = binding_list.first() {
                        let binding_loc = first_binding.loc.dupe();
                        let binding_reason = flow_common::reason::mk_reason(
                            flow_common::reason::VirtualReasonDesc::RIdentifier(
                                flow_common::reason::Name::new(name.dupe()),
                            ),
                            binding_loc,
                        );
                        Fl::add_output(
                            self.resolver.cx,
                            ErrorMessage::EMatchError(
                                MatchErrorKind::MatchInvalidPatternReference(Box::new(
                                    MatchInvalidPatternReferenceData {
                                        loc,
                                        binding_reason,
                                    },
                                )),
                            ),
                        );
                    }
                }
            }

            fn recurse(
                &mut self,
                acc: &Expression<ALoc, ALoc>,
                pattern: &MatchPattern<ALoc, ALoc>,
                bindings: &mut BindingsMap,
                non_binding_leaves: &mut ALocSet,
            ) -> Result<(), AbruptCompletion> {
                {
                    let loc = pattern.loc().dupe();
                    let reason = flow_common::reason::mk_reason(
                        flow_common::reason::VirtualReasonDesc::RDestructuring,
                        loc.dupe(),
                    );
                    let key = env_api::EnvKey::new(env_api::DefLocType::PatternLoc, loc);
                    self.resolver
                        .env_state
                        .write_entries
                        .insert(key, env_api::EnvEntry::AssigningWrite(reason));
                }

                match pattern {
                    MatchPattern::NumberPattern { loc, inner } => {
                        let lit = (inner.value, inner.raw.dupe());
                        let refi = RefinementKind::SingletonNumR {
                            loc: loc.dupe(),
                            sense: true,
                            lit,
                        };
                        let expr = Expression::new(ExpressionInner::NumberLiteral {
                            loc: loc.dupe(),
                            inner: Arc::new(inner.as_ref().clone()),
                        });
                        self.resolver
                            .literal_test(true, true, loc.dupe(), acc, refi, &expr)?;
                        non_binding_leaves.insert(loc.dupe());
                    }
                    MatchPattern::BigIntPattern { loc, inner } => {
                        let lit = (inner.value, inner.raw.dupe());
                        let refi = RefinementKind::SingletonBigIntR {
                            loc: loc.dupe(),
                            sense: true,
                            lit,
                        };
                        let expr = Expression::new(ExpressionInner::BigIntLiteral {
                            loc: loc.dupe(),
                            inner: Arc::new(inner.as_ref().clone()),
                        });
                        self.resolver
                            .literal_test(true, true, loc.dupe(), acc, refi, &expr)?;
                        non_binding_leaves.insert(loc.dupe());
                    }
                    MatchPattern::StringPattern { loc, inner } => {
                        let refi = RefinementKind::SingletonStrR {
                            loc: loc.dupe(),
                            sense: true,
                            lit: inner.value.dupe(),
                        };
                        let expr = Expression::new(ExpressionInner::StringLiteral {
                            loc: loc.dupe(),
                            inner: Arc::new(inner.as_ref().clone()),
                        });
                        self.resolver
                            .literal_test(true, true, loc.dupe(), acc, refi, &expr)?;
                        non_binding_leaves.insert(loc.dupe());
                    }
                    MatchPattern::BooleanPattern { loc, inner } => {
                        let refi = RefinementKind::SingletonBoolR {
                            loc: loc.dupe(),
                            sense: true,
                            lit: inner.value,
                        };
                        let expr = Expression::new(ExpressionInner::BooleanLiteral {
                            loc: loc.dupe(),
                            inner: Arc::new(inner.as_ref().clone()),
                        });
                        self.resolver
                            .literal_test(true, true, loc.dupe(), acc, refi, &expr)?;
                        non_binding_leaves.insert(loc.dupe());
                    }
                    MatchPattern::NullPattern { loc, inner: _ } => {
                        let null_lit = Expression::new(ExpressionInner::NullLiteral {
                            loc: loc.dupe(),
                            inner: Arc::new(None),
                        });
                        self.resolver
                            .null_test(true, true, loc.dupe(), acc, &null_lit)?;
                        non_binding_leaves.insert(loc.dupe());
                    }
                    MatchPattern::IdentifierPattern { loc, inner: id } => {
                        self.check_invalid_reference(bindings, id);
                        self.resolver.identifier(id)?;
                        let id_expr = Expression::new(ExpressionInner::Identifier {
                            loc: loc.dupe(),
                            inner: id.as_ref().dupe(),
                        });
                        let refis = self.resolver.maybe_sentinel(
                            true,
                            true,
                            loc.dupe(),
                            acc,
                            id_expr.loc().dupe(),
                        );
                        let refis = self.eq_refinement(acc, loc.dupe(), refis);
                        self.resolver.commit_refinement(refis);
                        non_binding_leaves.insert(loc.dupe());
                    }
                    MatchPattern::MemberPattern {
                        loc,
                        inner: member_pattern,
                    } => {
                        let mut captured_error: Option<AbruptCompletion> = None;
                        let (mem_expr, root_ident) = ast_utils::expression_of_match_member_pattern(
                            &mut |e: &Expression<ALoc, ALoc>| {
                                if captured_error.is_none() {
                                    if let Err(err) = self.resolver.expression(e) {
                                        captured_error = Some(err);
                                    }
                                }
                            },
                            member_pattern,
                        );
                        if let Some(err) = captured_error {
                            return Err(err);
                        }
                        self.check_invalid_reference(bindings, &root_ident);
                        let refis = self.resolver.maybe_sentinel(
                            true,
                            true,
                            loc.dupe(),
                            acc,
                            mem_expr.loc().dupe(),
                        );
                        let refis = self.eq_refinement(acc, loc.dupe(), refis);
                        self.resolver.commit_refinement(refis);
                        non_binding_leaves.insert(loc.dupe());
                    }
                    MatchPattern::UnaryPattern { loc, inner } => {
                        let (_, ref arg) = inner.argument;
                        match arg {
                            flow_parser::ast::match_pattern::unary_pattern::Argument::NumberLiteral(
                                num_lit,
                            ) => {
                                let (value, raw) = match inner.operator {
                                    flow_parser::ast::match_pattern::unary_pattern::Operator::Plus => {
                                        (num_lit.value, num_lit.raw.dupe())
                                    }
                                    flow_parser::ast::match_pattern::unary_pattern::Operator::Minus => {
                                        ast_utils::negate_number_literal((
                                            num_lit.value,
                                            num_lit.raw.dupe(),
                                        ))
                                    }
                                };
                                let refi = RefinementKind::SingletonNumR {
                                    loc: loc.dupe(),
                                    sense: true,
                                    lit: (value, raw.dupe()),
                                };
                                let expr = Expression::new(ExpressionInner::NumberLiteral {
                                    loc: loc.dupe(),
                                    inner: Arc::new(flow_parser::ast::NumberLiteral {
                                        value,
                                        raw,
                                        comments: num_lit.comments.dupe(),
                                    }),
                                });
                                self.resolver.literal_test(true, true, loc.dupe(), acc, refi, &expr)?;
                            }
                            flow_parser::ast::match_pattern::unary_pattern::Argument::BigIntLiteral(
                                bigint_lit,
                            ) => {
                                let (value, raw) = match inner.operator {
                                    flow_parser::ast::match_pattern::unary_pattern::Operator::Plus => {
                                        (bigint_lit.value , bigint_lit.raw.dupe())
                                    }
                                    flow_parser::ast::match_pattern::unary_pattern::Operator::Minus => {
                                        ast_utils::negate_bigint_literal((
                                            bigint_lit.value ,
                                            bigint_lit.raw.dupe(),
                                        ))
                                    }
                                };
                                let refi = RefinementKind::SingletonBigIntR {
                                    loc: loc.dupe(),
                                    sense: true,
                                    lit: (value, raw.dupe()),
                                };
                                let expr = Expression::new(ExpressionInner::BigIntLiteral {
                                    loc: loc.dupe(),
                                    inner: Arc::new(flow_parser::ast::BigIntLiteral {
                                        value,
                                        raw,
                                        comments: bigint_lit.comments.dupe(),
                                    }),
                                });
                                self.resolver.literal_test(true, true, loc.dupe(), acc, refi, &expr)?;
                            }
                        }
                        non_binding_leaves.insert(loc.dupe());
                    }
                    MatchPattern::AsPattern { inner, .. } => {
                        match &inner.target {
                            flow_parser::ast::match_pattern::as_pattern::Target::Binding {
                                pattern: binding,
                                ..
                            } => {
                                add_binding(binding.kind, &binding.id, bindings);
                            }
                            flow_parser::ast::match_pattern::as_pattern::Target::Identifier(id) => {
                                add_binding(VariableKind::Const, id, bindings);
                            }
                        }
                        self.recurse(acc, &inner.pattern, bindings, non_binding_leaves)?;
                    }
                    MatchPattern::BindingPattern { loc, inner } => {
                        self.impossible_refinement(acc, loc.dupe());
                        add_binding(inner.kind, &inner.id, bindings);
                    }
                    MatchPattern::WildcardPattern { loc, .. } => {
                        self.impossible_refinement(acc, loc.dupe());
                        non_binding_leaves.insert(loc.dupe());
                    }
                    MatchPattern::OrPattern { inner, .. } => {
                        let patterns_rev: Vec<_> = inner.patterns.iter().rev().collect();
                        self.check_or(&patterns_rev, acc, bindings, non_binding_leaves)?;
                    }
                    MatchPattern::ObjectPattern { loc, inner } => {
                        if let Some(key) = refinement_key::RefinementKey::of_expression(acc) {
                            let mut refining_locs = empty_refining_locs();
                            refining_locs.insert(loc.dupe());
                            // typeof x === 'object' && x !== null
                            let refi = RefinementKind::AndR(
                                Rc::new(RefinementKind::ObjectR),
                                Rc::new(RefinementKind::NotR(Rc::new(RefinementKind::NullR))),
                            );
                            self.resolver
                                .add_single_refinement(&key, refining_locs, refi);
                        }
                        self.object_pattern_props(false, acc, inner, bindings, non_binding_leaves)?;
                    }
                    MatchPattern::ArrayPattern { loc, inner } => {
                        self.array_pattern(acc, loc.dupe(), inner, bindings, non_binding_leaves)?;
                    }
                    MatchPattern::InstancePattern { loc, inner } => {
                        let (expr, id) = match &inner.constructor {
                            flow_parser::ast::match_pattern::InstancePatternConstructor::IdentifierConstructor(ctor_id) => {
                                self.resolver.identifier(ctor_id)?;
                                let expr = Expression::new(ExpressionInner::Identifier {
                                    loc: ctor_id.loc.dupe(),
                                    inner: ctor_id.dupe(),
                                });
                                (expr, ctor_id.dupe())
                            }
                            flow_parser::ast::match_pattern::InstancePatternConstructor::MemberConstructor(member) => {
                                let mut captured_error: Option<AbruptCompletion> = None;
                                let result = ast_utils::expression_of_match_member_pattern(
                                    &mut |e: &Expression<ALoc, ALoc>| {
                                        if captured_error.is_none() {
                                            if let Err(err) = self.resolver.expression(e) {
                                                captured_error = Some(err);
                                            }
                                        }
                                    },
                                    member,
                                );
                                if let Some(err) = captured_error {
                                    return Err(err);
                                }
                                result
                            }
                        };
                        self.check_invalid_reference(bindings, &id);
                        self.resolver.instance_test(
                            env_api::refi::InstanceofContext::MatchInstancePattern,
                            loc.dupe(),
                            acc,
                            &expr,
                        )?;
                        self.object_pattern_props(
                            true,
                            acc,
                            &inner.properties.1,
                            bindings,
                            non_binding_leaves,
                        )?;
                    }
                }
                Ok(())
            }

            fn check_or(
                &mut self,
                patterns: &[&MatchPattern<ALoc, ALoc>],
                acc: &Expression<ALoc, ALoc>,
                bindings: &mut BindingsMap,
                non_binding_leaves: &mut ALocSet,
            ) -> Result<(), AbruptCompletion> {
                match patterns {
                    [] => {}
                    [pattern] => {
                        self.recurse(acc, pattern, bindings, non_binding_leaves)?;
                    }
                    [rhs, rest @ ..] => {
                        self.resolver.push_refinement_scope(RefinementMaps::empty());
                        self.check_or(rest, acc, bindings, non_binding_leaves)?;
                        let lhs_latest_refinements = self.resolver.peek_new_refinements();
                        let env1 = self.resolver.env_snapshot_without_latest_refinements();
                        self.resolver.negate_new_refinements();
                        self.resolver.push_refinement_scope(RefinementMaps::empty());
                        self.recurse(acc, rhs, bindings, non_binding_leaves)?;
                        let rhs_latest_refinements = self.resolver.peek_new_refinements();
                        self.resolver.pop_refinement_scope();
                        self.resolver.pop_refinement_scope();
                        self.resolver.merge_self_env(&env1);
                        self.resolver.merge_refinement_scopes(
                            false,
                            &lhs_latest_refinements,
                            &rhs_latest_refinements,
                        );
                    }
                }
                Ok(())
            }

            fn array_pattern(
                &mut self,
                acc: &Expression<ALoc, ALoc>,
                loc: ALoc,
                pattern: &flow_parser::ast::match_pattern::ArrayPattern<ALoc, ALoc>,
                bindings: &mut BindingsMap,
                non_binding_leaves: &mut ALocSet,
            ) -> Result<(), AbruptCompletion> {
                if let Some(key) = refinement_key::RefinementKey::of_expression(acc) {
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    let refis = self.resolver.start_refinement(
                        &key,
                        refining_locs.dupe(),
                        RefinementKind::IsArrayR,
                    );
                    let op = if pattern.rest.is_some() {
                        env_api::refi::ArrayLengthOp::ArrLenGreaterThanEqual
                    } else {
                        env_api::refi::ArrayLengthOp::ArrLenEqual
                    };
                    // Check the length
                    let refis = self.resolver.extend_refinement(
                        &key,
                        refining_locs,
                        RefinementKind::ArrLenR {
                            op,
                            n: pattern.elements.len() as i32,
                        },
                        refis,
                    );
                    self.resolver.commit_refinement(refis);
                }

                fn number_of_i(i: usize) -> flow_parser::ast::NumberLiteral<ALoc> {
                    flow_parser::ast::NumberLiteral {
                        value: i as f64,
                        raw: FlowSmolStr::new(i.to_string()),
                        comments: None,
                    }
                }

                for (i, elem) in pattern.elements.iter().enumerate() {
                    let pat_loc = elem.pattern.loc().dupe();
                    let member = Expression::new(ExpressionInner::Member {
                        loc: elem.index.dupe(),
                        inner: Arc::new(Member {
                            object: acc.clone(),
                            property: MemberProperty::PropertyExpression(Expression::new(
                                ExpressionInner::NumberLiteral {
                                    loc: pat_loc,
                                    inner: Arc::new(number_of_i(i)),
                                },
                            )),
                            comments: None,
                        }),
                    });
                    self.recurse(&member, &elem.pattern, bindings, non_binding_leaves)?;
                }

                bindings_of_rest(bindings, &pattern.rest);
                Ok(())
            }

            fn object_pattern_props(
                &mut self,
                instance_pattern: bool,
                acc: &Expression<ALoc, ALoc>,
                pattern: &flow_parser::ast::match_pattern::ObjectPattern<ALoc, ALoc>,
                bindings: &mut BindingsMap,
                non_binding_leaves: &mut ALocSet,
            ) -> Result<(), AbruptCompletion> {
                for prop in pattern.properties.iter() {
                    match prop {
                        object_pattern::Property::Valid {
                            loc,
                            property: inner,
                        } => {
                            let (property, propname): (MemberProperty<ALoc, ALoc>, FlowSmolStr) =
                                match &inner.key {
                                    object_pattern::Key::Identifier(id) => (
                                        MemberProperty::PropertyIdentifier(id.dupe()),
                                        id.name.dupe(),
                                    ),
                                    object_pattern::Key::StringLiteral((key_loc, lit)) => (
                                        MemberProperty::PropertyExpression(Expression::new(
                                            ExpressionInner::StringLiteral {
                                                loc: key_loc.dupe(),
                                                inner: Arc::new(lit.clone()),
                                            },
                                        )),
                                        lit.value.dupe(),
                                    ),
                                    object_pattern::Key::NumberLiteral((key_loc, lit)) => {
                                        let name =
                                            flow_common::js_number::ecma_string_of_float(lit.value);
                                        (
                                            MemberProperty::PropertyExpression(Expression::new(
                                                ExpressionInner::NumberLiteral {
                                                    loc: key_loc.dupe(),
                                                    inner: Arc::new(lit.clone()),
                                                },
                                            )),
                                            FlowSmolStr::new(&name),
                                        )
                                    }
                                    object_pattern::Key::BigIntLiteral((key_loc, lit)) => (
                                        MemberProperty::PropertyExpression(Expression::new(
                                            ExpressionInner::BigIntLiteral {
                                                loc: key_loc.dupe(),
                                                inner: Arc::new(lit.clone()),
                                            },
                                        )),
                                        lit.raw.dupe(),
                                    ),
                                };

                            let member = Expression::new(ExpressionInner::Member {
                                loc: loc.dupe(),
                                inner: Arc::new(Member {
                                    object: acc.clone(),
                                    property,
                                    comments: None,
                                }),
                            });

                            if !instance_pattern {
                                if let Some(pat_loc) = needs_prop_exists_refi(&inner.pattern) {
                                    if let Some(key) =
                                        refinement_key::RefinementKey::of_expression(acc)
                                    {
                                        let refi = RefinementKind::PropExistsR {
                                            propname,
                                            loc: pat_loc.dupe(),
                                        };
                                        let mut refining_locs = empty_refining_locs();
                                        refining_locs.insert(pat_loc);
                                        self.resolver.add_single_refinement(
                                            &key,
                                            refining_locs,
                                            refi,
                                        );
                                    }
                                }
                            }

                            self.recurse(&member, &inner.pattern, bindings, non_binding_leaves)?;
                        }
                        object_pattern::Property::InvalidShorthand { .. } => {}
                    }
                }

                bindings_of_rest(bindings, &pattern.rest);
                Ok(())
            }
        }

        let (bindings, non_binding_leaves) = {
            let mut bindings: BindingsMap = BTreeMap::new();
            let mut non_binding_leaves: ALocSet = ALocSet::new();

            {
                let mut helpers = LocalHelpers { resolver: self };
                helpers.recurse(arg, root_pattern, &mut bindings, &mut non_binding_leaves)?;
            }

            (bindings, non_binding_leaves)
        };

        for (_, binding_list) in bindings.iter() {
            for (kind, id) in binding_list.iter() {
                match kind {
                    VariableKind::Var | VariableKind::Let => {}
                    VariableKind::Const => {
                        self.pattern_identifier(Some(*kind), id)?;
                    }
                }
            }
        }

        for loc in non_binding_leaves.iter() {
            let write_entry = env_api::EnvEntry::AssigningWrite(flow_common::reason::mk_reason(
                flow_common::reason::VirtualReasonDesc::RMatchPattern,
                loc.dupe(),
            ));
            let key = env_api::EnvKey::new(env_api::DefLocType::PatternLoc, loc.dupe());
            self.env_state.write_entries.insert(key, write_entry);
        }
        Ok(())
    }

    fn merge_conditional_branches_with_refinements(
        &mut self,
        (env1, refined_env1, completion_state1): (
            &PartialEnvSnapshot,
            &PartialEnvSnapshot,
            Option<&AbruptCompletion>,
        ),
        (env2, refined_env2, completion_state2): (
            &PartialEnvSnapshot,
            &PartialEnvSnapshot,
            Option<&AbruptCompletion>,
        ),
    ) {
        // We only want to merge the refined environments from the two branches of an if-statement
        // if there was an assignment in one of the branches. Otherwise, merging the positive and
        // negative branches of the refinement into a union would be unnecessary work to
        // reconstruct the original type.
        //
        // If one of the branches abnormally completes then we can just take the refinements
        // from the other branch.
        match (completion_state1, completion_state2) {
            (None, Some(_)) => self.reset_env(refined_env1),
            (Some(_), None) => self.reset_env(refined_env2),
            _ => {
                let names = self.env_state.env.all_names();
                for name in names {
                    let PartialEnvEntry {
                        env_val: value1,
                        heap_refinements: heap_entries1,
                        def_loc: _,
                    } = self.partial_env_snapshot_read(&name, env1);
                    let PartialEnvEntry {
                        env_val: value2,
                        heap_refinements: heap_entries2,
                        def_loc: _,
                    } = self.partial_env_snapshot_read(&name, env2);
                    let PartialEnvEntry {
                        env_val: refined_value1,
                        heap_refinements: refined_heap_entries1,
                        def_loc: _,
                    } = self.partial_env_snapshot_read(&name, refined_env1);
                    let PartialEnvEntry {
                        env_val: refined_value2,
                        heap_refinements: refined_heap_entries2,
                        def_loc: _,
                    } = self.partial_env_snapshot_read(&name, refined_env2);

                    let env_val = self.env_state.env.env_val_by_name(&name).unwrap();

                    let merged_heap = {
                        let mut result: HeapRefinementMap = FlowOrdMap::new();
                        let mut all_keys: std::collections::BTreeSet<_> =
                            refined_heap_entries1.keys().duped().collect();
                        all_keys.extend(refined_heap_entries2.keys().duped());
                        for key in all_keys {
                            let refined_heap_val1 = refined_heap_entries1.get(&key);
                            let refined_heap_val2 = refined_heap_entries2.get(&key);
                            let merged = match (refined_heap_val1, refined_heap_val2) {
                                (Some(Ok(rhv1)), Some(Ok(rhv2))) => {
                                    let heap_val1 = heap_map_find(&key, &heap_entries1);
                                    let heap_val2 = heap_map_find(&key, &heap_entries2);
                                    if ssa_val::id_of_val(&heap_val1)
                                        == ssa_val::id_of_val(&heap_val2)
                                    {
                                        if ssa_val::contains_bare_projection(&heap_val1) {
                                            None
                                        } else {
                                            Some(Ok(heap_val1))
                                        }
                                    } else {
                                        let v = ssa_val::merge(
                                            &mut self.cache.borrow_mut(),
                                            rhv1.dupe(),
                                            rhv2.dupe(),
                                        );
                                        if ssa_val::contains_bare_projection(&v) {
                                            None
                                        } else {
                                            Some(Ok(v))
                                        }
                                    }
                                }
                                (Some(Ok(_)), Some(Err(invalidation_info))) => {
                                    let heap_val1 = heap_map_find(&key, &heap_entries1);
                                    if ssa_val::contains_bare_projection(&heap_val1) {
                                        None
                                    } else {
                                        Some(Err(invalidation_info.dupe()))
                                    }
                                }
                                (Some(Err(invalidation_info)), Some(Ok(_))) => {
                                    let heap_val2 = heap_map_find(&key, &heap_entries2);
                                    if ssa_val::contains_bare_projection(&heap_val2) {
                                        None
                                    } else {
                                        Some(Err(invalidation_info.dupe()))
                                    }
                                }
                                (Some(Err(info1)), Some(Err(info2))) => {
                                    Some(Err(flow_common::refinement_invalidation::merge(
                                        Some(info1.dupe()),
                                        Some(info2.dupe()),
                                    )
                                    .unwrap()))
                                }
                                (None, _) | (_, None) => None,
                            };
                            if let Some(v) = merged {
                                result.insert(key, v);
                            }
                        }
                        result
                    };
                    *env_val.heap_refinements.borrow_mut() = merged_heap;

                    if ssa_val::id_of_val(&value1) == ssa_val::id_of_val(&value2) {
                        *env_val.val_ref.borrow_mut() = value1;
                    } else {
                        *env_val.val_ref.borrow_mut() = self.merge_vals_with_havoc(
                            &env_val.havoc,
                            &env_val.def_loc,
                            &refined_value1,
                            &refined_value2,
                        );
                    }
                }
            }
        }
    }

    fn with_env_state<T, F: FnOnce(&mut Self) -> T>(&mut self, f: F) -> T {
        let pre_state = self.env_state.clone();
        let pre_env = self.env_snapshot();
        let result = f(self);
        self.env_state = pre_state;
        // It's not enough to just restore the old env_state, since the env itself contains
        // refs. We need to call reset_env to _fully_ reset the env_state
        self.reset_env(&pre_env);
        result
    }

    // Functions called inside scout_changed_refinement_keys are responsible for popping any refinement
    // scopes they may introduce
    fn scout_changed_refinement_keys<F: FnOnce(&mut Self) -> Result<(), AbruptCompletion>>(
        &mut self,
        scout: F,
        continues: &[AbruptCompletion],
    ) -> Vec<refinement_key::Lookup> {
        // Calling scout may have side effects, like adding new abrupt completions. We
        // need to be sure to restore the old abrupt completion envs after scouting,
        // because a scout should be followed-up by a run that revisits everything visited by
        // the scout. with_env_state will ensure that all mutable state is restored.
        self.with_env_state(|this| {
            let pre_env = this.env_snapshot();
            let completion_state = this.run_to_completion(scout);
            this.run_to_completion(|r| {
                r.commit_abrupt_completion_matching(|c| c.mem(continues), completion_state.as_ref())
            });
            let post_env = this.env_snapshot();
            partial_env_snapshot::merge_env(
                Vec::new(),
                &pre_env,
                &post_env,
                |acc, name, pre_env_v, post_env_v| {
                    let PartialEnvEntry {
                        env_val: post_env_val,
                        heap_refinements: post_env_heap_refinements,
                        def_loc,
                    } = match post_env_v {
                        Some(v) => v.dupe(),
                        None => this.env_read_into_snapshot_from_below(name),
                    };
                    let PartialEnvEntry {
                        env_val: pre_env_val,
                        heap_refinements: pre_env_heap_refinements,
                        def_loc: _,
                    } = match pre_env_v {
                        Some(v) => v.dupe(),
                        None => this.env_read_into_snapshot_from_below(name),
                    };
                    let mut acc = post_env_heap_refinements.iter().fold(
                        acc,
                        |mut acc, (k, post_env_heap_refinement)| match post_env_heap_refinement {
                            Err(_) => acc,
                            Ok(post_env_heap_refinement) => match pre_env_heap_refinements.get(k) {
                                None | Some(Err(_)) => {
                                    acc.push(refinement_key::Lookup {
                                        base: name.dupe(),
                                        projections: k.dupe(),
                                    });
                                    acc
                                }
                                Some(Ok(pre_env_heap_refinement)) => {
                                    if ssa_val::id_of_val(post_env_heap_refinement)
                                        == ssa_val::id_of_val(pre_env_heap_refinement)
                                    {
                                        acc
                                    } else {
                                        acc.push(refinement_key::Lookup {
                                            base: name.dupe(),
                                            projections: k.dupe(),
                                        });
                                        acc
                                    }
                                }
                            },
                        },
                    );
                    let is_definitely_const_like = match &def_loc {
                        None => false,
                        Some(loc) => !flow_env_builder::invalidation_api::should_invalidate(
                            true,
                            &mut this.invalidation_caches.borrow_mut(),
                            this.prepass_info,
                            this.prepass_values,
                            loc.dupe(),
                        ),
                    };
                    if is_definitely_const_like
                        || ssa_val::id_of_val(&pre_env_val) == ssa_val::id_of_val(&post_env_val)
                    {
                        (acc, None::<()>)
                    } else {
                        acc.push(refinement_key::Lookup::of_name(name.dupe()));
                        (acc, None::<()>)
                    }
                },
            )
            .0
        })
    }

    fn havoc_changed_refinement_keys(&mut self, changed: &[refinement_key::Lookup]) {
        for lookup in changed {
            // If a var is changed then all the heap refinements on that var should
            // also be havoced. If only heap refinements are havoced then there's no
            // need to havoc the subject of the projection
            if lookup.projections.is_empty() {
                // let { val_ref; havoc; heap_refinements; kind; ... } = this#env_read base in
                if let Some(env_val) = self.env_read_opt(&lookup.base) {
                    // if kind <> Bindings.Const && not (Val.is_undeclared_or_skipped !val_ref) then
                    if !matches!(
                        env_val.kind,
                        BindingsKind::Const | BindingsKind::DeclaredConst
                    ) && !ssa_val::is_undeclared_or_skipped(&env_val.val_ref.borrow())
                    {
                        // this#havoc_heap_refinements heap_refinements;
                        env_val.heap_refinements.borrow_mut().clear();
                        // let writes = Val.writes_of_uninitialized this#refinement_may_be_undefined !val_ref in
                        let writes = ssa_val::writes_of_uninitialized(
                            |id| self.refinement_may_be_undefined(RefinementId(id)),
                            &env_val.val_ref.borrow(),
                        );
                        // val_ref := Base.List.fold ~init:havoc ~f:(fun acc write -> Val.merge acc (Val.of_write write)) writes
                        let new_val =
                            writes.into_iter().fold(env_val.havoc.dupe(), |acc, write| {
                                let write_val =
                                    ssa_val::of_write(&mut *self.cache.borrow_mut(), write);
                                ssa_val::merge(&mut *self.cache.borrow_mut(), acc, write_val)
                            });
                        *env_val.val_ref.borrow_mut() = new_val;
                    }
                }
            } else {
                let base = &lookup.base;
                let projs = &lookup.projections;
                let scope = self.env_state.env.current_scope_mut();
                if let Some(stack) = scope.local_stacked_env.get(base) {
                    stack
                        .back()
                        .unwrap()
                        .heap_refinements
                        .borrow_mut()
                        .remove(projs);
                }
            }
        }
    }

    fn handle_continues(
        &mut self,
        loop_completion_state: Option<&AbruptCompletion>,
        continues: &[AbruptCompletion],
    ) -> Option<AbruptCompletion> {
        self.run_to_completion(|this| {
            this.commit_abrupt_completion_matching(|c| c.mem(continues), loop_completion_state)
        })
    }

    /// After a loop we need to negate the loop guard and apply the refinement. The
    /// targets of those refinements may have been changed by the loop, but that
    /// doesn't matter. The only way to get out of the loop is for the negation of
    /// the refinement to hold, so we apply that negation even though the ssa_id might
    /// not match.
    ///
    /// The exception here is, of course, if we break out of the loop. If we break
    /// inside the loop then we should not negate the refinements because it is
    /// possible that we just exited the loop by breaking.
    ///
    /// We don't need to check for continues because they are handled before this point.
    /// We don't check for throw/return because then we wouldn't proceed to the line
    /// after the loop anyway.
    fn post_loop_refinements(
        &mut self,
        env_before_guard: &PartialEnvSnapshot,
        guard: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        if !self
            .env_state
            .abrupt_completion_envs
            .iter()
            .any(|(c, _)| matches!(c, AbruptCompletion::Break(None)))
        {
            self.push_refinement_scope(RefinementMaps::empty());
            self.expression_refinement(guard)?;
            self.negate_new_refinements();
            self.pop_refinement_scope_without_unrefining();
            let final_env = self.env_snapshot();
            self.reset_env(env_before_guard);
            self.push_refinement_scope(RefinementMaps::empty());
            self.expression_refinement(guard)?;
            self.pop_refinement_scope();
            self.reset_env(&final_env);
        }
        Ok(())
    }

    /// Unlike the ssa_builder, the name_resolver does not create REF unresolved
    /// Val.ts to model the write states of variables in loops. This approach
    /// would cause a lot of cycles in the ordering algorithm, which means
    /// we'd need to ask for a lot of annotations. Moreover, it's not clear where
    /// those annotations should go.
    ///
    /// Instead, we scout the body of the loop to find which variables are
    /// written to. If a variable is written, then we havoc that variable
    /// before entering the loop. This does not apply to variables that are
    /// only refined.
    ///
    /// After visiting the body, we reset the state in the ssa environment,
    /// havoc any vars that need to be havoced, and then visit the body again.
    /// After that we negate the refinements on the loop guard by revisiting the guard
    /// with the post-loop state and negating the refinement.
    ///
    /// This last part that revisits the guard will record writes that we don't want recorded. To
    /// restore the original state, we reset the environment to what it was when we originally
    /// visited the guard and then visit it again. You can see this in post_loop_refinements.
    ///
    /// Here's how each param should be used:
    /// scout: Visit the guard and any updaters if applicable, then visit the body
    /// guard: An optional expression AST that is the predicate that is evaluated before
    ///   entering the loop body.
    /// visit_guard_and_body: Visit the guard with a refinement scope, any updaters
    ///   if applicable, and then visit the body. Return the loop completion state.
    /// make_completion_states: given the loop completion state, give the list of
    ///   possible completion states for the loop. For do while loops this is different
    ///   than regular while loops, so those two implementations may be instructive.
    /// auto_handle_continues: Every loop needs to filter out continue completion states.
    ///   The default behavior is to do that filtering at the end of the body.
    ///   If you need to handle continues before that, like in a do/while loop, then
    ///   set this to false. Ensure that you handle continues in both the scouting and
    ///   main passes.
    fn env_loop<'b, FScout, FVisit, FMakeCompletion>(
        &mut self,
        guard: Option<&'b flow_parser::ast::expression::Expression<ALoc, ALoc>>,
        scout: FScout,
        visit_guard_and_body: FVisit,
        make_completion_states: FMakeCompletion,
        auto_handle_continues: bool,
        continues: Vec<AbruptCompletion>,
    ) -> Result<(), AbruptCompletion>
    where
        FScout: FnOnce(&mut Self) -> Result<(), AbruptCompletion>,
        FVisit: FnOnce(
            &mut Self,
        ) -> Result<
            (
                Option<AbruptCompletion>,
                PartialEnvSnapshot,
                Option<PartialEnvSnapshot>,
            ),
            AbruptCompletion,
        >,
        FMakeCompletion: FnOnce(
            Option<AbruptCompletion>,
        )
            -> (Option<AbruptCompletion>, Vec<Option<AbruptCompletion>>),
    {
        self.expecting_abrupt_completions(|this| {
            // Scout the body for changed vars
            let changed_refinement_keys = this.scout_changed_refinement_keys(scout, &continues);

            // We havoc the changed vars in order to prevent loops in the EnvBuilder writes-graph,
            // which would require a fix-point analysis that would not be compatible with
            // local type inference
            this.havoc_changed_refinement_keys(&changed_refinement_keys);

            // Now we push a refinement scope and visit the guard/body. At the end, we completely
            // get rid of refinements introduced by the guard, even if they occur in a PHI node, to
            // ensure that the refinement does not escape the loop via something like
            // control flow. For example:
            // while (x != null) {
            //   if (x == 3) {
            //     x = 4;
            //   }
            // }
            // x; // Don't want x to be a PHI of x != null and x = 4.
            this.push_refinement_scope(RefinementMaps::empty());
            let (loop_completion_state, env_before_guard, env_after_guard_no_refinements) =
                visit_guard_and_body(this)?;

            let loop_completion_state = if auto_handle_continues {
                this.handle_continues(loop_completion_state.as_ref(), &continues)
            } else {
                loop_completion_state
            };
            this.pop_refinement_scope_after_loop();

            // We either enter the loop body or we don't
            if let Some(ref env) = env_after_guard_no_refinements {
                this.merge_self_env(env);
            }
            if let Some(guard_expr) = guard {
                this.post_loop_refinements(&env_before_guard, guard_expr)?;
            }

            let (hd, tl) = make_completion_states(loop_completion_state);
            let completion_state =
                this.run_to_completion(|r| r.merge_completion_states(hd.as_ref(), &tl));

            this.commit_abrupt_completion_matching(
                |c| matches!(c, AbruptCompletion::Break(None)),
                completion_state.as_ref(),
            )
        })
    }

    fn scoped_for_statement(
        &mut self,
        _loc: ALoc,
        stmt: &flow_parser::ast::statement::For<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let continues: Vec<AbruptCompletion> = std::iter::once(AbruptCompletion::Continue(None))
            .chain(self.env_state.possible_labeled_continues.dupe())
            .collect();

        let continues_for_scout = continues.clone();

        let scout = move |resolver: &mut Self| {
            if let Some(init) = &stmt.init {
                resolver.for_statement_init(init)?;
            }
            if let Some(test) = &stmt.test {
                resolver.expression(test)?;
            }
            let loop_completion_state = resolver.run_to_completion(|r| r.statement(&stmt.body));
            let loop_completion_state =
                resolver.handle_continues(loop_completion_state.as_ref(), &continues_for_scout);
            if loop_completion_state.is_none() {
                if let Some(update) = &stmt.update {
                    resolver.expression(update)?;
                }
            }
            Ok(())
        };

        let continues_for_visit = continues.clone();

        let visit_guard_and_body = move |resolver: &mut Self| {
            if let Some(init) = &stmt.init {
                if let Err(ac) = resolver.for_statement_init(init) {
                    return Ok((Some(ac), resolver.env_snapshot(), None));
                }
            }
            let env_before_guard = resolver.env_snapshot();
            if let Some(test) = &stmt.test {
                if let Err(ac) = resolver.expression_refinement(test) {
                    return Ok((Some(ac), env_before_guard, None));
                }
            }
            let env = resolver.env_snapshot_without_latest_refinements();
            let loop_completion_state = resolver.run_to_completion(|r| r.statement(&stmt.body));
            let loop_completion_state =
                resolver.handle_continues(loop_completion_state.as_ref(), &continues_for_visit);
            if loop_completion_state.is_none() {
                if let Some(update) = &stmt.update {
                    if let Err(ac) = resolver.expression(update) {
                        return Ok((Some(ac), env_before_guard, Some(env)));
                    }
                }
            }
            Ok((loop_completion_state, env_before_guard, Some(env)))
        };

        let make_completion_states =
            |loop_completion_state: Option<AbruptCompletion>| (None, vec![loop_completion_state]);

        self.env_loop(
            stmt.test.as_ref(),
            scout,
            visit_guard_and_body,
            make_completion_states,
            false,
            continues,
        )
    }

    fn for_in_or_of_left_declaration(
        &mut self,
        left: &(
            ALoc,
            flow_parser::ast::statement::VariableDeclaration<ALoc, ALoc>,
        ),
    ) -> Result<(), AbruptCompletion> {
        let (loc, decl) = left;
        let flow_parser::ast::statement::VariableDeclaration {
            declarations,
            kind,
            comments: _,
        } = decl;
        match &**declarations {
            [declarator] => {
                use flow_parser::ast::pattern::Pattern;
                let id = &declarator.id;
                match id {
                    Pattern::Identifier { .. } | Pattern::Object { .. } | Pattern::Array { .. } => {
                        self.pattern(Some(*kind), id)?;
                        Ok(())
                    }
                    _ => {
                        let loc = id.loc();
                        panic!("Env_invariant: unexpected AST node at {:?}", loc)
                    }
                }
            }
            _ => {
                panic!(
                    "Env_invariant: Syntactically valid for-in loops must have exactly one left declaration at {:?}",
                    loc
                )
            }
        }
    }

    fn scoped_for_in_or_of_statement<FLeft>(
        &mut self,
        traverse_left: FLeft,
        body: &flow_parser::ast::statement::Statement<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion>
    where
        FLeft: Fn(&mut Self) -> Result<(), AbruptCompletion> + Clone,
    {
        let continues: Vec<AbruptCompletion> = std::iter::once(AbruptCompletion::Continue(None))
            .chain(self.env_state.possible_labeled_continues.dupe())
            .collect();

        // You might be wondering why the lhs has to be scouted-- the LHS can be a pattern that
        // includes a default write with a variable that is written to inside the loop. It's
        // critical that we catch loops in the dependency graph with such variables, since the
        // ordering algorithm will not have a good place to ask for an annotation in that case.
        let scout = |resolver: &mut Self| {
            traverse_left(resolver)?;
            resolver.run_to_completion(|r| r.statement(body));
            Ok(())
        };

        let visit_guard_and_body = |resolver: &mut Self| {
            let env = resolver.env_snapshot();
            traverse_left(resolver)?;
            let loop_completion_state = resolver.run_to_completion(|r| r.statement(body));
            Ok((loop_completion_state, env.dupe(), Some(env)))
        };

        let make_completion_states =
            |loop_completion_state: Option<AbruptCompletion>| (None, vec![loop_completion_state]);

        self.env_loop(
            None,
            scout,
            visit_guard_and_body,
            make_completion_states,
            true,
            continues,
        )
    }

    fn scoped_for_in_statement(
        &mut self,
        _loc: ALoc,
        stmt: &flow_parser::ast::statement::ForIn<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let traverse_left = |resolver: &mut Self| resolver.for_in_statement_lhs(&stmt.left);
        self.push_refinement_scope(RefinementMaps::empty());
        self.expression_refinement(&stmt.right)?;
        self.scoped_for_in_or_of_statement(traverse_left, &stmt.body)?;
        self.pop_refinement_scope();
        Ok(())
    }

    fn scoped_for_of_statement(
        &mut self,
        _loc: ALoc,
        stmt: &flow_parser::ast::statement::ForOf<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.expression(&stmt.right)?;
        let traverse_left = |resolver: &mut Self| resolver.for_of_statement_lhs(&stmt.left);
        self.scoped_for_in_or_of_statement(traverse_left, &stmt.body)
    }

    fn switch_completeness(&mut self, loc: ALoc) {
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;

        let desc = VirtualReasonDesc::RCustom(FlowSmolStr::new_inline("switch"));
        let reason = VirtualReason::new(desc, loc);
        let var_name = MAYBE_EXHAUSTIVELY_CHECKED_VAR_NAME_STR.dupe();
        let env_val = self.env_read(&var_name);
        let new_val = ssa_val::one(&mut *self.cache.borrow_mut(), reason);
        *env_val.val_ref.borrow_mut() = new_val;
    }

    // **********************************************************
    //  [PRE] switch (e) { case e1: s1 ... case eN: sN } [POST] *
    // **********************************************************
    //      |                                                   *
    //      e                                                   *
    //     /                                                    *
    //    e1                                                    *
    //    | \                                                   *
    //    .  s1                                                 *
    //    |   |                                                 *
    //    ei  .                                                 *
    //    | \ |                                                 *
    //    .  si                                                 *
    //    |   |                                                 *
    //    eN  .                                                 *
    //    | \ |                                                 *
    //    |  sN                                                 *
    //     \  |                                                 *
    //       \|                                                 *
    //        |                                                 *
    // **********************************************************
    //  [PRE] e [ENV0]                                          *
    //  ENV0' = empty                                           *
    //  \forall i = 0..N-1:                                     *
    //    [ENVi] ei+1 [ENVi+1]                                  *
    //    [ENVi+1 | ENVi'] si+1 [ENVi+1']                       *
    //  POST = ENVN | ENVN'                                     *
    // **********************************************************
    fn switch_cases_with_lexical_bindings<'b>(
        &mut self,
        switch_loc: &ALoc,
        exhaustive_out: &ALoc,
        discriminant: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        cases_with_lexical_bindings: &[(
            &'b flow_parser::ast::statement::switch::Case<ALoc, ALoc>,
            BTreeMap<
                FlowSmolStr,
                (
                    flow_analysis::bindings::Kind,
                    Vec1<(ALoc, flow_analysis::bindings::Kind)>,
                ),
            >,
        )],
    ) -> Result<(), AbruptCompletion> {
        let incoming_env = self.env_snapshot();
        self.expecting_abrupt_completions(|resolver| {
            let mut case_starting_env = incoming_env.dupe();
            let mut case_completion_states: Vec<Option<AbruptCompletion>> = Vec::new();
            let mut fallthrough_env: Option<PartialEnvSnapshot> = None;
            let mut has_default = false;

            for (case, lexical_bindings) in cases_with_lexical_bindings {
                let (
                    new_case_starting_env,
                    new_completion_state,
                    new_fallthrough_env,
                    new_has_default,
                ) = resolver.env_switch_case(
                    discriminant,
                    (case_starting_env, fallthrough_env, has_default),
                    case,
                    lexical_bindings,
                )?;
                case_starting_env = new_case_starting_env;
                case_completion_states.push(new_completion_state);
                fallthrough_env = new_fallthrough_env;
                has_default = new_has_default;
            }

            resolver.reset_env(&case_starting_env);
            if !has_default {
                let discriminant_after_all_negated_refinements =
                    resolver.get_val_of_expression(discriminant);
                if let Some(discriminant_val) = discriminant_after_all_negated_refinements {
                    resolver.env_state.values.insert(
                        switch_loc.dupe(),
                        ReadEntry {
                            def_loc: None,
                            value: discriminant_val,
                            val_binding_kind: ssa_val::ValBindingKind::InternalBinding,
                            name: None,
                        },
                    );
                }
            }

            match (&fallthrough_env, has_default) {
                // If the switch has a default then it is exhaustive. Thus, the post-env can be
                // determined by joining all of the breaks with the last fallthrough env. If there
                // was no fallthrough env, then the we can use empty as the base.
                (Some(env), true) => resolver.reset_env(env),
                (None, true) => resolver.reset_to_unreachable_env(),
                // If the switch wasn't exhaustive then merge with the case_starting_env as a base. If
                // the last case fell out then merge that in too.
                (Some(fallthrough), false) => {
                    resolver.switch_completeness(exhaustive_out.dupe());
                    resolver.merge_self_env(fallthrough);
                }
                (None, false) => {
                    resolver.switch_completeness(exhaustive_out.dupe());
                }
            }

            // In general, cases are non-exhaustive, but if it has a default case then it is!
            let completion_state = if has_default && !case_completion_states.is_empty() {
                // Since there is a default we know there is at least one element in this
                // list, which means calling List.hd or tail will not fail
                let mut remaining_states = case_completion_states;
                let first_state = remaining_states.remove(0);
                resolver.run_to_completion(|r| {
                    r.merge_completion_states(first_state.as_ref(), &remaining_states)
                })
            } else {
                None
            };

            resolver.commit_abrupt_completion_matching(
                |c| matches!(c, AbruptCompletion::Break(None)),
                completion_state.as_ref(),
            )
        })
    }

    fn env_switch_case(
        &mut self,
        discriminant: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        (case_starting_env, fallthrough_env, has_default): (
            PartialEnvSnapshot,
            Option<PartialEnvSnapshot>,
            bool,
        ),
        case: &flow_parser::ast::statement::switch::Case<ALoc, ALoc>,
        lexical_bindings: &BTreeMap<
            FlowSmolStr,
            (
                flow_analysis::bindings::Kind,
                Vec1<(ALoc, flow_analysis::bindings::Kind)>,
            ),
        >,
    ) -> Result<
        (
            PartialEnvSnapshot,
            Option<AbruptCompletion>,
            Option<PartialEnvSnapshot>,
            bool,
        ),
        AbruptCompletion,
    > {
        use flow_parser::ast::expression::ExpressionInner;
        use flow_parser::ast::statement::switch::Case;

        let Case {
            loc: case_loc,
            test,
            case_test_loc: _,
            consequent,
            comments: _,
        } = case;
        self.reset_env(&case_starting_env);
        // Reset discriminant
        self.push_refinement_scope(RefinementMaps::empty());
        let (has_default, latest_refinements, case_starting_env) = match test {
            None => (true, RefinementMaps::empty(), self.env_snapshot()),
            Some(test) => {
                match discriminant.deref() {
                    ExpressionInner::Member { inner, .. } => {
                        match self.get_val_of_expression(&inner.object) {
                            None => {
                                self.env_state.values.remove(case_loc);
                            }
                            Some(refined_v) => {
                                self.env_state.values.insert(
                                    case_loc.dupe(),
                                    ReadEntry {
                                        def_loc: None,
                                        value: refined_v,
                                        val_binding_kind: ssa_val::ValBindingKind::InternalBinding,
                                        name: None,
                                    },
                                );
                            }
                        }
                    }
                    _ => {}
                }

                self.expression(test)?;
                let test_loc = test.loc();
                self.eq_test(
                    true,
                    true,
                    CondContext::SwitchTest,
                    test_loc.dupe(),
                    discriminant,
                    test,
                )?;
                (
                    has_default,
                    self.peek_new_refinements(),
                    self.env_snapshot_without_latest_refinements(),
                )
            }
        };
        if let Some(ref fallthrough) = fallthrough_env {
            self.merge_self_env(fallthrough);
        }
        for (name, (kind, locs)) in lexical_bindings {
            if self.is_excluded_ordinary_name(name) {
                continue;
            }
            use flow_analysis::bindings::Kind;
            match kind {
                Kind::Let | Kind::Const | Kind::DeclaredLet | Kind::DeclaredConst => {
                    let env_entry = self.env_read(name);
                    Self::havoc_heap_refinements(&env_entry.heap_refinements);
                    let (loc, _) = locs.first();
                    let loc = loc.dupe();
                    *env_entry.val_ref.borrow_mut() = ssa_val::declared_but_skipped(
                        &mut *self.cache.borrow_mut(),
                        name.dupe(),
                        loc,
                    );
                }
                _ => {}
            }
        }
        let case_completion_state = self.run_to_completion(|r| {
            for stmt in consequent.iter() {
                r.statement(stmt)?;
            }
            Ok(())
        });
        let fallthrough_env = match &case_completion_state {
            None => Some(self.env_snapshot()),
            Some(_) => None,
        };
        self.pop_refinement_scope();
        self.reset_env(&case_starting_env);
        let negated_refinements = self.negate_refinements(&latest_refinements);
        self.push_refinement_scope(negated_refinements);
        let case_starting_env = self.env_snapshot();
        self.pop_refinement_scope();

        Ok((
            case_starting_env,
            case_completion_state,
            fallthrough_env,
            has_default,
        ))
    }

    fn this_binding_function_id_opt(
        &mut self,
        fun_loc: ALoc,
        has_this_annot: bool,
        id: Option<&flow_parser::ast::Identifier<ALoc, ALoc>>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;

        if let Some(id) = id {
            self.function_identifier(id)?;
        }

        let function_write_loc = match id {
            Some(id) => id.loc.dupe(),
            None => {
                let reason = VirtualReason::new(
                    VirtualReasonDesc::RFunction(flow_common::reason::ReasonDescFunction::RNormal),
                    fun_loc.dupe(),
                );
                self.env_state.write_entries.insert(
                    env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, fun_loc.dupe()),
                    env_api::EnvEntry::AssigningWrite(reason),
                );
                fun_loc.dupe()
            }
        };

        let function_write_key = env_api::EnvKey::new(
            env_api::DefLocType::OrdinaryNameLoc,
            function_write_loc.dupe(),
        );
        if self.is_assigning_write(&function_write_key) && !has_this_annot {
            let reason = VirtualReason::new(VirtualReasonDesc::RThis, fun_loc.dupe());
            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::FunctionThisLoc, fun_loc),
                env_api::EnvEntry::AssigningWrite(reason),
            );
        }

        Ok(())
    }

    fn visit_function_or_component_param_pattern(
        &mut self,
        is_rest: bool,
        ploc: ALoc,
        pattern: &flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
    ) {
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;
        use flow_parser::ast::pattern::Pattern;

        let reason = match pattern {
            Pattern::Identifier { inner, .. } => {
                let name = Some(inner.name.name.dupe());
                if is_rest {
                    VirtualReason::new(VirtualReasonDesc::RRestParameter(name), ploc.dupe())
                } else {
                    VirtualReason::new(VirtualReasonDesc::RParameter(name), ploc.dupe())
                }
            }
            _ => VirtualReason::new(VirtualReasonDesc::RDestructuring, ploc.dupe()),
        };
        self.env_state.write_entries.insert(
            env_api::EnvKey::new(env_api::DefLocType::FunctionParamLoc, ploc),
            env_api::EnvEntry::AssigningWrite(reason),
        );
    }

    fn component_body_with_params(
        &mut self,
        _component_loc: &ALoc,
        body: &(ALoc, flow_parser::ast::statement::Block<ALoc, ALoc>),
        params: &flow_parser::ast::statement::component_params::Params<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let f = |resolver: &mut Self| -> Result<(), AbruptCompletion> {
            let env = resolver.env_snapshot();
            resolver.run(
                |r| {
                    let completion_state = r.run_to_completion(|r2| {
                        let loc = body.0.dupe();

                        let bindings = Bindings::singleton(flow_analysis::bindings::Entry {
                            loc: loc.dupe(),
                            name: MAYBE_EXHAUSTIVELY_CHECKED_VAR_NAME_STR.dupe(),
                            kind: BindingsKind::Internal,
                        });
                        r2.with_scoped_bindings(ThisSuperBindingEnv::FunctionEnv, &bindings, |r3| {
                            r3.cx.add_exhaustive_check(loc.dupe(), (vec![], false));
                            scope_builder::component_body_with_params(
                                r3,
                                r3.enable_enums,
                                true,
                                body,
                                params,
                            )?;

                            if let Some(env_val) = r3.env_read_opt(&MAYBE_EXHAUSTIVELY_CHECKED_VAR_NAME_STR) {
                                let val = env_val.val_ref.borrow();
                                let mut cache = HashMap::new();
                                let simplified = ssa_val::simplify(
                                    &mut cache,
                                    None,
                                    ssa_val::ValBindingKind::InternalBinding,
                                    None,
                                    &val,
                                );
                                let mut locs: Vec<ALoc> = Vec::new();
                                let mut undeclared = false;
                                for write_loc in &simplified.write_locs {
                                    match write_loc {
                                        env_api::WriteLoc::Undeclared { .. } => undeclared = true,
                                        env_api::WriteLoc::Write(r) => locs.push(r.loc().dupe()),
                                        _ => {
                                            // OCaml: raise Env_api.(Env_invariant (Some component_loc, Impossible "Unexpected env state for maybe_exhaustively_checked"))
                                            panic!(
                                                "Env invariant: Unexpected env state for maybe_exhaustively_checked at {:?}",
                                                loc
                                            );
                                        }
                                    }
                                }
                                r3.cx.add_exhaustive_check(loc, (locs, undeclared));
                            }

                            Ok(())
                        })
                    });

                    r.commit_abrupt_completion_matching(
                        |c| matches!(c, AbruptCompletion::Return | AbruptCompletion::Throw),
                        completion_state.as_ref(),
                    )
                },
                |r| {
                    r.reset_env(&env);
                    Ok(())
                },
            )
        };

        self.under_uninitialized_env(|resolver| resolver.expecting_abrupt_completions(f))
    }

    // Helper for function expressions - adapted from scope_builder
    fn function_expression_without_name(
        &mut self,
        is_arrow: bool,
        loc: &ALoc,
        expr: &flow_parser::ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::function_expression_without_name(
            self,
            self.enable_enums,
            true,
            is_arrow,
            loc,
            expr,
            &|this, f| this.hoist_annotations(|this_inner| f(this_inner)),
            |this, fun_loc, has_this_annot, id| {
                this.this_binding_function_id_opt(fun_loc.dupe(), has_this_annot, id)
            },
            |this,
             _enable_enums,
             _with_types,
             is_arrow,
             fun_loc,
             generator_return_loc,
             params,
             return_,
             predicate,
             body| {
                this.lambda(
                    is_arrow,
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

    fn class_impl(
        &mut self,
        loc: &ALoc,
        cls: &flow_parser::ast::class::Class<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::class_(self, true, loc, cls, |v, class_loc, id| {
            v.class_identifier_opt(class_loc, id)
        })
    }

    fn lambda(
        &mut self,
        is_arrow: bool,
        fun_loc: &ALoc,
        generator_return_loc: Option<&ALoc>,
        params: &flow_parser::ast::function::Params<ALoc, ALoc>,
        return_: &flow_parser::ast::function::ReturnAnnot<ALoc, ALoc>,
        predicate: Option<&flow_parser::ast::types::Predicate<ALoc, ALoc>>,
        body: &flow_parser::ast::function::Body<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let f = |resolver: &mut Self| -> Result<(), AbruptCompletion> {
            let env = resolver.env_snapshot();
            resolver.run(
                |r| {
                    // If this is a type guard function type_guard_name will be set in
                    // type_guard_annotation.
                    let saved_type_guard_name = r.env_state.type_guard_name.dupe();
                    let saved_inferred_candidate = r.env_state.inferred_type_guard_candidate.dupe();
                    r.env_state.inferred_type_guard_candidate =
                        flow_parser::ast_utils::get_inferred_type_guard_candidate(params, body, return_)
                            .map(|(loc, name)| (loc.dupe(), FlowSmolStr::from(name)));
                    let completion_state = r.run_to_completion(|r2| {
                        let body_loc = match body {
                            flow_parser::ast::function::Body::BodyBlock((loc, _)) => loc.dupe(),
                            flow_parser::ast::function::Body::BodyExpression(expr) => expr.loc().dupe(),
                        };
                        let mut bindings = Bindings::singleton(flow_analysis::bindings::Entry {
                            loc: body_loc.dupe(),
                            name: MAYBE_EXHAUSTIVELY_CHECKED_VAR_NAME_STR.dupe(),
                            kind: BindingsKind::Internal,
                        });

                        if !is_arrow {
                            match &params.this_ {
                                Some(this_param) => {
                                    let this_loc = this_param.loc.dupe();
                                    r2.env_state.write_entries.insert(
                                        env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, this_loc.dupe()),
                                        env_api::EnvEntry::AssigningWrite(flow_common::reason::VirtualReason::new(
                                            flow_common::reason::VirtualReasonDesc::RThis,
                                            this_loc.dupe(),
                                        )),
                                    );
                                    bindings.add(flow_analysis::bindings::Entry {
                                        loc: this_loc,
                                        name: FlowSmolStr::new_inline("this"),
                                        kind: BindingsKind::ThisAnnot,
                                    });
                                }
                                None => {
                                    bindings.add(flow_analysis::bindings::Entry {
                                        loc: fun_loc.dupe(),
                                        name: FlowSmolStr::new_inline("this"),
                                        kind: BindingsKind::Const,
                                    });
                                }
                            }
                        }
                        if let Some(return_loc) = generator_return_loc {
                            bindings.add(flow_analysis::bindings::Entry {
                                loc: return_loc.dupe(),
                                name: FlowSmolStr::new_inline(NEXT_VAR_NAME),
                                kind: BindingsKind::GeneratorNext,
                            });
                        }
                        r2.with_scoped_bindings(ThisSuperBindingEnv::FunctionEnv, &bindings, |r3| {
                            r3.cx.add_exhaustive_check(body_loc.dupe(), (vec![], false));
                            scope_builder::lambda(
                                r3,
                                r3.enable_enums,
                                true,
                                is_arrow,
                                fun_loc,
                                generator_return_loc,
                                params,
                                return_,
                                predicate,
                                body,
                            )?;
                            if let Some(env_val) = r3.env_read_opt(&MAYBE_EXHAUSTIVELY_CHECKED_VAR_NAME_STR) {
                                let val = env_val.val_ref.borrow();
                                let mut cache = HashMap::new();
                                let simplified = ssa_val::simplify(&mut cache, None, ssa_val::ValBindingKind::InternalBinding, None, &val);
                                let mut locs: Vec<ALoc> = Vec::new();
                                let mut undeclared = false;
                                for write_loc in &simplified.write_locs {
                                    match write_loc {
                                        env_api::WriteLoc::Undeclared { .. } => undeclared = true,
                                        env_api::WriteLoc::Write(r) => locs.push(r.loc().dupe()),
                                        _ => {
                                            panic!(
                                                "Env invariant: Unexpected env state for maybe_exhaustively_checked at {:?}",
                                                body_loc
                                            );
                                        }
                                    }
                                }
                                r3.cx.add_exhaustive_check(body_loc, (locs, undeclared));
                            }

                            Ok(())
                        })
                    });
                    r.env_state.type_guard_name = saved_type_guard_name;
                    r.env_state.inferred_type_guard_candidate = saved_inferred_candidate;
                    r.commit_abrupt_completion_matching(
                        |c| matches!(c, AbruptCompletion::Return | AbruptCompletion::Throw),
                        completion_state.as_ref(),
                    )
                },
                |r| {
                    r.reset_env(&env);
                    Ok(())
                },
            )
        };
        self.under_uninitialized_env(|resolver| resolver.expecting_abrupt_completions(f))
    }

    fn check_class_name(&mut self, name_loc: ALoc, name: &FlowSmolStr) {
        use std::str::FromStr;

        use flow_typing_errors::error_message::BindingError;
        use flow_typing_errors::intermediate_error_types::IncorrectType;
        if let Ok(keyword) = IncorrectType::from_str(name.as_str()) {
            if keyword.is_type_reserved() {
                Fl::add_output(
                    self.cx,
                    ErrorMessage::EBindingError(Box::new((
                        BindingError::EReservedKeyword { keyword },
                        name_loc.dupe(),
                        flow_common::reason::Name::new(name.dupe()),
                        name_loc,
                    ))),
                );
            }
        }
    }

    fn class_identifier_opt(
        &mut self,
        class_loc: &ALoc,
        id: Option<&flow_parser::ast::Identifier<ALoc, ALoc>>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;

        let (class_write_loc, class_self_reason) = match id {
            Some(identifier) => {
                // Process `class <name> {}`
                let name_loc = identifier.loc.dupe();
                let name = &identifier.name;
                self.check_class_name(name_loc.dupe(), name);
                self.bind_pattern_identifier_customized(
                    PatternWriteKind::ClassBinding,
                    name_loc.dupe(),
                    name,
                    ssa_val::one,
                );
                ast_visitor::identifier_default(self, identifier)?;

                let reason = VirtualReason::new(
                    VirtualReasonDesc::RType(flow_common::reason::Name::new(name.dupe())),
                    name_loc.dupe(),
                );
                (name_loc, reason)
            }
            None => {
                // Process annonymous class
                let reason = VirtualReason::new(
                    VirtualReasonDesc::RType(flow_common::reason::Name::new(
                        FlowSmolStr::new_inline("<<anonymous class>>"),
                    )),
                    class_loc.dupe(),
                );
                self.env_state.write_entries.insert(
                    env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, class_loc.dupe()),
                    EnvEntry::AssigningWrite(reason.dupe()),
                );
                (class_loc.dupe(), reason)
            }
        };
        if self.is_assigning_write(&env_api::EnvKey::ordinary(class_write_loc.dupe())) {
            let self_write = EnvEntry::AssigningWrite(class_self_reason);
            let this_write = EnvEntry::AssigningWrite(VirtualReason::new(
                VirtualReasonDesc::RThis,
                class_loc.dupe(),
            ));
            let super_write = EnvEntry::AssigningWrite(VirtualReason::new(
                VirtualReasonDesc::RSuper,
                class_loc.dupe(),
            ));

            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::ClassSelfLoc, class_loc.dupe()),
                self_write,
            );
            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::ClassInstanceThisLoc, class_loc.dupe()),
                this_write.dupe(),
            );
            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::ClassInstanceSuperLoc, class_loc.dupe()),
                super_write.dupe(),
            );
            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::ClassStaticThisLoc, class_loc.dupe()),
                this_write,
            );
            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::ClassStaticSuperLoc, class_loc.dupe()),
                super_write,
            );
        }
        Ok(())
    }

    fn add_sentinel_check_writes(
        &mut self,
        checks: std::collections::BTreeMap<FlowSmolStr, flow_common::hint::SentinelRefinement>,
    ) {
        use flow_common::hint::SentinelRefinement;

        for (_, check) in checks {
            if let SentinelRefinement::Member(reason) = check {
                let loc = reason.loc().dupe();
                self.env_state.write_entries.insert(
                    env_api::EnvKey::new(env_api::DefLocType::ExpressionLoc, loc),
                    env_api::EnvEntry::AssigningWrite(reason),
                );
            }
        }
    }

    fn non_this_binding_function(
        &mut self,
        loc: &ALoc,
        func: &flow_parser::ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.arrow_function(loc, func)
    }

    fn delete(&mut self, loc: ALoc, argument: &Expression<ALoc, ALoc>) {
        use flow_common::reason::VirtualReasonDesc;
        use flow_common::reason::mk_reason;

        let undefined_reason = mk_reason(VirtualReasonDesc::RVoid, loc.dupe());
        let undefined = flow_env_builder::ssa_val::undefined(
            &mut *self.cache.borrow_mut(),
            undefined_reason.dupe(),
        );
        match argument.deref() {
            ExpressionInner::Identifier { loc: id_loc, inner }
                if !self.is_excluded_ordinary_name(&inner.name) =>
            {
                let env_val = self.env_read(&inner.name);
                let stored_binding_kind = env_val.kind;
                let def_loc = env_val.def_loc.dupe();
                let val_ref = &env_val.val_ref;
                let val_snapshot = val_ref.borrow().dupe();
                match error_for_assignment_kind(
                    &inner.name,
                    id_loc.dupe(),
                    def_loc,
                    stored_binding_kind,
                    PatternWriteKind::AssignmentWrite,
                    &val_snapshot,
                    self.enable_const_params,
                ) {
                    None => {
                        *val_ref.borrow_mut() = undefined;
                    }
                    Some(err) => {
                        self.env_state.write_entries.insert(
                            env_api::EnvKey::new(
                                env_api::DefLocType::OrdinaryNameLoc,
                                argument.loc().dupe(),
                            ),
                            env_api::EnvEntry::NonAssigningWrite,
                        );
                        Fl::add_output(self.cx, err);
                    }
                }
            }
            ExpressionInner::Member { inner: member, .. } => {
                self.assign_member(true, member, loc.dupe(), loc, undefined, undefined_reason);
            }
            _ => {}
        }
    }

    // When the refinement scope we push is non-empty we want to make sure that the variables
    // that scope refines are given their new refinement writes in the environment
    fn push_refinement_scope(&mut self, new_latest_refinements: RefinementMaps) {
        let RefinementMaps {
            ref applied,
            ref changeset,
            total: _,
        } = new_latest_refinements;
        let applied = applied.dupe();
        let changeset = changeset.dupe();
        self.env_state
            .latest_refinements
            .push(new_latest_refinements);
        for (ssa_id, (lookup, refinement_id)) in &applied {
            let ssa_id = *ssa_id;
            let refinement_id = *refinement_id;
            let refine_val = |this: &mut Self, v: Val<ALoc>| {
                if ssa_val::base_id_of_val(&v) == ssa_id {
                    ssa_val::refinement(&mut this.cache.borrow_mut(), refinement_id.0, v)
                } else {
                    v
                }
            };
            let create_val_for_heap = changeset
                .0
                .get(lookup)
                .duped()
                .map(|v| move |_: &mut Self| v);
            self.map_val_with_lookup(lookup, create_val_for_heap, refine_val);
        }
    }

    // See pop_refinement_scope. The only difference here is that we unrefine values deeply
    // instead of just at the top level. The reason for this is that intermediate control-flow
    // can introduce refinement writes into phi nodes, and we don't want those refinements to
    // escape the scope of the loop. You may find it instructive to change the calls to
    // just pop_refinement_scope to see the behavioral differences.
    fn pop_refinement_scope_after_loop(&mut self) {
        let RefinementMaps {
            applied,
            changeset: _,
            total: _,
        } = self.env_state.latest_refinements.last().unwrap().dupe();
        self.env_state.latest_refinements.pop();
        for (lookup, refinement_id) in applied.values() {
            let refinement_id = *refinement_id;
            let unrefine_deeply = |this: &mut Self, x: Val<ALoc>| {
                ssa_val::unrefine_deeply(&mut this.cache.borrow_mut(), refinement_id.0, x)
            };
            self.map_val_with_lookup(lookup, None::<fn(&mut Self) -> Val<ALoc>>, unrefine_deeply);
        }
    }

    // Invariant refinement scopes can be popped, but the refinement should continue living on.
    // To model that, we pop the refinement scope but do not unrefine the refinements. The
    // refinements live on in the Refinement writes in the env.
    fn pop_refinement_scope_without_unrefining(&mut self) {
        self.env_state.latest_refinements.pop();
    }

    // When a refinement scope ends, we need to undo the refinement applied to the
    // variables mentioned in the latest_refinements head. Some of these values may no
    // longer be the refined value, in which case Val.unrefine will be a no-op. Otherwise,
    // the Refinement Val.t is replaced with the original Val.t that was being refined, with
    // the same original ssa_id. That means that if for some reason you needed to push the refinement
    // scope again that you would re-refine the unrefined variables, which is desirable in cases
    // where we juggle refinement scopes like we do for nullish coalescing
    fn pop_refinement_scope(&mut self) {
        let Some(refinement_maps) = self.env_state.latest_refinements.pop() else {
            return;
        };
        let applied = refinement_maps.applied;

        for (_, (lookup, refinement_id)) in applied.iter() {
            let refinement_id = *refinement_id;
            self.map_val_with_lookup(lookup, None::<fn(&mut Self) -> Val<ALoc>>, |_, v| {
                ssa_val::unrefine(refinement_id.0, v)
            });
        }
    }

    fn peek_new_refinements(&self) -> RefinementMaps {
        self.env_state
            .latest_refinements
            .last()
            .duped()
            .unwrap_or_else(RefinementMaps::empty)
    }

    fn negate_refinements(&mut self, maps: &RefinementMaps) -> RefinementMaps {
        match &maps.total {
            None => RefinementMaps::empty(),
            Some(total) => {
                let negated = RefinementProp::not(total.dupe());
                let (applied, changeset_map) = self.normalize_total_refinements(&negated);
                let changeset = Changeset(changeset_map.into_iter().collect());
                RefinementMaps {
                    applied: applied.into_iter().collect(),
                    changeset,
                    total: Some(negated),
                }
            }
        }
    }

    fn negate_new_refinements(&mut self) {
        let Some(head) = self.env_state.latest_refinements.last().duped() else {
            return;
        };
        let new_latest_refinements = self.negate_refinements(&head);
        self.pop_refinement_scope();
        self.push_refinement_scope(new_latest_refinements);
    }

    fn merge_self_refinement_scope(&mut self, other: &RefinementMaps) {
        let RefinementMaps {
            applied: _,
            changeset: _,
            total,
        } = other;
        let RefinementMaps {
            applied: _,
            changeset: _,
            total: head_total,
        } = self.env_state.latest_refinements.last().unwrap().dupe();
        let merged = conj_total(head_total, total.dupe());
        let (applied, changeset) = match &merged {
            Some(m) => self.normalize_total_refinements(m),
            None => (FlowOrdMap::new(), FlowOrdMap::new()),
        };
        let refis = RefinementMaps {
            applied: applied.into_iter().collect(),
            changeset: Changeset(changeset.into_iter().collect()),
            total: merged,
        };
        self.pop_refinement_scope();
        self.push_refinement_scope(refis);
    }

    fn normalize_total_refinements(
        &mut self,
        total: &RefinementProp,
    ) -> (
        FlowOrdMap<usize, (Lookup, RefinementId)>,
        FlowOrdMap<Lookup, Val<ALoc>>,
    ) {
        fn nnf(total: &RefinementProp) -> RefinementProp {
            match &**total {
                RefinementPropInner::Not(inner) => match &**inner {
                    RefinementPropInner::And(t1, t2) => RefinementProp::or(
                        nnf(&RefinementProp::not(t1.dupe())),
                        nnf(&RefinementProp::not(t2.dupe())),
                    ),
                    RefinementPropInner::Or(t1, t2) => RefinementProp::and(
                        nnf(&RefinementProp::not(t1.dupe())),
                        nnf(&RefinementProp::not(t2.dupe())),
                    ),
                    RefinementPropInner::Not(t) => nnf(t),
                    RefinementPropInner::Refinements { .. } => total.dupe(),
                },
                RefinementPropInner::And(t1, t2) => RefinementProp::and(nnf(t1), nnf(t2)),
                RefinementPropInner::Or(t1, t2) => RefinementProp::or(nnf(t1), nnf(t2)),
                RefinementPropInner::Refinements { .. } => total.dupe(),
            }
        }
        fn recur<'a, Cx: Context, Fl: Flow<Cx = Cx>>(
            resolver: &mut NameResolver<'a, Cx, Fl>,
            total: &RefinementProp,
        ) -> (
            FlowOrdMap<usize, (Lookup, RefinementId)>,
            FlowOrdMap<Lookup, Val<ALoc>>,
        ) {
            match &**total {
                RefinementPropInner::Refinements {
                    applied: r,
                    changeset: c,
                } => (r.dupe(), c.dupe()),
                RefinementPropInner::Not(inner)
                    if matches!(&**inner, RefinementPropInner::Refinements { .. }) =>
                {
                    let RefinementPropInner::Refinements {
                        applied: r,
                        changeset: c,
                    } = &**inner
                    else {
                        unreachable!()
                    };
                    let negated_r: FlowOrdMap<usize, (Lookup, RefinementId)> = r
                        .iter()
                        .map(|(ssa_id, (lookup, refinement_id))| {
                            let new_refinement_id = resolver.new_id();
                            resolver.env_state.refinement_heap.borrow_mut().insert(
                                RefinementId(new_refinement_id),
                                RefinementChain::Not(refinement_id.0),
                            );
                            (*ssa_id, (lookup.dupe(), RefinementId(new_refinement_id)))
                        })
                        .collect();
                    (negated_r, c.dupe())
                }
                RefinementPropInner::Not(_) => {
                    panic!("Env_invariant: Impossible - Negations not resolved")
                }
                RefinementPropInner::And(t1, t2) => {
                    let (r1, c1) = recur(resolver, t1);
                    let (r2, c2) = recur(resolver, t2);
                    let mut r = r1;
                    for (ssa_id, (lookup2, rid2)) in r2.iter() {
                        let lookup1_opt = r.get(ssa_id).cloned();
                        if let Some((lookup1, rid1)) = lookup1_opt {
                            if lookup1 == *lookup2 {
                                let new_refinement_id = resolver.new_id();
                                resolver.env_state.refinement_heap.borrow_mut().insert(
                                    RefinementId(new_refinement_id),
                                    RefinementChain::And(rid1.0, rid2.0),
                                );
                                r.insert(*ssa_id, (lookup1, RefinementId(new_refinement_id)));
                            } else {
                                r.remove(ssa_id);
                            }
                        } else {
                            r.insert(*ssa_id, (lookup2.dupe(), *rid2));
                        }
                    }
                    let c = FlowOrdMap::from(c1.into_inner().union(c2.into_inner()));
                    (r, c)
                }
                RefinementPropInner::Or(t1, t2) => {
                    let (r1, c1) = recur(resolver, t1);
                    let (r2, c2) = recur(resolver, t2);
                    let mut r = FlowOrdMap::new();
                    for (ssa_id, (lookup1, rid1)) in r1.iter() {
                        if let Some((lookup2, rid2)) = r2.get(ssa_id) {
                            if lookup1 == lookup2 {
                                let new_refinement_id = resolver.new_id();
                                resolver.env_state.refinement_heap.borrow_mut().insert(
                                    RefinementId(new_refinement_id),
                                    RefinementChain::Or(rid1.0, rid2.0),
                                );
                                r.insert(
                                    *ssa_id,
                                    (lookup1.dupe(), RefinementId(new_refinement_id)),
                                );
                            }
                        }
                    }
                    let c = FlowOrdMap::from(c1.into_inner().union(c2.into_inner()));
                    (r, c)
                }
            }
        }
        let nnf_result = nnf(total);
        recur(self, &nnf_result)
    }

    fn commit_refinement(&mut self, refinements: BTreeMap<Lookup, (ALoc, Refinement<ALoc>)>) {
        let Some(head) = self.env_state.latest_refinements.last().cloned() else {
            return;
        };
        let RefinementMaps {
            applied,
            changeset: old_changeset,
            total,
        } = head;

        let (applied, changeset, map) = refinements.into_iter().fold(
            (applied, BTreeMap::new(), BTreeMap::new()),
            |(mut applied, mut changeset, mut map), (key, (loc, refinement))| {
                // Prevent refinement on undeclared const/let.
                let should_not_refine = if key.projections.is_empty() {
                    if let Some(env_val) = self.env_read_opt(&key.base) {
                        matches!(
                            env_val.kind,
                            BindingsKind::Const
                                | BindingsKind::Let
                                | BindingsKind::DeclaredConst
                                | BindingsKind::DeclaredLet
                        ) && ssa_val::is_undeclared_or_skipped(&env_val.val_ref.borrow())
                            && env_val.def_loc.is_some()
                    } else {
                        false
                    }
                } else {
                    false
                };

                if should_not_refine {
                    return (applied, changeset, map);
                }

                let add_refinements = |resolver: &mut Self, v: Val<ALoc>| {
                    let ssa_id = ssa_val::base_id_of_val(&v);
                    let refinement_id = resolver.new_id();
                    resolver.env_state.refinement_heap.borrow_mut().insert(
                        RefinementId(refinement_id),
                        RefinementChain::Base(refinement.dupe()),
                    );
                    let latest_refinement_opt = applied.get(&ssa_id).cloned();
                    let (final_refinement_id, unrefined_v) = match latest_refinement_opt {
                        Some((_, existing_refinement_id)) => {
                            let unrefined_v = ssa_val::unrefine(existing_refinement_id.0, v.dupe());
                            let new_refinement_id = resolver.new_id();
                            let new_chain =
                                RefinementChain::And(existing_refinement_id.0, refinement_id);
                            resolver
                                .env_state
                                .refinement_heap
                                .borrow_mut()
                                .insert(RefinementId(new_refinement_id), new_chain);
                            (new_refinement_id, unrefined_v)
                        }
                        None => (refinement_id, v),
                    };
                    (
                        (ssa_id, refinement_id, final_refinement_id),
                        ssa_val::refinement(
                            &mut *resolver.cache.borrow_mut(),
                            final_refinement_id,
                            unrefined_v,
                        ),
                    )
                };

                let key_for_heap = key.dupe();
                let loc_for_heap = loc.dupe();
                let result = self.map_val_with_lookup_result(
                    &key,
                    Some(|resolver: &mut Self| {
                        use flow_common::reason::mk_reason;
                        let refi_key = refinement_key::RefinementKey {
                            lookup: key_for_heap.dupe(),
                            loc: loc_for_heap.dupe(),
                        };
                        let reason = mk_reason(refi_key.reason_desc(), loc_for_heap.dupe());
                        resolver.env_state.write_entries.insert(
                            env_api::EnvKey::new(
                                env_api::DefLocType::OrdinaryNameLoc,
                                loc_for_heap.dupe(),
                            ),
                            env_api::EnvEntry::AssigningWrite(reason),
                        );
                        ssa_val::projection(&mut *resolver.cache.borrow_mut(), loc_for_heap.dupe())
                    }),
                    add_refinements,
                );

                match result {
                    Some(((ssa_id, base_refinement_id, final_refinement_id), change)) => {
                        applied.insert(ssa_id, (key.dupe(), RefinementId(final_refinement_id)));
                        if let Some(change) = change {
                            changeset.insert(key.dupe(), change);
                        }
                        map.insert(ssa_id, (key, RefinementId(base_refinement_id)));
                        (applied, changeset, map)
                    }
                    None => (applied, changeset, map),
                }
            },
        );

        let merged_changeset: FlowOrdMap<Lookup, Val<ALoc>> = {
            let mut merged: FlowOrdMap<Lookup, Val<ALoc>> = changeset
                .iter()
                .map(|(k, v)| (k.dupe(), v.dupe()))
                .collect();
            for (k, v) in old_changeset {
                if !merged.contains_key(&k) {
                    merged.insert(k, v);
                }
            }
            merged
        };
        let changeset_for_prop: FlowOrdMap<Lookup, Val<ALoc>> = changeset.into_iter().collect();
        let map_for_prop: FlowOrdMap<usize, (Lookup, RefinementId)> = map.into_iter().collect();
        let new_total = conj_total(
            total,
            Some(RefinementProp::refinements(
                map_for_prop,
                changeset_for_prop,
            )),
        );
        if let Some(latest) = self.env_state.latest_refinements.last_mut() {
            latest.applied = applied;
            latest.changeset = Changeset(merged_changeset);
            latest.total = new_total;
        }
    }

    fn add_single_refinement(
        &mut self,
        key: &refinement_key::RefinementKey<ALoc>,
        refining_locs: FlowOrdSet<ALoc>,
        refi_kind: env_api::RefinementKind<ALoc>,
    ) {
        let refis = self.start_refinement(key, refining_locs, refi_kind);
        self.commit_refinement(refis);
    }

    fn add_pred_func_info(
        &mut self,
        callee_loc: ALoc,
        call_expr: Expression<ALoc, ALoc>,
        callee: Expression<ALoc, ALoc>,
        targs: Option<CallTypeArgs<ALoc, ALoc>>,
        arguments: ArgList<ALoc, ALoc>,
    ) {
        self.env_state.pred_func_map.insert(
            callee_loc,
            env_api::PredFuncInfo {
                class_stack: self.class_stack.dupe(),
                call_expr,
                callee,
                targs,
                arguments,
            },
        );
    }

    fn start_refinement(
        &self,
        key: &refinement_key::RefinementKey<ALoc>,
        refining_locs: FlowOrdSet<ALoc>,
        refi_kind: env_api::RefinementKind<ALoc>,
    ) -> BTreeMap<Lookup, (ALoc, Refinement<ALoc>)> {
        let mut result = BTreeMap::new();
        let refinement = Refinement {
            refining_locs,
            kind: refi_kind,
        };
        result.insert(key.lookup.dupe(), (key.loc.dupe(), refinement));
        result
    }

    fn extend_refinement(
        &self,
        key: &refinement_key::RefinementKey<ALoc>,
        refining_locs: FlowOrdSet<ALoc>,
        refi_kind: env_api::RefinementKind<ALoc>,
        mut refis: BTreeMap<Lookup, (ALoc, Refinement<ALoc>)>,
    ) -> BTreeMap<Lookup, (ALoc, Refinement<ALoc>)> {
        use env_api::RefinementKind;

        let new_refinement = Refinement {
            refining_locs: refining_locs.dupe(),
            kind: refi_kind,
        };

        refis
            .entry(key.lookup.dupe())
            .and_modify(|(loc1, existing)| {
                if *loc1 != key.loc {
                    panic!("Env_invariant: Loc mismatch");
                }
                existing.refining_locs = existing.refining_locs.dupe().union(refining_locs.dupe());
                existing.kind = RefinementKind::AndR(
                    Rc::new(existing.kind.dupe()),
                    Rc::new(new_refinement.kind.dupe()),
                );
            })
            .or_insert((key.loc.dupe(), new_refinement));

        refis
    }

    fn identifier_refinement(&mut self, loc: &ALoc, name: FlowSmolStr) {
        self.any_identifier(loc.dupe(), &name);

        if let Some(env_val) = self.env_read_opt(&name) {
            if !ssa_val::is_undeclared_or_skipped(&env_val.val_ref.borrow()) {
                let key = refinement_key::RefinementKey::of_name(name, loc.dupe());
                let mut refining_locs = empty_refining_locs();
                refining_locs.insert(loc.dupe());
                self.add_single_refinement(&key, refining_locs, env_api::RefinementKind::TruthyR);
            }
        }
    }

    fn assignment_refinement(
        &mut self,
        loc: ALoc,
        assignment: &flow_parser::ast::expression::Assignment<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::pattern::Pattern;

        self.assignment(&loc, assignment)?;
        let (left, _) = ast_utils::unwrap_nonnull_lhs(&assignment.left);

        match &*left {
            Pattern::Identifier { loc: id_loc, inner } => {
                let key =
                    refinement_key::RefinementKey::of_name(inner.name.name.dupe(), id_loc.dupe());
                let mut refining_locs = empty_refining_locs();
                refining_locs.insert(loc);
                self.add_single_refinement(&key, refining_locs, env_api::RefinementKind::TruthyR);
            }
            _ => {}
        }
        Ok(())
    }

    fn merge(
        &mut self,
        conjunction: bool,
        maps1: &RefinementMaps,
        maps2: &RefinementMaps,
    ) -> RefinementMaps {
        // let total = if conjunction then ...
        let total = match (&maps1.total, &maps2.total) {
            (Some(t1), Some(t2)) => {
                if conjunction {
                    Some(RefinementProp::and(t1.dupe(), t2.dupe()))
                } else {
                    Some(RefinementProp::or(t1.dupe(), t2.dupe()))
                }
            }
            (Some(t), None) | (None, Some(t)) => {
                let empty_refis = RefinementProp::refinements(FlowOrdMap::new(), FlowOrdMap::new());
                if conjunction {
                    Some(RefinementProp::and(t.dupe(), empty_refis))
                } else {
                    Some(RefinementProp::or(t.dupe(), empty_refis))
                }
            }
            (None, None) => None,
        };
        let (applied, changeset) = match &total {
            Some(t) => {
                let (app, cset) = self.normalize_total_refinements(t);
                (
                    app.into_iter().collect(),
                    Changeset(cset.into_iter().collect()),
                )
            }
            None => (FlowOrdMap::new(), Changeset::default()),
        };
        RefinementMaps {
            applied,
            changeset,
            total,
        }
    }

    fn merge_refinement_scopes(
        &mut self,
        conjunction: bool,
        lhs: &RefinementMaps,
        rhs: &RefinementMaps,
    ) {
        let new_latest_refinements = self.merge(conjunction, lhs, rhs);
        self.merge_self_refinement_scope(&new_latest_refinements);
    }

    // Refines an expr if that expr has a refinement key, othewise does nothing
    fn add_refinement_to_expr(
        &mut self,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        refining_locs: FlowOrdSet<ALoc>,
        refi_kind: env_api::RefinementKind<ALoc>,
    ) {
        if let Some(key) = refinement_key::RefinementKey::of_expression(expr) {
            self.add_single_refinement(&key, refining_locs, refi_kind);
        }
    }

    fn logical_refinement(
        &mut self,
        operator: LogicalOperator,
        left_expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        right_expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;
        use flow_parser::ast::expression::LogicalOperator as Op;

        let loc = left_expr.loc().dupe();

        self.push_refinement_scope(RefinementMaps::empty());
        // The RHS is _only_ evaluated if the LHS fails its check. That means that patterns like
        // x || invariant(false) should propagate the truthy refinement to the next line. We keep track
        // of the completion state on the rhs to do that. If the LHS throws then the entire expression
        // throws, so there's no need to catch the exception from the LHS
        let (lhs_latest_refinements, rhs_latest_refinements, env1, rhs_completion_state) =
            match operator {
                Op::Or | Op::And => {
                    self.expression_refinement(left_expr)?;
                    let lhs_latest_refinements_or = self.peek_new_refinements();
                    let env1 = self.env_snapshot_without_latest_refinements();

                    if matches!(operator, Op::Or) {
                        self.negate_new_refinements();
                    }

                    self.push_refinement_scope(RefinementMaps::empty());
                    let rhs_completion_state = self.run_to_completion(|this| {
                        this.expression_refinement(right_expr)?;
                        Ok(())
                    });
                    let rhs_latest_refinements = self.peek_new_refinements();
                    // Pop RHS refinement scope
                    self.pop_refinement_scope();

                    // If this is `or`, the RHS did not fire if the LHS was truthy, so we want
                    // the original, un-havoced refinements
                    // If this is `and`, we want to save the LHS refinements that may have been
                    // havoced by the RHS.
                    let lhs_latest_refinements = match operator {
                        Op::Or => lhs_latest_refinements_or,
                        _ => self.peek_new_refinements(),
                    };
                    // Pop LHS refinement scope
                    self.pop_refinement_scope();
                    (
                        lhs_latest_refinements,
                        rhs_latest_refinements,
                        env1,
                        rhs_completion_state,
                    )
                }
                Op::NullishCoalesce => {
                    // If this overall expression is truthy, then either the LHS or the RHS has to be truthy.
                    // If it's because the LHS is truthy, then the LHS also has to be non-maybe (this is of course
                    // true by definition, but it's also true because of the nature of ??).
                    // But if we're evaluating the RHS, the LHS doesn't have to be truthy, it just has to be
                    // non-maybe. As a result, we do this weird dance of refinements so that when we traverse the
                    // RHS we have done the null-test but the overall result of this expression includes both the
                    // truthy and non-maybe qualities.
                    //
                    // We can't use null_test here because null_test requires some actual null for the
                    // sentinel refinement it can create. We can add some complexity here to introduce a
                    // synthetic null Val.t and get sentinel refinements against null here, but that seems
                    // like an unlikely way for nullish coalescing to be used. Instead, we simply add a
                    // NotR MaybeR refinement to the left
                    self.expression(left_expr)?;
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    self.add_refinement_to_expr(
                        left_expr,
                        refining_locs.dupe(),
                        RefinementKind::NotR(Rc::new(RefinementKind::MaybeR)),
                    );
                    let nullish = self.peek_new_refinements();
                    let env1 = self.env_snapshot_without_latest_refinements();
                    self.negate_new_refinements();

                    self.push_refinement_scope(RefinementMaps::empty());
                    let rhs_completion_state = self.run_to_completion(|this| {
                        this.expression_refinement(right_expr)?;
                        Ok(())
                    });
                    let rhs_latest_refinements = self.peek_new_refinements();
                    self.pop_refinement_scope();
                    self.pop_refinement_scope();

                    self.push_refinement_scope(RefinementMaps::empty());
                    self.add_refinement_to_expr(left_expr, refining_locs, RefinementKind::TruthyR);
                    let truthy_refinements = self.peek_new_refinements();
                    self.pop_refinement_scope();

                    self.push_refinement_scope(RefinementMaps::empty());
                    self.merge_refinement_scopes(true, &nullish, &truthy_refinements);
                    let lhs_latest_refinements = self.peek_new_refinements();
                    self.pop_refinement_scope();

                    (
                        lhs_latest_refinements,
                        rhs_latest_refinements,
                        env1,
                        rhs_completion_state,
                    )
                }
            };

        let conjunction = matches!(operator, Op::And);

        match rhs_completion_state {
            Some(AbruptCompletion::Throw) => {
                let env2 = self.env_snapshot();
                self.reset_env(&env1);
                self.push_refinement_scope(lhs_latest_refinements);
                self.pop_refinement_scope_without_unrefining();
                self.merge_self_env(&env2);
            }
            _ => {
                self.merge_self_env(&env1);
                self.merge_refinement_scopes(
                    conjunction,
                    &lhs_latest_refinements,
                    &rhs_latest_refinements,
                );
            }
        }
        Ok(())
    }

    /// Handle null test refinement.
    // method null_test ~sense ~strict loc expr other =
    fn null_test(
        &mut self,
        sense: bool,
        strict: bool,
        loc: ALoc,
        expr: &Expression<ALoc, ALoc>,
        other: &Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;

        let other_loc = other.loc().dupe();

        if strict {
            // It's easier to reason about the following code if we only consider
            // `foo?.bar === null` for now.
            // We will have sentinel refinement on `foo` and is-null refinement `foo.bar` linked
            // together.
            // In addition to the linked refinement above, we also have a separate implicit
            // refinement on the optional chaining. Since we know that the prop foo.bar must be
            // exactly null and thus `foo` must not be maybe.
            //
            // Now for the sense=false case, we simply negative everything at the end. The linked
            // refinement will be negated together.
            let refis = self.maybe_sentinel(true, strict, loc.dupe(), expr, other_loc.dupe());
            let will_negate = !sense;
            let refis = self.maybe_prop_nullish(
                will_negate,
                sense,
                strict,
                loc.dupe(),
                expr,
                other_loc.dupe(),
                other,
                refis,
            );
            let refis = match refinement_key::RefinementKey::of_expression(expr) {
                None => refis,
                Some(key) => {
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    self.extend_refinement(&key, refining_locs, RefinementKind::NullR, refis)
                }
            };
            self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
                OptionalChainingRefinement::CanApplyPropIsExactlyNullRefi,
                expr,
            )?;
            self.commit_refinement(refis);
            if will_negate {
                self.negate_new_refinements();
            }
        } else {
            // It's easier to reason about the following code if we only consider
            // `foo?.bar != null` for now.
            // We will have sentinel refinement on `foo` and non-maybe refinement `foo.bar` linked
            // together.
            // In addition to the linked refinement above, we also have a separate implicit
            // refinement on the optional chaining. Since we know that the prop foo.bar must exist
            // and thus `foo` must not be maybe.
            //
            // Now for the sense=true case, we simply negative everything at the end. The linked
            // refinement will be negated together.
            // Negating if sense is false is handled by negate_new_refinements.
            let refis = self.maybe_sentinel(true, strict, loc.dupe(), expr, other_loc.dupe());
            let will_negate = sense;
            let refis = self.maybe_prop_nullish(
                will_negate,
                sense,
                strict,
                loc.dupe(),
                expr,
                other_loc.dupe(),
                other,
                refis,
            );
            let refis = match refinement_key::RefinementKey::of_expression(expr) {
                None => refis,
                Some(key) => {
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    self.extend_refinement(
                        &key,
                        refining_locs,
                        RefinementKind::NotR(Rc::new(RefinementKind::MaybeR)),
                        refis,
                    )
                }
            };
            self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
                OptionalChainingRefinement::CanApplyPropNonNullishRefi,
                expr,
            )?;
            self.commit_refinement(refis);
            if will_negate {
                self.negate_new_refinements();
            }
        }
        Ok(())
    }

    fn is_global_undefined(&mut self) -> bool {
        let undefined_name = FlowSmolStr::new_inline("undefined");
        match self.env_read_opt(&undefined_name) {
            None => false,
            Some(entry) => ssa_val::is_global_undefined(&entry.val_ref.borrow()),
        }
    }

    fn void_test(
        &mut self,
        sense: bool,
        strict: bool,
        check_for_bound_undefined: bool,
        loc: ALoc,
        expr: &Expression<ALoc, ALoc>,
        other: &Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;

        self.expression(other)?;
        let other_loc = other.loc().dupe();
        // Negating if sense is true is handled by negate_new_refinements.
        let refis = self.maybe_sentinel(false, strict, loc.dupe(), expr, other_loc.dupe());
        if !check_for_bound_undefined || self.is_global_undefined() {
            // It's easier to reason about the following code if we only consider
            // `foo?.bar !== undefined` for now.
            // We will have sentinel refinement on `foo` and non-void refinement `foo.bar` linked
            // together.
            // In addition to the linked refinement above, we also have a separate implicit
            // refinement on the optional chaining. Since we know that the prop foo.bar must be void
            // and thus `foo` must not be maybe.
            //
            // Now for the sense=true case, we simply negative everything at the end. The linked
            // refinement will be negated together.
            let will_negate = sense;
            let refis = self.maybe_prop_nullish(
                will_negate,
                sense,
                strict,
                loc.dupe(),
                expr,
                other_loc.dupe(),
                other,
                refis,
            );
            let refis = match refinement_key::RefinementKey::of_expression(expr) {
                None => refis,
                Some(key) => {
                    // let refinement = if strict then NotR UndefinedR else NotR MaybeR in
                    let refinement = if strict {
                        RefinementKind::NotR(Rc::new(RefinementKind::UndefinedR))
                    } else {
                        RefinementKind::NotR(Rc::new(RefinementKind::MaybeR))
                    };
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    self.extend_refinement(&key, refining_locs, refinement, refis)
                }
            };
            let can_refine_obj_prop = if strict {
                OptionalChainingRefinement::CanApplyPropNonVoidRefi
            } else {
                OptionalChainingRefinement::CanApplyPropNonNullishRefi
            };
            self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
                can_refine_obj_prop,
                expr,
            )?;
            self.commit_refinement(refis);
            if will_negate {
                self.negate_new_refinements();
            }
        } else {
            self.expression(expr)?;
            self.commit_refinement(refis);
        }
        Ok(())
    }

    fn typeof_test(
        &mut self,
        loc: ALoc,
        arg: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        str_loc: ALoc,
        typename: &str,
        sense: bool,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;

        let (refinement, undef) = match typename {
            "boolean" => (Some(RefinementKind::BoolR(loc.dupe())), false),
            "function" => (Some(RefinementKind::FunctionR), false),
            "number" => (Some(RefinementKind::NumberR(loc.dupe())), false),
            "object" => (Some(RefinementKind::ObjectR), false),
            "string" => (Some(RefinementKind::StringR(loc.dupe())), false),
            "symbol" => (Some(RefinementKind::SymbolR(loc.dupe())), false),
            "undefined" => (Some(RefinementKind::UndefinedR), true),
            "bigint" => (Some(RefinementKind::BigIntR(loc.dupe())), false),
            _ => {
                Fl::add_output(
                    self.cx,
                    ErrorMessage::EInvalidTypeof(Box::new((str_loc, typename.into()))),
                );
                (None, false)
            }
        };

        match (
            refinement,
            refinement_key::RefinementKey::of_expression(arg),
        ) {
            (Some(refi), Some(refinement_key)) => {
                if undef {
                    // It's easier to reason about the following code if we only consider
                    // `typeof foo?.bar !== 'undefined'` for now.
                    // First, we will have non-void refinement `foo.bar`.
                    // In addition, we also have a separate implicit refinement on the optional chaining.
                    // Since we know that the prop foo.bar must exist and thus `foo` must not be maybe.
                    //
                    // Now for the sense=true case, we simply negative everything at the end.
                    self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
                        OptionalChainingRefinement::CanApplyPropNonVoidRefi,
                        arg,
                    )?;
                    let refinement = RefinementKind::NotR(Rc::new(refi));
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc);
                    self.add_single_refinement(&refinement_key, refining_locs, refinement);
                    if sense {
                        self.negate_new_refinements();
                    }
                } else {
                    // It's easier to reason about the following code if we only consider
                    // `typeof foo?.bar === 'string'` for now.
                    // First, we will have typeof refinement on `foo.bar`.
                    // In addition, we also have a separate implicit refinement on the optional chaining.
                    // Since we know that the prop foo.bar must not be nullish and thus `foo` must not be maybe.
                    //
                    // Now for the sense=false case, we simply negative everything at the end.
                    self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
                        OptionalChainingRefinement::CanApplyPropNonNullishRefi,
                        arg,
                    )?;
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc);
                    self.add_single_refinement(&refinement_key, refining_locs, refi);
                    if !sense {
                        self.negate_new_refinements();
                    }
                }
            }
            _ => {
                self.expression(arg)?;
            }
        }
        Ok(())
    }

    // method literal_test ~strict ~sense loc expr refinement other =
    fn literal_test(
        &mut self,
        strict: bool,
        sense: bool,
        loc: ALoc,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        refinement: env_api::RefinementKind<ALoc>,
        other: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        // It's easier to reason about the following code if we only consider
        // `foo?.bar === 3` for now.
        // First, we will have literal refinement on `foo.bar`.
        // In addition, we also have a separate implicit refinement on the optional chaining.
        // Since we know that the prop foo.bar must not be nullish and thus `foo` must not be maybe.
        //
        // Now for the sense=true case, we simply negative everything at the end.
        let other_loc = other.loc().dupe();
        let mut refis = self.maybe_sentinel(true, strict, loc.dupe(), expr, other_loc);

        refis = match refinement_key::RefinementKey::of_expression(expr) {
            Some(key) if strict => {
                let mut refining_locs = empty_refining_locs();
                refining_locs.insert(loc.dupe());
                self.extend_refinement(&key, refining_locs, refinement, refis)
            }
            _ => refis,
        };
        self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
            OptionalChainingRefinement::CanApplyPropNonNullishRefi,
            expr,
        )?;
        self.commit_refinement(refis);
        if !sense {
            self.negate_new_refinements();
        }
        Ok(())
    }

    fn maybe_prop_nullish(
        &mut self,
        will_negate: bool,
        sense: bool,
        strict: bool,
        loc: ALoc,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        other_loc: ALoc,
        other: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        refis: BTreeMap<Lookup, (ALoc, Refinement<ALoc>)>,
    ) -> BTreeMap<Lookup, (ALoc, Refinement<ALoc>)> {
        use env_api::RefinementKind;
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;
        use flow_parser::ast::expression::ExpressionInner;
        use flow_parser::ast::expression::member::Property as MemberProperty;

        // let open Flow_ast in
        // if strict then refis
        if strict {
            return refis;
        }

        let member_expr = match expr.deref() {
            ExpressionInner::OptionalMember { inner, .. } => Some(&inner.member),
            ExpressionInner::Member { inner, .. } => Some(inner.as_ref()),
            _ => None,
        };
        let get_refis = |this: &mut Self,
                         ploc: ALoc,
                         _object: &Expression<ALoc, ALoc>,
                         prop_name: FlowSmolStr|
         -> BTreeMap<Lookup, (ALoc, Refinement<ALoc>)> {
            let sentinel = match other.deref() {
                ExpressionInner::NullLiteral { .. } => Some(RefinementKind::PropNullishR {
                    propname: prop_name.dupe(),
                    loc: other_loc.dupe(),
                }),
                ExpressionInner::Identifier { inner, .. }
                    if inner.name == "undefined" && this.is_global_undefined() =>
                {
                    Some(RefinementKind::PropNullishR {
                        propname: prop_name.dupe(),
                        loc: other_loc.dupe(),
                    })
                }
                ExpressionInner::Unary { inner, .. }
                    if inner.operator == flow_parser::ast::expression::UnaryOperator::Void =>
                {
                    Some(RefinementKind::PropNullishR {
                        propname: prop_name.dupe(),
                        loc: other_loc.dupe(),
                    })
                }
                _ => None,
            };

            match (
                refinement_key::RefinementKey::of_expression(_object),
                sentinel,
            ) {
                (Some(refinement_key), Some(sent)) => {
                    let reason = VirtualReason::new(
                        VirtualReasonDesc::RProperty(Some(flow_common::reason::Name::new(
                            prop_name,
                        ))),
                        ploc,
                    );
                    this.env_state.write_entries.insert(
                        env_api::EnvKey::new(env_api::DefLocType::ExpressionLoc, other_loc.dupe()),
                        env_api::EnvEntry::AssigningWrite(reason),
                    );
                    let refinement = if sense {
                        sent.dupe()
                    } else {
                        RefinementKind::NotR(Rc::new(sent.dupe()))
                    };
                    let refinement = if will_negate {
                        RefinementKind::NotR(Rc::new(refinement))
                    } else {
                        refinement
                    };
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    this.extend_refinement(
                        &refinement_key,
                        refining_locs,
                        refinement,
                        refis.clone(),
                    )
                }
                _ => refis.clone(),
            }
        };

        match member_expr {
            Some(member) => match &member.property {
                MemberProperty::PropertyIdentifier(id) => {
                    get_refis(self, id.loc.dupe(), &member.object, id.name.dupe())
                }
                MemberProperty::PropertyExpression(prop_expr) => match prop_expr.deref() {
                    ExpressionInner::StringLiteral {
                        loc: ploc, inner, ..
                    } => get_refis(self, ploc.dupe(), &member.object, inner.value.dupe()),
                    ExpressionInner::NumberLiteral {
                        loc: ploc, inner, ..
                    } if flow_common::js_number::is_float_safe_integer(inner.value) => {
                        let prop_name = flow_common::js_number::ecma_string_of_float(inner.value);
                        get_refis(
                            self,
                            ploc.dupe(),
                            &member.object,
                            FlowSmolStr::new(prop_name),
                        )
                    }
                    _ => refis,
                },
                _ => refis,
            },
            None => refis,
        }
    }

    fn maybe_sentinel(
        &mut self,
        sense: bool,
        strict: bool,
        loc: ALoc,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        other_loc: ALoc,
    ) -> BTreeMap<Lookup, (ALoc, Refinement<ALoc>)> {
        use env_api::RefinementKind;
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;
        use flow_parser::ast::expression::ExpressionInner;
        use flow_parser::ast::expression::member::Property as MemberProperty;

        let member_expr = match expr.deref() {
            ExpressionInner::OptionalMember { inner, .. } => Some(&inner.member),
            ExpressionInner::Member { inner, .. } => Some(inner.as_ref()),
            _ => None,
        };

        let get_refis = |this: &mut Self,
                         ploc: ALoc,
                         _object: &Expression<ALoc, ALoc>,
                         prop_name: FlowSmolStr| {
            match refinement_key::RefinementKey::of_expression(_object) {
                Some(refinement_key) => {
                    let reason = VirtualReason::new(
                        VirtualReasonDesc::RProperty(Some(flow_common::reason::Name::new(
                            prop_name.dupe(),
                        ))),
                        ploc,
                    );
                    this.env_state.write_entries.insert(
                        env_api::EnvKey::new(env_api::DefLocType::ExpressionLoc, other_loc.dupe()),
                        env_api::EnvEntry::AssigningWrite(reason),
                    );
                    let refinement = if sense {
                        RefinementKind::SentinelR {
                            prop: prop_name,
                            other_loc: other_loc.dupe(),
                        }
                    } else {
                        RefinementKind::NotR(Rc::new(RefinementKind::SentinelR {
                            prop: prop_name,
                            other_loc: other_loc.dupe(),
                        }))
                    };
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    this.start_refinement(&refinement_key, refining_locs, refinement)
                }
                None => BTreeMap::new(),
            }
        };

        match (strict, member_expr) {
            (true, Some(member)) => match &member.property {
                MemberProperty::PropertyIdentifier(id) => {
                    get_refis(self, id.loc.dupe(), &member.object, id.name.dupe())
                }
                MemberProperty::PropertyExpression(prop_expr) => match prop_expr.deref() {
                    ExpressionInner::StringLiteral {
                        loc: ploc, inner, ..
                    } => get_refis(self, ploc.dupe(), &member.object, inner.value.dupe()),
                    ExpressionInner::NumberLiteral {
                        loc: ploc, inner, ..
                    } if flow_common::js_number::is_float_safe_integer(inner.value) => {
                        let prop_name = flow_common::js_number::ecma_string_of_float(inner.value);
                        get_refis(
                            self,
                            ploc.dupe(),
                            &member.object,
                            FlowSmolStr::new(prop_name),
                        )
                    }
                    _ => BTreeMap::new(),
                },
                _ => BTreeMap::new(),
            },
            _ => BTreeMap::new(),
        }
    }

    fn eq_test(
        &mut self,
        strict: bool,
        sense: bool,
        cond_context: CondContext,
        loc: ALoc,
        left: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        right: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;
        use flow_env_builder::eq_test::EqTestAction;

        let eq = |this: &mut Self,
                  refis: BTreeMap<Lookup, (ALoc, Refinement<ALoc>)>,
                  left: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
                  right: &flow_parser::ast::expression::Expression<ALoc, ALoc>|
         -> BTreeMap<Lookup, (ALoc, Refinement<ALoc>)> {
            let right_loc = right.loc().dupe();
            match (
                refinement_key::RefinementKey::of_expression(left),
                refinement_key::RefinementKey::of_expression(right),
            ) {
                (Some(lhs_key), Some(rhs_key))
                    if strict && lhs_key.lookup.base != rhs_key.lookup.base =>
                {
                    let reason =
                        flow_common::reason::VirtualReason::new(lhs_key.reason_desc(), loc.dupe());
                    this.env_state.write_entries.insert(
                        env_api::EnvKey::new(env_api::DefLocType::ExpressionLoc, right_loc.dupe()),
                        EnvEntry::AssigningWrite(reason),
                    );
                    let refinement = RefinementKind::EqR(right_loc);
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    this.extend_refinement(&lhs_key, refining_locs, refinement, refis)
                }
                _ => refis,
            }
        };

        let action = flow_env_builder::eq_test::dispatch_eq_test(
            cond_context == CondContext::SwitchTest,
            strict,
            sense,
            loc.dupe(),
            left,
            right,
        );

        match action {
            EqTestAction::TypeOf(loc, arg, str_expr, typename, sense) => {
                let str_loc = str_expr.loc().dupe();
                self.typeof_test(loc, arg, str_loc, typename, sense)?;
            }
            EqTestAction::Literal(strict, sense, loc, expr, refinement, other) => {
                self.literal_test(strict, sense, loc, expr, refinement, other)?;
            }
            EqTestAction::Null(sense, strict, loc, expr, other) => {
                self.null_test(sense, strict, loc, expr, other)?;
            }
            EqTestAction::Void(sense, strict, check, loc, expr, other) => {
                self.void_test(sense, strict, check, loc, expr, other)?;
            }
            EqTestAction::MemberEqOther(expr, other) => {
                self.expression(expr)?;
                let other_loc = other.loc().dupe();
                let refis = self.maybe_sentinel(true, strict, loc.dupe(), expr, other_loc);
                let refis = eq(self, refis, expr, other);
                self.expression(other)?;
                self.commit_refinement(refis);
                if !sense {
                    self.negate_new_refinements();
                }
            }
            EqTestAction::OtherEqMember(other, expr) => {
                let other_loc = other.loc().dupe();
                let refis = self.maybe_sentinel(true, strict, loc.dupe(), expr, other_loc);
                let refis = eq(self, refis, other, expr);
                self.expression(other)?;
                self.expression(expr)?;
                self.commit_refinement(refis);
                if !sense {
                    self.negate_new_refinements();
                }
            }
            EqTestAction::Other(left, right) => {
                let refis = eq(self, BTreeMap::new(), left, right);
                self.expression(left)?;
                self.expression(right)?;
                self.commit_refinement(refis);
                if !sense {
                    self.negate_new_refinements();
                }
            }
        }
        Ok(())
    }

    fn in_test(
        &mut self,
        loc: ALoc,
        prop_expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        obj_expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;
        use flow_parser::ast::expression::ExpressionInner;

        self.expression(prop_expr)?;
        // We already ensure that RHS of `in` must be object,
        // so the expression must be at least truthy as well
        self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
            OptionalChainingRefinement::CanApplyPropTruthyRefi,
            obj_expr,
        )?;

        match refinement_key::RefinementKey::of_expression(obj_expr) {
            None => {}
            Some(obj_refinement_key) => {
                let propname: Option<FlowSmolStr> = match prop_expr.deref() {
                    ExpressionInner::StringLiteral { inner, .. } => Some(inner.value.dupe()),
                    ExpressionInner::NumberLiteral { inner, .. }
                        if flow_common::js_number::is_float_safe_integer(inner.value) =>
                    {
                        Some(FlowSmolStr::new(
                            flow_common::js_number::ecma_string_of_float(inner.value),
                        ))
                    }
                    _ => None,
                };

                if let Some(propname) = propname {
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    self.add_single_refinement(
                        &obj_refinement_key,
                        refining_locs,
                        RefinementKind::PropExistsR { propname, loc },
                    );
                }
            }
        }
        Ok(())
    }

    fn instance_test(
        &mut self,
        context: env_api::refi::InstanceofContext,
        loc: ALoc,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        instance: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;

        // We already ensure that RHS of instanceof must be object,
        // so the expression must be at least truthy as well
        self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
            OptionalChainingRefinement::CanApplyPropTruthyRefi,
            expr,
        )?;

        self.expression(instance)?;
        match refinement_key::RefinementKey::of_expression(expr) {
            None => {}
            Some(refinement_key) => {
                let instance_loc = instance.loc().dupe();
                // instance is not something the name_resolver can reason about.
                // However, we still need to has a read and write entry, so we can
                // record it in statement.ml and use it in new-env.
                let reason = flow_common::reason::VirtualReason::new(
                    refinement_key.reason_desc(),
                    instance_loc.dupe(),
                );
                self.env_state.write_entries.insert(
                    env_api::EnvKey::new(env_api::DefLocType::ExpressionLoc, instance_loc),
                    env_api::EnvEntry::AssigningWrite(reason),
                );
                let mut refining_locs = empty_refining_locs();
                refining_locs.insert(loc);
                self.add_single_refinement(
                    &refinement_key,
                    refining_locs,
                    RefinementKind::InstanceOfR {
                        expr: Rc::new(instance.clone()),
                        context,
                    },
                );
            }
        }
        Ok(())
    }

    fn binary_refinement(
        &mut self,
        loc: ALoc,
        expr: &flow_parser::ast::expression::Binary<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::refi::InstanceofContext;
        use flow_parser::ast::expression::BinaryOperator as Operator;

        let operator = &expr.operator;
        let left = &expr.left;
        let right = &expr.right;
        match operator {
            // == and != refine if lhs or rhs is an ident and other side is null
            Operator::Equal => {
                self.eq_test(false, true, CondContext::OtherTest, loc.dupe(), left, right)?
            }
            Operator::NotEqual => self.eq_test(
                false,
                false,
                CondContext::OtherTest,
                loc.dupe(),
                left,
                right,
            )?,
            Operator::StrictEqual => {
                self.eq_test(true, true, CondContext::OtherTest, loc.dupe(), left, right)?
            }
            Operator::StrictNotEqual => {
                self.eq_test(true, false, CondContext::OtherTest, loc.dupe(), left, right)?
            }
            Operator::Instanceof => {
                self.instance_test(InstanceofContext::InstanceOfExpr, loc, left, right)?;
            }
            Operator::In => {
                self.in_test(loc, left, right)?;
            }
            Operator::LessThan
            | Operator::LessThanEqual
            | Operator::GreaterThan
            | Operator::GreaterThanEqual
            | Operator::LShift
            | Operator::RShift
            | Operator::RShift3
            | Operator::Plus
            | Operator::Minus
            | Operator::Mult
            | Operator::Exp
            | Operator::Div
            | Operator::Mod
            | Operator::BitOr
            | Operator::Xor
            | Operator::BitAnd => {
                self.binary(&loc, expr)?;
            }
        }
        Ok(())
    }

    fn call_refinement(
        &mut self,
        loc: ALoc,
        call: &flow_parser::ast::expression::Call<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;
        use flow_parser::ast::expression::Expression;
        use flow_parser::ast::expression::ExpressionInner;
        use flow_parser::ast::expression::ExpressionOrSpread;
        use flow_parser::ast::expression::member::Property as MemberProperty;

        if ast_utils::is_call_to_is_array(&call.callee)
            && call.arguments.arguments.len() == 1
            && matches!(
                &call.arguments.arguments[0],
                ExpressionOrSpread::Expression(_)
            )
        {
            let arg = match &call.arguments.arguments[0] {
                ExpressionOrSpread::Expression(e) => e,
                _ => unreachable!(),
            };

            let refi = match refinement_key::RefinementKey::of_expression(arg) {
                None => BTreeMap::new(),
                Some(refinement_key) => {
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    self.start_refinement(&refinement_key, refining_locs, RefinementKind::IsArrayR)
                }
            };

            self.expression(&call.callee)?;
            self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
                OptionalChainingRefinement::CanApplyPropTruthyRefi,
                arg,
            )?;
            self.commit_refinement(refi);
            return Ok(());
        }

        if !ast_utils::is_call_to_invariant(&call.callee) {
            let callee = &call.callee;
            let arguments = &call.arguments;
            let targs = &call.targs;

            // This case handles predicate functions. We ensure that this
            // is not a call to invariant.
            // The only other criterion that must be met for this call to produce
            // a refinement is that the arguments cannot contain a spread.
            //
            // Assuming there are no spreads we create a mapping from each argument
            // index to the refinement key at that index.
            //
            // The semantics for passing the same argument multiple times to predicate
            // function are sketchy. Pre-LTI Flow allows you to do this but it is buggy. See
            // https://fburl.com/vf52s7rb on v0.155.0
            //
            // We should strongly consider disallowing the same refinement key to
            // appear multiple times in the arguments. *)
            let arglist = &arguments.arguments;
            let has_spread = arglist
                .iter()
                .any(|arg| matches!(arg, ExpressionOrSpread::Spread(_)));
            let refinement_keys: Vec<Option<refinement_key::RefinementKey<ALoc>>> = if has_spread {
                Vec::new()
            } else {
                arglist
                    .iter()
                    .map(refinement_key::RefinementKey::of_argument)
                    .collect()
            };
            self.expression(callee)?;
            if let Some(targs) = targs {
                self.call_type_args(targs)?;
            }
            self.arg_list(arguments)?;
            self.havoc_current_env(
                flow_common::refinement_invalidation::Reason::FunctionCall,
                loc.dupe(),
            );

            let mut refis: BTreeMap<Lookup, (ALoc, env_api::refi::Refinement<ALoc>)> =
                BTreeMap::new();

            // Paremeter type-guard refinements *)
            // Function calls may introduce refinements if the function called is a
            // type-guard function. The EnvBuilder has no idea if a function is a
            // type-guard function or not. To handle that, we encode that a variable
            // _might_ be havoced by a function call if that variable is passed
            // as an argument. Variables not passed into the function are havoced if
            // the invalidation api says they can be invalidated.
            let callee_loc = callee.loc().dupe();
            let call_exp = Expression::new(ExpressionInner::Call {
                loc: loc.dupe(),
                inner: Arc::new(call.clone()),
            });
            let mut key_map: BTreeMap<Lookup, (refinement_key::RefinementKey<ALoc>, Vec<i32>)> =
                BTreeMap::new();
            for (index, key_opt) in refinement_keys.iter().enumerate() {
                if let Some(key) = key_opt {
                    let entry = key_map
                        .entry(key.lookup.dupe())
                        .or_insert_with(|| (key.dupe(), Vec::new()));
                    // Handles cases like `if (foo(x, x)) {}`
                    entry.1.push(index as i32);
                }
            }

            if !key_map.is_empty() {
                self.add_pred_func_info(
                    callee_loc.dupe(),
                    call_exp.clone(),
                    callee.clone(),
                    targs.clone(),
                    arguments.clone(),
                );
                for (key, indices) in key_map.values() {
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    refis = self.extend_refinement(
                        key,
                        refining_locs,
                        RefinementKind::LatentR {
                            func: Rc::new(callee.clone()),
                            targs: targs.clone().map(Rc::new),
                            arguments: Rc::new(arguments.clone()),
                            index: Rc::from(indices.as_slice()),
                        },
                        refis,
                    );
                }
            }

            if let ExpressionInner::Member {
                inner,
                loc: member_loc,
                ..
            } = callee.deref()
            {
                if let MemberProperty::PropertyIdentifier(_) = &inner.property {
                    if let Some(key) = refinement_key::RefinementKey::of_expression(&inner.object) {
                        if !key_map.contains_key(&key.lookup) {
                            // TODO For now in `x.f(x)` we do not consider the this-refinement on x.
                            // This is not fundamentally impossible, but causes crashes. *)
                            let call_exp = Expression::new(ExpressionInner::Call {
                                loc: loc.dupe(),
                                inner: Arc::new(call.clone()),
                            });
                            self.add_pred_func_info(
                                member_loc.dupe(),
                                call_exp,
                                callee.clone(),
                                targs.clone(),
                                arguments.clone(),
                            );
                            let mut refining_locs = empty_refining_locs();
                            refining_locs.insert(loc.dupe());
                            refis = self.extend_refinement(
                                &key,
                                refining_locs,
                                RefinementKind::LatentThisR {
                                    func: Rc::new(callee.clone()),
                                    targs: targs.clone().map(Rc::new),
                                    arguments: Rc::new(arguments.clone()),
                                },
                                refis,
                            );
                        }
                    }
                }
            }

            if !refis.is_empty() {
                self.commit_refinement(refis);
            }
            return Ok(());
        }

        self.call(&loc, call)
    }

    fn unary_refinement(
        &mut self,
        loc: &ALoc,
        unary: &flow_parser::ast::expression::Unary<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::expression::UnaryOperator;

        match unary.operator {
            UnaryOperator::Not => {
                self.push_refinement_scope(RefinementMaps::empty());
                self.expression_refinement(&unary.argument)?;
                self.negate_new_refinements();
                let negated_refinements = self.peek_new_refinements();
                self.pop_refinement_scope();
                self.merge_self_refinement_scope(&negated_refinements);
            }
            _ => {
                self.unary_expression(loc, unary)?;
            }
        }
        Ok(())
    }

    fn record_member_read(&mut self, expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>) {
        use flow_parser::ast::expression::ExpressionInner;

        let loc = expr.loc().dupe();
        match expr.deref() {
            ExpressionInner::OptionalMember { .. } | ExpressionInner::Member { .. } => {
                match self.get_val_of_expression_with_invalidated_refinement_snapshot(expr) {
                    None => {
                        // In some cases, we may re-visit the same expression multiple times via different
                        // environments--for example, visiting the discriminant of a switch statement. In
                        // these cases, it's possible that there was a val for an expression in a previous
                        // environment which is no longer available when seen through a subsequent. In order
                        // to prevent old environment values from "leaking" through, we need to actively remove
                        // values that may have previously existed but no longer do.
                        self.env_state.values.remove(&loc);
                    }
                    Some(Err(invalidation_info)) => {
                        self.env_state.values.remove(&loc);
                        self.env_state
                            .refinement_invalidation_info
                            .insert(loc, invalidation_info);
                    }
                    Some(Ok(refined_v)) => {
                        // We model a heap refinement as a separate const binding. We prefer this over using
                        // None so that we can report errors when using this value in a type position
                        self.env_state.values.insert(
                            loc,
                            ReadEntry {
                                def_loc: None,
                                value: refined_v,
                                val_binding_kind: ssa_val::ValBindingKind::SourceLevelBinding(
                                    BindingsKind::Const,
                                ),
                                name: None,
                            },
                        );
                    }
                }
            }
            _ => {}
        }
    }

    fn recursively_record_member_read(
        &mut self,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) {
        use flow_parser::ast::expression::ExpressionInner;

        match expr.deref() {
            ExpressionInner::OptionalMember { inner, .. } => {
                self.recursively_record_member_read(&inner.member.object);
            }
            ExpressionInner::Member { inner, .. } => {
                self.recursively_record_member_read(&inner.object);
            }
            _ => {}
        }
        self.record_member_read(expr);
    }

    fn optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
        &mut self,
        can_refine_obj_prop: OptionalChainingRefinement,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::expression::ExpressionInner;

        let loc = expr.loc().dupe();
        self.record_member_read(expr);
        match expr.deref() {
            ExpressionInner::OptionalMember { .. } => {
                self.member_expression_refinement_must_be_able_to_refine_base_object_to_non_maybe(
                    can_refine_obj_prop,
                    loc,
                    expr,
                    BTreeMap::new(),
                )?;
            }
            ExpressionInner::OptionalCall { inner, .. } => {
                let callee = &inner.call.callee;
                let targs = &inner.call.targs;
                let arguments = &inner.call.arguments;

                // TODO: Currently, optional call foo?.(...) is not modeled as NotR (MaybeR) on foo and
                // then call foo(...) at all. Before MethodT is un-entangled, let's not add the
                // non-maybe refinement on callee for now.
                let refi: BTreeMap<Lookup, (ALoc, Refinement<ALoc>)> = BTreeMap::new();

                self.push_refinement_scope(empty_refinements());
                // The refinement made here is local to the call expression and won't escape.
                // We do need to refine. Consider x?.(x), the x in the args position should read
                // a refined value, because otherwise the function won't be called and the
                // arguments won't be evaluated.
                self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
                    OptionalChainingRefinement::CanApplyPropNonNullishRefi,
                    callee,
                )?;
                self.commit_refinement(refi);
                if let Some(targs) = targs {
                    self.call_type_args(targs)?;
                }
                self.arg_list(arguments)?;
                self.pop_refinement_scope();
                self.havoc_current_env(
                    flow_common::refinement_invalidation::Reason::FunctionCall,
                    loc,
                );
            }
            ExpressionInner::Member {
                inner,
                loc: member_loc,
                ..
            } => {
                self.member(member_loc, inner)?;
            }
            ExpressionInner::Call {
                inner,
                loc: call_loc,
                ..
            } => {
                self.call(call_loc, inner)?;
            }
            _ => {
                self.expression(expr)?;
            }
        }
        Ok(())
    }

    fn member_expression_refinement_must_be_able_to_refine_base_object_to_non_maybe(
        &mut self,
        can_refine_obj_prop: OptionalChainingRefinement,
        loc: ALoc,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
        refis: BTreeMap<Lookup, (ALoc, Refinement<ALoc>)>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;
        use flow_parser::ast::expression::ExpressionInner;
        use flow_parser::ast::expression::OptionalMemberKind;
        use flow_parser::ast::expression::member::Property as MemberProperty;

        let optional = match expr.deref() {
            ExpressionInner::OptionalMember { inner, .. } => matches!(
                inner.optional,
                OptionalMemberKind::AssertNonnull | OptionalMemberKind::Optional
            ),
            _ => false,
        };

        let (object, property) = match expr.deref() {
            ExpressionInner::OptionalMember { inner, .. } => {
                (&inner.member.object, &inner.member.property)
            }
            ExpressionInner::Member { inner, .. } => (&inner.object, &inner.property),
            // | _ -> raise Env_api.(Env_invariant (Some loc, Impossible "member_expression_refinement can only be called on OptionalMember or Member"))
            _ => {
                panic!(
                    "member_expression_refinement can only be called on OptionalMember or Member"
                );
            }
        };

        self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
            OptionalChainingRefinement::CanApplyPropNonNullishRefi,
            object,
        )?;

        let propname: Option<FlowSmolStr> = match property {
            MemberProperty::PropertyIdentifier(id) => Some(id.name.dupe()),
            MemberProperty::PropertyExpression(prop_expr) => match prop_expr.deref() {
                ExpressionInner::StringLiteral { inner, .. } => Some(inner.value.dupe()),
                ExpressionInner::NumberLiteral { inner, .. } => {
                    if flow_common::js_number::is_float_safe_integer(inner.value) {
                        Some(FlowSmolStr::new(
                            flow_common::js_number::ecma_string_of_float(inner.value),
                        ))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            MemberProperty::PropertyPrivateName(_) => None,
        };

        let refinement_key_opt = refinement_key::RefinementKey::of_expression(object);

        match refinement_key_opt {
            None => {
                self.member_property(property)?;
            }
            Some(refinement_key) => {
                if optional {
                    let mut refining_locs = empty_refining_locs();
                    refining_locs.insert(loc.dupe());
                    self.add_single_refinement(
                        &refinement_key,
                        refining_locs,
                        RefinementKind::NotR(Rc::new(RefinementKind::MaybeR)),
                    );
                }
                self.member_property(property)?;

                let mut refis = refis;
                match can_refine_obj_prop {
                    OptionalChainingRefinement::CanApplyPropTruthyRefi => {
                        if let Some(propname) = propname {
                            let mut refining_locs = empty_refining_locs();
                            refining_locs.insert(loc.dupe());
                            refis = self.extend_refinement(
                                &refinement_key,
                                refining_locs,
                                RefinementKind::PropTruthyR {
                                    propname,
                                    loc: loc.dupe(),
                                },
                                refis,
                            );
                        }
                    }
                    OptionalChainingRefinement::CanApplyPropNonNullishRefi => {
                        if let Some(propname) = propname {
                            let mut refining_locs = empty_refining_locs();
                            refining_locs.insert(loc.dupe());
                            refis = self.extend_refinement(
                                &refinement_key,
                                refining_locs,
                                RefinementKind::NotR(Rc::new(RefinementKind::PropNullishR {
                                    propname,
                                    loc: loc.dupe(),
                                })),
                                refis,
                            );
                        }
                    }
                    OptionalChainingRefinement::CanApplyPropNonVoidRefi => {
                        if let Some(propname) = propname {
                            let mut refining_locs = empty_refining_locs();
                            refining_locs.insert(loc.dupe());
                            refis = self.extend_refinement(
                                &refinement_key,
                                refining_locs,
                                RefinementKind::PropNonVoidR {
                                    propname,
                                    loc: loc.dupe(),
                                },
                                refis,
                            );
                        }
                    }
                    OptionalChainingRefinement::CanApplyPropIsExactlyNullRefi => {
                        if let Some(propname) = propname {
                            let mut refining_locs = empty_refining_locs();
                            refining_locs.insert(loc.dupe());
                            refis = self.extend_refinement(
                                &refinement_key,
                                refining_locs,
                                RefinementKind::PropIsExactlyNullR {
                                    propname,
                                    loc: loc.dupe(),
                                },
                                refis,
                            );
                        }
                    }
                }

                self.commit_refinement(refis);
            }
        }
        Ok(())
    }

    fn expression_refinement(
        &mut self,
        expression: &Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.handle_array_providers(expression);
        let loc = expression.loc().dupe();
        match expression.deref() {
            ExpressionInner::Identifier { inner, loc, .. } => {
                self.identifier_refinement(loc, inner.name.dupe());
            }
            ExpressionInner::Logical { inner, .. } => {
                self.logical_refinement(inner.operator, &inner.left, &inner.right)?;
            }
            ExpressionInner::Assignment { inner, loc, .. } => {
                self.assignment_refinement(loc.dupe(), inner)?;
            }
            ExpressionInner::Binary { inner, loc, .. } => {
                self.binary_refinement(loc.dupe(), inner)?;
            }
            ExpressionInner::Call { inner, loc, .. } => {
                self.call_refinement(loc.dupe(), inner)?;
            }
            ExpressionInner::Unary { inner, loc, .. } => {
                self.unary_refinement(loc, inner)?;
            }
            ExpressionInner::Member { .. } | ExpressionInner::OptionalMember { .. } => {
                // Add a PropTruthy refinement to the object and a
                // Truthy heap refinement to the access
                let refis = match refinement_key::RefinementKey::of_expression(expression) {
                    None => BTreeMap::new(),
                    Some(key) => {
                        let mut refining_locs = empty_refining_locs();
                        refining_locs.insert(loc.dupe());
                        self.start_refinement(&key, refining_locs, env_api::RefinementKind::TruthyR)
                    }
                };
                self.record_member_read(expression);
                // Over here, optional chaining appears directly at the truthy refinement position,
                // so the following holds
                self.member_expression_refinement_must_be_able_to_refine_base_object_to_non_maybe(
                    OptionalChainingRefinement::CanApplyPropTruthyRefi,
                    loc,
                    expression,
                    refis,
                )?;
            }
            ExpressionInner::Array { .. }
            | ExpressionInner::ArrowFunction { .. }
            | ExpressionInner::AsConstExpression { .. }
            | ExpressionInner::AsExpression { .. }
            | ExpressionInner::Class { .. }
            | ExpressionInner::Conditional { .. }
            | ExpressionInner::Function { .. }
            | ExpressionInner::Import { .. }
            | ExpressionInner::JSXElement { .. }
            | ExpressionInner::JSXFragment { .. }
            | ExpressionInner::StringLiteral { .. }
            | ExpressionInner::NumberLiteral { .. }
            | ExpressionInner::BooleanLiteral { .. }
            | ExpressionInner::NullLiteral { .. }
            | ExpressionInner::RegExpLiteral { .. }
            | ExpressionInner::BigIntLiteral { .. }
            | ExpressionInner::Match { .. }
            | ExpressionInner::ModuleRefLiteral { .. }
            | ExpressionInner::MetaProperty { .. }
            | ExpressionInner::New { .. }
            | ExpressionInner::Object { .. }
            | ExpressionInner::OptionalCall { .. }
            | ExpressionInner::Record { .. }
            | ExpressionInner::Sequence { .. }
            | ExpressionInner::Super { .. }
            | ExpressionInner::TaggedTemplate { .. }
            | ExpressionInner::TemplateLiteral { .. }
            | ExpressionInner::TypeCast { .. }
            | ExpressionInner::TSSatisfies { .. }
            | ExpressionInner::This { .. }
            | ExpressionInner::Update { .. }
            | ExpressionInner::Yield { .. } => {
                self.expression(expression)?;
            }
        }
        Ok(())
    }

    fn chain_to_refinement(&self, chain: &RefinementChain) -> Refinement<ALoc> {
        match chain {
            RefinementChain::Base(refinement) => refinement.dupe(),
            RefinementChain::And(id1, id2) => {
                let chain1 = self
                    .env_state
                    .refinement_heap
                    .borrow()
                    .get(&RefinementId(*id1))
                    .unwrap()
                    .dupe();
                let Refinement {
                    refining_locs: locs1,
                    kind: ref1,
                } = self.chain_to_refinement(&chain1);
                let chain2 = self
                    .env_state
                    .refinement_heap
                    .borrow()
                    .get(&RefinementId(*id2))
                    .unwrap()
                    .dupe();
                let Refinement {
                    refining_locs: locs2,
                    kind: ref2,
                } = self.chain_to_refinement(&chain2);
                let mut refining_locs = locs1;
                refining_locs.extend(locs2);
                Refinement {
                    refining_locs,
                    kind: env_api::RefinementKind::AndR(Rc::new(ref1), Rc::new(ref2)),
                }
            }
            RefinementChain::Or(id1, id2) => {
                let chain1 = self
                    .env_state
                    .refinement_heap
                    .borrow()
                    .get(&RefinementId(*id1))
                    .unwrap()
                    .dupe();
                let Refinement {
                    refining_locs: locs1,
                    kind: ref1,
                } = self.chain_to_refinement(&chain1);
                let chain2 = self
                    .env_state
                    .refinement_heap
                    .borrow()
                    .get(&RefinementId(*id2))
                    .unwrap()
                    .dupe();
                let Refinement {
                    refining_locs: locs2,
                    kind: ref2,
                } = self.chain_to_refinement(&chain2);
                let mut refining_locs = locs1;
                refining_locs.extend(locs2);
                Refinement {
                    refining_locs,
                    kind: env_api::RefinementKind::OrR(Rc::new(ref1), Rc::new(ref2)),
                }
            }
            RefinementChain::Not(id) => {
                let inner_chain = self
                    .env_state
                    .refinement_heap
                    .borrow()
                    .get(&RefinementId(*id))
                    .unwrap()
                    .dupe();
                let Refinement {
                    refining_locs: locs,
                    kind: ref_kind,
                } = self.chain_to_refinement(&inner_chain);
                Refinement {
                    refining_locs: locs,
                    kind: env_api::RefinementKind::NotR(Rc::new(ref_kind)),
                }
            }
        }
    }

    fn refinement_of_id(&self, id: RefinementId) -> Refinement<ALoc> {
        let chain = self
            .env_state
            .refinement_heap
            .borrow()
            .get(&id)
            .unwrap()
            .dupe();
        self.chain_to_refinement(&chain)
    }

    fn handle_array_providers(
        &mut self,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) {
        use env_api::DefLocType;
        use env_api::EnvEntry;
        use env_api::EnvKey;
        use flow_common::reason::mk_expression_reason;

        let loc = expr.loc();
        if self.provider_info.is_array_provider(loc) {
            let reason = mk_expression_reason(expr);
            let key = EnvKey::new(DefLocType::ArrayProviderLoc, loc.dupe());
            self.env_state
                .write_entries
                .insert(key, EnvEntry::AssigningWrite(reason));
        }
    }

    fn hoist_annotations<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let visiting_hoisted_type = self.env_state.visiting_hoisted_type;
        self.env_state.visiting_hoisted_type = true;
        let result = f(self);
        self.env_state.visiting_hoisted_type = visiting_hoisted_type;
        result
    }

    fn jsx_function_call(&mut self, loc: ALoc) -> Result<(), AbruptCompletion> {
        use flow_common::options::JsxMode;
        use flow_common::options::ReactRuntime;
        let jsx_base_name = self.env_state.jsx_base_name.dupe();
        match (self.cx.react_runtime(), jsx_base_name, self.cx.jsx()) {
            (ReactRuntime::Classic, Some(name), JsxMode::JsxReact) => {
                self.any_identifier(loc, &name);
            }
            (_, Some(name), JsxMode::JsxPragma(_, _)) => {
                self.any_identifier(loc, &name);
            }
            (ReactRuntime::Classic, None, JsxMode::JsxPragma(_, ref ast)) => {
                self.expression(ast)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn statements_with_bindings(
        &mut self,
        loc: ALoc,
        bindings: flow_analysis::bindings::Bindings<ALoc>,
        statements: &[flow_parser::ast::statement::Statement<ALoc, ALoc>],
    ) -> Option<AbruptCompletion> {
        self.with_bindings(true, loc, bindings, |resolver| {
            let completion = resolver.run_to_completion(|r| r.statement_list(statements));
            Ok(completion)
        })
        .unwrap_or_default()
    }

    fn synthesize_read(&self, name: &FlowSmolStr) -> env_api::Read<ALoc> {
        let env_val = self.env_read(name);
        let v = if self.env_state.visiting_hoisted_type {
            env_val.havoc.dupe()
        } else {
            env_val.val_ref.borrow().dupe()
        };
        ssa_val::simplify(
            &mut *self.simplify_cache.borrow_mut(),
            env_val.def_loc.dupe(),
            ssa_val::ValBindingKind::SourceLevelBinding(env_val.kind),
            Some(name.dupe()),
            &v,
        )
    }

    fn visit_program(&mut self, program: &Program<ALoc, ALoc>) -> Option<AbruptCompletion> {
        let loc = program.loc.dupe();
        let statements = &program.statements;
        use flow_analysis::hoister::Hoister;
        use flow_parser::ast_visitor::AstVisitor;
        let mut hoist = Hoister::new(self.enable_enums, true);
        let Ok(()) = hoist.program(program);
        let bindings = hoist.into_bindings();
        self.statements_with_bindings(loc, bindings, statements)
    }
}

// =============================================================================
// AstVisitor implementation for NameResolver
// =============================================================================

impl<'ast, 'a, Cx: Context, Fl: Flow<Cx = Cx>>
    AstVisitor<'ast, ALoc, ALoc, &'ast ALoc, AbruptCompletion> for NameResolver<'a, Cx, Fl>
{
    fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
        loc
    }

    fn normalize_type(type_: &'ast ALoc) -> &'ast ALoc {
        type_
    }

    fn type_(
        &mut self,
        t: &flow_parser::ast::types::Type<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        match t.deref() {
            TypeInner::Infer { loc, inner } if !self.env_state.in_conditional_type_extends => {
                Fl::add_output(self.cx, ErrorMessage::EInvalidInfer(loc.dupe()));
                let name_loc = inner.tparam.name.loc.dupe();
                self.env_state.write_entries.insert(
                    env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, name_loc),
                    env_api::EnvEntry::NonAssigningWrite,
                );
                Ok(())
            }
            _ => ast_visitor::type_default(self, t),
        }
    }

    fn binding_type_identifier(
        &mut self,
        ident: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;
        use flow_typing_errors::intermediate_error_types::IncorrectType;

        let loc = ident.loc.dupe();
        let name = &ident.name;
        let env_val = self.env_read(name);
        let kind = env_val.kind;
        let def_loc = env_val.def_loc.dupe();
        let kind_at_loc = env_val.kind_at_loc.clone();
        let reserved_keyword_error = if let Ok(keyword) = name.as_str().parse::<IncorrectType>() {
            if keyword.is_type_reserved() {
                match kind {
                    BindingsKind::Type { .. } | BindingsKind::Interface { .. } => {
                        Some(ErrorMessage::EBindingError(Box::new((
                            BindingError::EReservedKeyword { keyword },
                            loc.dupe(),
                            flow_common::reason::Name::new(name.dupe()),
                            loc.dupe(),
                        ))))
                    }
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        };
        let error = match reserved_keyword_error {
            Some(error) => Some(error),
            None => match def_loc {
                // Identifiers with no binding can never reintroduce "cannot reassign binding" errors
                None => None,
                Some(ref def_loc_val) => match kind {
                    BindingsKind::Interface { .. }
                        if loc != *def_loc_val
                            && matches!(
                                kind_at_loc.get(&loc),
                                Some(BindingsKind::Interface { .. })
                            ) =>
                    {
                        // Both the existing binding and this declaration are interfaces:
                        // allow declaration merging. Record conflict for post-inference check
                        // and add AssigningWrite so name_def resolves this declaration.
                        let entry = self
                            .env_state
                            .interface_merge_conflicts
                            .entry(def_loc_val.dupe())
                            .or_insert_with(Vec::new);
                        entry.push(loc.dupe());
                        let reason = VirtualReason::new(
                            VirtualReasonDesc::RType(flow_common::reason::Name::new(name.dupe())),
                            loc.dupe(),
                        );
                        self.env_state.write_entries.insert(
                            env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, loc.dupe()),
                            env_api::EnvEntry::AssigningWrite(reason),
                        );
                        None
                    }
                    BindingsKind::Type { .. }
                    | BindingsKind::Interface { .. }
                    | BindingsKind::DeclaredClass
                    | BindingsKind::DeclaredVar
                    | BindingsKind::DeclaredLet
                    | BindingsKind::DeclaredConst
                        if loc != *def_loc_val =>
                    {
                        // Types are already bound in hoister,
                        // so we only check for rebind in different locations.
                        Some(ErrorMessage::EBindingError(Box::new((
                            BindingError::ENameAlreadyBound,
                            loc.dupe(),
                            flow_common::reason::Name::new(name.dupe()),
                            def_loc_val.dupe(),
                        ))))
                    }
                    BindingsKind::Type { .. } | BindingsKind::Interface { .. } => None,
                    BindingsKind::Var
                    | BindingsKind::Const
                    | BindingsKind::Let
                    | BindingsKind::Class
                    | BindingsKind::Enum
                    | BindingsKind::Record
                    | BindingsKind::Function
                    | BindingsKind::Component
                    | BindingsKind::Parameter
                    | BindingsKind::ComponentParameter
                    | BindingsKind::Import
                    | BindingsKind::TsImport => Some(ErrorMessage::EBindingError(Box::new((
                        BindingError::ENameAlreadyBound,
                        loc.dupe(),
                        flow_common::reason::Name::new(name.to_string()),
                        def_loc_val.dupe(),
                    )))),
                    _ => None,
                },
            },
        };
        if let Some(err) = error {
            let is_reserved_keyword = matches!(
                &err,
                ErrorMessage::EBindingError(box (BindingError::EReservedKeyword { .. }, _, _, _))
            );
            Fl::add_output(self.cx, err);
            if !is_reserved_keyword {
                self.env_state.write_entries.insert(
                    env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, loc.dupe()),
                    env_api::EnvEntry::NonAssigningWrite,
                );
            }
        }
        ast_visitor::identifier_default(self, ident)
    }

    fn function_identifier(
        &mut self,
        ident: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        // The parent flow_ast_mapper treats functions as Vars, but in Flow
        // (not JS, Flow) they have special behavior with functions.
        let loc = ident.loc.dupe();
        let x = &ident.name;
        self.bind_pattern_identifier_customized(
            PatternWriteKind::FunctionBinding,
            loc,
            x,
            ssa_val::one,
        );
        ast_visitor::identifier_default(self, ident)
    }

    fn component_identifier(
        &mut self,
        ident: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let loc = ident.loc.dupe();
        let x = &ident.name;
        self.bind_pattern_identifier_customized(
            PatternWriteKind::ComponentBinding,
            loc,
            x,
            ssa_val::one,
        );
        ast_visitor::identifier_default(self, ident)
    }

    // Override the object type constuctor to disable the EReferenceInAnnotation check
    // since this is a common and safe way to encode recursive object types.
    fn object_type(
        &mut self,
        ot: &flow_parser::ast::types::Object<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let old_val = std::mem::take(&mut self.env_state.current_bindings);
        let result = ast_visitor::object_type_default(self, ot);
        self.env_state.current_bindings = old_val;
        result
    }

    fn type_identifier_reference(
        &mut self,
        id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.error_on_reference_to_currently_declared_id(id);
        ast_visitor::type_identifier_reference_default(self, id)
    }

    fn typeof_identifier(
        &mut self,
        id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.error_on_reference_to_currently_declared_id(id);
        ast_visitor::typeof_identifier_default(self, id)
    }

    fn this_expression(
        &mut self,
        loc: &ALoc,
        _this: &flow_parser::ast::expression::This<ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.any_identifier(loc.dupe(), &FlowSmolStr::new_inline("this"));
        Ok(())
    }

    fn super_expression(
        &mut self,
        loc: &ALoc,
        _super: &flow_parser::ast::expression::Super<ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.any_identifier(loc.dupe(), &FlowSmolStr::new_inline("super"));
        Ok(())
    }

    fn identifier(
        &mut self,
        id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let loc = id.loc.dupe();
        let x = &id.name;
        if self.env_state.in_param_default {
            self.error_on_reference_to_currently_declared_id_in_default(id);
        }
        self.any_identifier(loc, x);
        ast_visitor::identifier_default(self, id)
    }

    fn untyped_identifier(
        &mut self,
        id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let loc = id.loc.dupe();
        let x = &id.name;
        self.any_identifier(loc, x);
        ast_visitor::untyped_identifier_default(self, id)
    }

    fn generic_identifier_type(
        &mut self,
        mut git: &flow_parser::ast::types::generic::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::types::generic::Identifier;

        loop {
            match git {
                Identifier::Unqualified(i) => {
                    self.type_identifier_reference(i)?;
                    return Ok(());
                }
                Identifier::Qualified(qual) => {
                    git = &qual.qualification;
                }
                Identifier::ImportTypeAnnot(_) => {
                    return Ok(());
                }
            }
        }
    }

    fn jsx_element_name_identifier(
        &mut self,
        ident: &flow_parser::ast::jsx::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.any_identifier(ident.loc.dupe(), &ident.name);
        Ok(())
    }

    fn jsx_element_name_namespaced(
        &mut self,
        _ns: &flow_parser::ast::jsx::NamespacedName<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        // TODO: what identifiers does `<foo:bar />` read?
        Ok(())
    }

    fn jsx_member_expression(
        &mut self,
        jsx_mem_expr: &flow_parser::ast::jsx::MemberExpression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::Identifier;
        use flow_parser::ast::expression;
        use flow_parser::ast::expression::member;
        use flow_parser::ast::jsx::member_expression;

        fn obj_to_expr(
            obj: &member_expression::Object<ALoc, ALoc>,
        ) -> expression::Expression<ALoc, ALoc> {
            match obj {
                member_expression::Object::Identifier(jsx_id) => {
                    let id_loc = jsx_id.loc.dupe();
                    expression::Expression::new(expression::ExpressionInner::Identifier {
                        loc: id_loc.dupe(),
                        inner: Identifier::new(IdentifierInner {
                            loc: id_loc,
                            name: jsx_id.name.dupe(),
                            comments: jsx_id.comments.dupe(),
                        }),
                    })
                }
                member_expression::Object::MemberExpression(mem_expr) => {
                    let loc = mem_expr.loc.dupe();
                    let prop_loc = mem_expr.property.loc.dupe();
                    expression::Expression::new(expression::ExpressionInner::Member {
                        loc: loc.dupe(),
                        inner: Arc::new(expression::Member {
                            object: obj_to_expr(&mem_expr.object),
                            property: member::Property::PropertyIdentifier(Identifier::new(
                                IdentifierInner {
                                    loc: prop_loc,
                                    name: mem_expr.property.name.dupe(),
                                    comments: mem_expr.property.comments.dupe(),
                                },
                            )),
                            comments: None,
                        }),
                    })
                }
            }
        }

        let wrapped_obj = member_expression::Object::MemberExpression(Arc::new(
            flow_parser::ast::jsx::MemberExpression {
                loc: jsx_mem_expr.loc.dupe(),
                object: jsx_mem_expr.object.clone(),
                property: jsx_mem_expr.property.clone(),
            },
        ));
        let expr = obj_to_expr(&wrapped_obj);
        self.expression(&expr)?;
        Ok(())
    }

    fn pattern(
        &mut self,
        kind: Option<flow_parser::ast::VariableKind>,
        pattern: &flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;

        let ploc = pattern.loc().dupe();
        if flow_parser::ast_utils::pattern_has_binding(pattern) {
            let reason = VirtualReason::new(VirtualReasonDesc::RDestructuring, ploc.dupe());
            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::PatternLoc, ploc),
                env_api::EnvEntry::AssigningWrite(reason),
            );
        }
        ast_visitor::pattern_default(self, kind, pattern)
    }

    fn pattern_identifier(
        &mut self,
        kind: Option<flow_parser::ast::VariableKind>,
        ident: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let loc = ident.loc.dupe();
        let x = &ident.name;
        let pattern_write_kind = variable_declaration_binding_kind_to_pattern_write_kind(kind);
        self.bind_pattern_identifier_customized(pattern_write_kind, loc, x, ssa_val::one);
        ast_visitor::identifier_default(self, ident)
    }

    fn pattern_array_element(
        &mut self,
        kind: Option<flow_parser::ast::VariableKind>,
        elem: &flow_parser::ast::pattern::array::NormalElement<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        // Flip order compared to base class
        if let Some(default) = &elem.default {
            self.expression(default)?;
        }
        self.pattern_array_element_pattern(kind, &elem.argument)?;
        Ok(())
    }

    // Override pattern_object_property to detect SELF-references in pattern-inline defaults.
    // For {a = a}, check if a's default references a itself (self-reference) - ERROR.
    // For {a, b = a}, b's default references a, not b - OK (not a self-reference).
    // For {a = b, b}, a's default references b - handled by reference-before-declaration.
    // We only check for self-references here; forward references are caught elsewhere.
    fn pattern_object_property(
        &mut self,
        kind: Option<flow_parser::ast::VariableKind>,
        prop: &flow_parser::ast::pattern::object::NormalProperty<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.pattern_object_property_key(kind, &prop.key)?;
        self.pattern_object_property_pattern(kind, &prop.pattern)?;
        if let Some(default_expr) = &prop.default {
            self.visit_default_with_pattern_bindings(&prop.pattern, default_expr)?;
        }
        Ok(())
    }

    fn function_param(
        &mut self,
        param: &flow_parser::ast::function::Param<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let (argument, default) = match param {
            flow_parser::ast::function::Param::RegularParam {
                loc: _,
                argument,
                default,
            } => (argument, default),
            flow_parser::ast::function::Param::ParamProperty { .. } => {
                // Skip parameter properties, they are not supported
                return Ok(());
            }
        };
        self.visit_function_or_component_param_pattern(false, argument.loc().dupe(), argument);
        ast_visitor::function_param_pattern_default(self, argument)?;
        if let Some(default_expr) = default {
            self.visit_default_with_pattern_bindings(argument, default_expr)?;
        }
        Ok(())
    }

    // Order of evaluation matters
    fn assignment(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::expression::Assignment<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;
        use flow_common::reason::mk_expression_reason;
        use flow_parser::ast::expression::AssignmentOperator as Operator;
        use flow_parser::ast::expression::Expression;
        use flow_parser::ast::expression::ExpressionInner;
        use flow_parser::ast::expression::member::Property as MemberProperty;
        use flow_parser::ast::pattern::Pattern;

        let assign_loc = loc.dupe();
        let operator = &expr.operator;
        let right = &expr.right;
        let (left, _) = ast_utils::unwrap_nonnull_lhs(&expr.left);
        let left_loc = left.loc().dupe();
        if let Pattern::Expression {
            inner: member_expr, ..
        } = &*left
        {
            if let ExpressionInner::Member { inner: member, .. } = &***member_expr {
                if let MemberProperty::PropertyExpression(prop_expr) = &member.property {
                    let prop_loc = prop_expr.loc().dupe();
                    let reason = mk_expression_reason(prop_expr);
                    self.env_state.write_entries.insert(
                        env_api::EnvKey::new(env_api::DefLocType::ExpressionLoc, prop_loc),
                        env_api::EnvEntry::AssigningWrite(reason),
                    );
                }
            }
        }

        match operator {
            None => match &*left {
                Pattern::Identifier { .. } | Pattern::Object { .. } | Pattern::Array { .. } => {
                    // given `x = e`, read e then write x
                    self.expression(right)?;
                    self.binding_pattern_track_object_destructuring(None, right.clone(), &left)?;
                }
                Pattern::Expression { inner: e, .. } => {
                    // given `o.x = e`, read o then read e
                    self.assign_expression(assign_loc, e, right)?;
                }
            },
            Some(
                Operator::PlusAssign
                | Operator::MinusAssign
                | Operator::MultAssign
                | Operator::ExpAssign
                | Operator::DivAssign
                | Operator::ModAssign
                | Operator::LShiftAssign
                | Operator::RShiftAssign
                | Operator::RShift3Assign
                | Operator::BitOrAssign
                | Operator::BitXorAssign
                | Operator::BitAndAssign,
            ) => {
                match &*left {
                    Pattern::Identifier { inner, .. } => {
                        // given `x += e`, read x then read e then write x
                        self.identifier(&inner.name)?;
                        self.expression(right)?;
                        self.assignment_pattern(&left)?;
                    }
                    Pattern::Expression { inner: e, .. } => {
                        // given `o.x += e`, read o then read e
                        self.pattern_expression(e)?;
                        self.assign_expression(assign_loc, e, right)?;
                    }
                    Pattern::Object { .. } | Pattern::Array { .. } => {}
                }
            }
            Some(op @ (Operator::OrAssign | Operator::AndAssign | Operator::NullishAssign)) => {
                let left_expr: Option<Expression<ALoc, ALoc>> = match &*left {
                    Pattern::Identifier {
                        loc: lhs_loc,
                        inner,
                    } => Some(Expression::new(ExpressionInner::Identifier {
                        loc: lhs_loc.dupe(),
                        inner: inner.name.dupe(),
                    })),
                    Pattern::Expression {
                        loc: lhs_loc,
                        inner,
                    } => {
                        if let ExpressionInner::Member { inner: mem, .. } = &***inner {
                            Some(Expression::new(ExpressionInner::Member {
                                loc: lhs_loc.dupe(),
                                inner: mem.clone(),
                            }))
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                self.push_refinement_scope(empty_refinements());

                match &left_expr {
                    None => {}
                    Some(left_expr) => match op {
                        Operator::OrAssign | Operator::AndAssign => {
                            self.expression_refinement(left_expr)?;
                        }
                        Operator::NullishAssign => {
                            self.expression(left_expr)?;
                            let mut refining_locs = empty_refining_locs();
                            refining_locs.insert(left_loc.dupe());
                            self.add_refinement_to_expr(
                                left_expr,
                                refining_locs,
                                RefinementKind::NotR(Rc::new(RefinementKind::MaybeR)),
                            );
                        }
                        _ => {}
                    },
                }

                let env1 = self.env_snapshot_without_latest_refinements();
                let env1_with_refinements = self.env_snapshot();
                match op {
                    Operator::NullishAssign | Operator::OrAssign => {
                        self.negate_new_refinements();
                    }
                    _ => {}
                }

                // The RHS is _only_ evaluated if the LHS fails its check. That means that patterns like
                // x || invariant(false) should propagate the truthy refinement to the next line. We keep track
                // of the completion state on the rhs to do that. If the LHS throws then the entire expression
                // throws, so there's no need to catch the exception from the LHS
                let rhs_completion_state = self.run_to_completion(|this| this.expression(right));
                match rhs_completion_state {
                    Some(AbruptCompletion::Throw) => {
                        self.reset_env(&env1_with_refinements);
                        self.pop_refinement_scope_without_unrefining();
                    }
                    _ => {
                        self.pop_refinement_scope();
                        self.merge_self_env(&env1);
                    }
                }
                match &*left {
                    Pattern::Expression { loc, inner } => {
                        if let ExpressionInner::Member { inner: mem, .. } = &***inner {
                            let reason = flow_common::reason::VirtualReason::new(
                                flow_common::reason::VirtualReasonDesc::RSomeProperty,
                                loc.dupe(),
                            );
                            let assigned_val =
                                ssa_val::one(&mut *self.cache.borrow_mut(), reason.dupe());
                            self.assign_member(
                                false,
                                mem,
                                assign_loc,
                                loc.dupe(),
                                assigned_val,
                                reason,
                            );
                        }
                    }
                    Pattern::Identifier { .. } => {
                        self.assignment_pattern(&left)?;
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn variable_declaration(
        &mut self,
        loc: &ALoc,
        decl: &flow_parser::ast::statement::VariableDeclaration<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let declarations = &decl.declarations;
        let kind = decl.kind;

        if kind != flow_parser::ast::VariableKind::Const {
            for declarator in declarations.iter() {
                flow_parser::ast_utils::fold_bindings_of_pattern(
                    (),
                    &declarator.id,
                    &mut |_, id| {
                        self.valid_declaration_check(&id.name, &id.loc);
                    },
                );
            }
        }
        ast_visitor::variable_declaration_default(self, loc, decl)
    }

    // Order of evaluation matters
    fn variable_declarator(
        &mut self,
        kind: flow_parser::ast::VariableKind,
        decl: &flow_parser::ast::statement::variable::Declarator<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::Name;
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;
        use flow_parser::ast::pattern::Pattern;
        use flow_parser::ast::types::AnnotationOrHint;

        let id = &decl.id;
        let init = &decl.init;

        let annot = match id {
            Pattern::Identifier { inner, .. } => &inner.annot,
            Pattern::Object { inner, .. } => &inner.annot,
            Pattern::Array { inner, .. } => &inner.annot,
            _ => {
                return Ok(());
            }
        };

        // Special case: empty array literal with identifier pattern and missing annotation
        if let (
            Some(init_expr),
            Pattern::Identifier {
                inner: id_inner,
                loc: _,
            },
        ) = (init, id)
        {
            if let ExpressionInner::Array { inner: arr, .. } = &**init_expr {
                if arr.elements.is_empty() && matches!(id_inner.annot, AnnotationOrHint::Missing(_))
                {
                    let name_loc = id_inner.name.loc.dupe();
                    let x = &id_inner.name.name;
                    let write_kind =
                        variable_declaration_binding_kind_to_pattern_write_kind(Some(kind));
                    let reason = VirtualReason::new(
                        VirtualReasonDesc::RIdentifier(Name::new(x.dupe())),
                        name_loc.dupe(),
                    );
                    let write_entry = env_api::EnvEntry::AssigningWrite(reason.dupe());
                    let assigned_val = match self.provider_info.providers_of_def(&name_loc) {
                        Some(def_providers) => ssa_val::empty_array(
                            &mut *self.cache.borrow_mut(),
                            reason.dupe(),
                            def_providers.array_providers.dupe(),
                        ),
                        None => ssa_val::one(&mut *self.cache.borrow_mut(), reason.dupe()),
                    };

                    let env_entry = self.env_read(x);
                    let val_ref = env_entry.val_ref.dupe();
                    let heap_refinements = env_entry.heap_refinements.dupe();
                    let stored_binding_kind = env_entry.kind;
                    let def_loc = env_entry.def_loc.dupe();

                    let val_snapshot = val_ref.borrow().dupe();
                    match error_for_assignment_kind(
                        x,
                        name_loc.dupe(),
                        def_loc,
                        stored_binding_kind,
                        write_kind,
                        &val_snapshot,
                        self.enable_const_params,
                    ) {
                        Some(err) => {
                            Fl::add_output(self.cx, err);
                            self.env_state.write_entries.insert(
                                env_api::EnvKey::new(
                                    env_api::DefLocType::OrdinaryNameLoc,
                                    name_loc.dupe(),
                                ),
                                env_api::EnvEntry::NonAssigningWrite,
                            );
                        }
                        None => {
                            self.record_pattern_loc_writes(id);
                            Self::havoc_heap_refinements(&heap_refinements);
                            if !ssa_val::is_declared_function(&val_ref.borrow()) {
                                if !self.is_excluded_ordinary_name(x) {
                                    *val_ref.borrow_mut() = assigned_val;
                                }
                                self.env_state.write_entries.insert(
                                    env_api::EnvKey::new(
                                        env_api::DefLocType::OrdinaryNameLoc,
                                        name_loc.dupe(),
                                    ),
                                    write_entry,
                                );
                            } else {
                                self.env_state
                                    .write_entries
                                    .entry(env_api::EnvKey::new(
                                        env_api::DefLocType::OrdinaryNameLoc,
                                        name_loc.dupe(),
                                    ))
                                    .or_insert(env_api::EnvEntry::NonAssigningWrite);
                            }
                        }
                    }
                    return Ok(());
                }
            }
        }

        match (init, id) {
            (Some(init_expr), _) => {
                // given `var x = e`, read e then write x
                self.expression(init_expr)?;
                self.binding_pattern_track_object_destructuring(Some(kind), init_expr.clone(), id)?;
            }
            (None, _) => {
                // No rhs means no write occurs, but the variable moves from undeclared to uninitialized.
                self.record_pattern_loc_writes(id);
                flow_parser::ast_utils::fold_bindings_of_pattern((), id, &mut |(), identifier| {
                    let loc = identifier.loc.dupe();
                    let name = &identifier.name;

                    let env_val = self.env_read(name);
                    let val_ref = env_val.val_ref.dupe();
                    let stored_binding_kind = env_val.kind;
                    let def_loc = env_val.def_loc.dupe();
                    let write_kind =
                        variable_declaration_binding_kind_to_pattern_write_kind(Some(kind));
                    let error = error_for_assignment_kind(
                        name,
                        loc.dupe(),
                        def_loc.dupe(),
                        stored_binding_kind,
                        write_kind,
                        &val_ref.borrow(),
                        self.enable_const_params,
                    );
                    if ssa_val::is_undeclared(&val_ref.borrow())
                        && !self.is_excluded_ordinary_name(name)
                    {
                        *val_ref.borrow_mut() =
                            ssa_val::uninitialized(&mut *self.cache.borrow_mut(), loc.dupe());
                    }
                    let reason = VirtualReason::new(
                        VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                        loc.dupe(),
                    );

                    match (error, annot) {
                        (None, flow_parser::ast::types::AnnotationOrHint::Available(_)) => {
                            self.env_state.write_entries.insert(
                                env_api::EnvKey::new(env_api::DefLocType::OrdinaryNameLoc, loc),
                                env_api::EnvEntry::AssigningWrite(reason),
                            );
                        }
                        (Some(err), _) => {
                            self.error_assignment(
                                loc,
                                name,
                                reason,
                                stored_binding_kind,
                                write_kind,
                                err,
                                &val_ref,
                            );
                        }
                        _ => {}
                    }
                });

                self.with_current_pattern_bindings(id, |this| this.type_annotation_hint(annot))?;
            }
        }

        Ok(())
    }

    fn declare_variable(
        &mut self,
        _loc: &ALoc,
        decl: &flow_parser::ast::statement::DeclareVariable<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let kind = decl.kind;
        for declarator in decl.declarations.iter() {
            if let flow_parser::ast::pattern::Pattern::Identifier { inner, .. } = &declarator.id {
                let ident = &inner.name;
                self.pattern_identifier(Some(kind), ident)?;
                self.hoist_annotations(|this| {
                    this.with_current_id_binding(ident, |this| {
                        if let flow_parser::ast::types::AnnotationOrHint::Available(annot) =
                            &inner.annot
                        {
                            this.type_annotation(annot)?;
                        }
                        if let Some(init) = &declarator.init {
                            this.expression(init)?;
                        }
                        Ok(())
                    })
                })?;
            }
        }
        Ok(())
    }

    fn update_expression(
        &mut self,
        _loc: &ALoc,
        expr: &flow_parser::ast::expression::Update<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::expression::ExpressionInner;

        let argument = &expr.argument;
        match argument.deref() {
            ExpressionInner::Identifier { inner: x, .. } => {
                // given `x++`, read x then write x
                self.identifier(x)?;
                self.pattern_identifier(None, x)?;
            }
            ExpressionInner::Member { loc, inner: member } => {
                // given `o.x++`, read o.x then write o.x
                self.expression(argument)?;
                self.pattern_expression(argument)?;
                let val_reason = flow_common::reason::VirtualReason::new(
                    flow_common::reason::VirtualReasonDesc::RSomeProperty,
                    loc.dupe(),
                );
                let assigned_val = ssa_val::number(&mut *self.cache.borrow_mut(), val_reason);
                match refinement_key::Lookup::of_member(member, false) {
                    Some(lookup) => {
                        let assigned_val_1 = assigned_val.dupe();
                        self.map_val_with_lookup(
                            &lookup,
                            Some(|_: &mut Self| assigned_val.dupe()),
                            |_, _| assigned_val_1,
                        );
                    }
                    _ => {}
                }
            }
            _ => {
                // given 'o()++`, read o
                self.expression(argument)?;
            }
        }
        Ok(())
    }

    // things that cause abrupt completions

    fn break_(
        &mut self,
        _loc: &ALoc,
        break_stmt: &flow_parser::ast::statement::Break<ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let label = break_stmt
            .label
            .as_ref()
            .map(|l| abrupt_completion::Label::from(l.name.dupe()));
        self.raise_abrupt_completion(AbruptCompletion::Break(label))
    }

    fn continue_(
        &mut self,
        _loc: &ALoc,
        cont: &flow_parser::ast::statement::Continue<ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let label = cont
            .label
            .as_ref()
            .map(|l| abrupt_completion::Label::from(l.name.dupe()));
        self.raise_abrupt_completion(AbruptCompletion::Continue(label))
    }

    fn body_expression(
        &mut self,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        match &self.env_state.type_guard_name {
            Some(tg_info) => {
                let return_reason = flow_common::reason::mk_expression_reason(expr);
                let tg_info_clone = tg_info.dupe();
                self.record_type_guard_maps(&tg_info_clone, return_reason, expr)?;
            }
            None => {
                ast_visitor::body_expression_default(self, expr)?;
            }
        }
        Ok(())
    }

    fn return_(
        &mut self,
        _loc: &ALoc,
        ret: &flow_parser::ast::statement::Return<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        match (&self.env_state.type_guard_name, &ret.argument) {
            (None, _) | (Some(_), None) => {
                if let Some(arg) = &ret.argument {
                    self.expression(arg)?;
                }
            }
            (Some(tg_info), Some(argument)) => {
                let return_reason = flow_common::reason::mk_expression_reason(argument);
                let tg_info_clone = tg_info.dupe();
                self.record_type_guard_maps(&tg_info_clone, return_reason, argument)?;
            }
        }
        self.raise_abrupt_completion(AbruptCompletion::Return)
    }

    fn throw(
        &mut self,
        _loc: &ALoc,
        throw_stmt: &flow_parser::ast::statement::Throw<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.expression(&throw_stmt.argument)?;
        self.raise_abrupt_completion(AbruptCompletion::Throw)
    }

    fn if_statement(
        &mut self,
        _loc: &ALoc,
        stmt: &flow_parser::ast::statement::If<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let test = &stmt.test;
        let consequent = &stmt.consequent;
        let alternate = &stmt.alternate;

        self.push_refinement_scope(empty_refinements());
        self.expression_refinement(test)?;
        let test_refinements = self.peek_new_refinements();
        let env0 = self.env_snapshot_without_latest_refinements();
        // collect completions and environments of every branch
        let then_completion_state = self.run_to_completion(|r| {
            r.if_consequent_statement(alternate.is_some(), consequent)?;
            Ok(())
        });
        let then_env_no_refinements = self.env_snapshot_without_latest_refinements();
        let then_env_with_refinements = self.env_snapshot();
        self.pop_refinement_scope();
        self.reset_env(&env0);
        self.push_refinement_scope(test_refinements);
        self.negate_new_refinements();
        let else_completion_state = self.run_to_completion(|r| {
            if let Some(alt) = alternate {
                r.statement(&alt.body)?;
            }
            Ok(())
        });
        // merge environments
        let else_env_no_refinements = self.env_snapshot_without_latest_refinements();
        let else_env_with_refinements = self.env_snapshot();
        self.pop_refinement_scope();
        self.reset_env(&env0);
        self.merge_conditional_branches_with_refinements(
            (
                &then_env_no_refinements,
                &then_env_with_refinements,
                then_completion_state.as_ref(),
            ),
            (
                &else_env_no_refinements,
                &else_env_with_refinements,
                else_completion_state.as_ref(),
            ),
        );

        // merge completions
        self.merge_completion_states(then_completion_state.as_ref(), &[else_completion_state])
    }

    fn conditional(
        &mut self,
        _loc: &ALoc,
        expr: &flow_parser::ast::expression::Conditional<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.push_refinement_scope(empty_refinements());
        self.expression_refinement(&expr.test)?;
        let test_refinements = self.peek_new_refinements();
        let env0 = self.env_snapshot_without_latest_refinements();
        let consequent_completion_state = self.run_to_completion(|r| {
            r.expression(&expr.consequent)?;
            Ok(())
        });
        let consequent_env_no_refinements = self.env_snapshot_without_latest_refinements();
        let consequent_env_with_refinements = self.env_snapshot();
        self.pop_refinement_scope();
        self.reset_env(&env0);
        self.push_refinement_scope(test_refinements);
        self.negate_new_refinements();
        let alternate_completion_state = self.run_to_completion(|r| {
            r.expression(&expr.alternate)?;
            Ok(())
        });
        let alternate_env_no_refinements = self.env_snapshot_without_latest_refinements();
        let alternate_env_with_refinements = self.env_snapshot();
        self.pop_refinement_scope();
        self.reset_env(&env0);
        self.merge_conditional_branches_with_refinements(
            (
                &consequent_env_no_refinements,
                &consequent_env_with_refinements,
                consequent_completion_state.as_ref(),
            ),
            (
                &alternate_env_no_refinements,
                &alternate_env_with_refinements,
                alternate_completion_state.as_ref(),
            ),
        );
        self.merge_completion_states(
            consequent_completion_state.as_ref(),
            &[alternate_completion_state],
        )
    }

    fn match_<B>(
        &mut self,
        _match_loc: &ALoc,
        x: &'ast flow_parser::ast::match_::Match<ALoc, ALoc, B>,
        mut on_case_body: impl FnMut(&mut Self, &'ast B) -> Result<(), AbruptCompletion>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::match_::Match;
        let Match {
            arg,
            cases,
            match_keyword_loc,
            comments: _,
        } = x;
        self.expression(arg)?;
        let env0 = self.env_snapshot();
        let bindings = {
            let match_root_id = ast_utils::match_root_ident::<ALoc, ALoc>(match_keyword_loc.dupe());
            let entry = flow_analysis::bindings::Entry {
                loc: match_root_id.loc.dupe(),
                name: match_root_id.name.dupe(),
                kind: flow_analysis::bindings::Kind::Internal,
            };
            Bindings::singleton(entry)
        };
        let cases_info = self.with_bindings(true, match_keyword_loc.dupe(), bindings, |r| {
            let match_root_id2 =
                ast_utils::match_root_ident::<ALoc, ALoc>(match_keyword_loc.dupe());
            let missing_annot = flow_parser::ast::types::AnnotationOrHint::Missing(ALoc::none());
            r.pattern_identifier_with_annot_check(
                Some(flow_parser::ast::VariableKind::Const),
                match_keyword_loc.dupe(),
                &match_root_id2,
                &missing_annot,
            )?;
            r.identifier(&ast_utils::match_root_ident(match_keyword_loc.dupe()))?;
            let cases_info: Vec<_> = cases
                .iter()
                .map(|case| r.visit_match_case(case, &mut on_case_body))
                .collect();
            let match_expr = flow_parser::ast::expression::Expression::new(
                flow_parser::ast::expression::ExpressionInner::Identifier {
                    loc: match_keyword_loc.dupe(),
                    inner: ast_utils::match_root_ident(match_keyword_loc.dupe()),
                },
            );
            if let Some(refined_value) = r.get_val_of_expression(&match_expr) {
                r.env_state.values.insert(
                    match_keyword_loc.dupe(),
                    ReadEntry {
                        def_loc: None,
                        value: refined_value,
                        val_binding_kind: ssa_val::ValBindingKind::InternalBinding,
                        name: None,
                    },
                );
            }

            Ok(cases_info)
        })?;
        self.reset_env(&env0);

        match cases_info.as_slice() {
            [] => {}
            [(_, env_next, completion_state)] => {
                if completion_state.is_none() {
                    self.reset_env(env_next);
                }
                Self::from_completion(completion_state.dupe())?;
            }
            [(_, first_env, first_completion_state), rest_cases_info @ ..] => {
                let (next_env, rest_completion_states) = rest_cases_info.iter().fold(
                    (first_env.dupe(), vec![]),
                    |(env_acc, mut completion_states_acc), (_, case_env, completion_state)| {
                        self.merge_env(&env_acc, case_env);
                        completion_states_acc.push(completion_state.dupe());
                        (self.env_snapshot(), completion_states_acc)
                    },
                );
                self.reset_env(&env0);
                self.merge_completion_states(
                    first_completion_state.as_ref(),
                    &rest_completion_states,
                )?;
                self.reset_env(&next_env);
            }
        }
        Ok(())
    }

    fn while_(
        &mut self,
        _loc: &ALoc,
        stmt: &flow_parser::ast::statement::While<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let test = &stmt.test;
        let body = &stmt.body;
        let scout = |resolver: &mut Self| {
            resolver.expression(test)?;
            resolver.run_to_completion(|r| {
                r.statement(body)?;
                Ok(())
            });
            Ok(())
        };
        let visit_guard_and_body = |resolver: &mut Self| {
            let env_before_guard = resolver.env_snapshot();
            if let Err(ac) = resolver.expression_refinement(test) {
                return Ok((Some(ac), env_before_guard, None));
            }
            let env = resolver.env_snapshot_without_latest_refinements();
            let loop_completion_state = resolver.run_to_completion(|r| {
                r.statement(body)?;
                Ok(())
            });
            Ok((loop_completion_state, env_before_guard, Some(env)))
        };
        let make_completion_states =
            |loop_completion_state: Option<AbruptCompletion>| (None, vec![loop_completion_state]);
        let continues: Vec<AbruptCompletion> = std::iter::once(AbruptCompletion::Continue(None))
            .chain(self.env_state.possible_labeled_continues.dupe())
            .collect();
        self.env_loop(
            Some(test),
            scout,
            visit_guard_and_body,
            make_completion_states,
            true,
            continues,
        )
    }

    fn do_while(
        &mut self,
        _loc: &ALoc,
        stmt: &flow_parser::ast::statement::DoWhile<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let continues: Vec<AbruptCompletion> = std::iter::once(AbruptCompletion::Continue(None))
            .chain(self.env_state.possible_labeled_continues.dupe())
            .collect();

        let scout = {
            let continues_clone = continues.clone();
            move |resolver: &mut Self| {
                let loop_completion_state = resolver.run_to_completion(|r| {
                    r.statement(&stmt.body)?;
                    Ok(())
                });
                resolver.handle_continues(loop_completion_state.as_ref(), &continues_clone);
                if loop_completion_state.is_none() {
                    resolver.expression(&stmt.test)?;
                }
                Ok(())
            }
        };

        let visit_guard_and_body = {
            let continues_clone = continues.clone();
            move |resolver: &mut Self| {
                let loop_completion_state = resolver.run_to_completion(|r| {
                    r.statement(&stmt.body)?;
                    Ok(())
                });
                let loop_completion_state =
                    resolver.handle_continues(loop_completion_state.as_ref(), &continues_clone);
                let env_before_guard = resolver.env_snapshot();
                if loop_completion_state.is_none() {
                    if let Err(ac) = resolver.expression_refinement(&stmt.test) {
                        return Ok((Some(ac), env_before_guard, None));
                    }
                }
                Ok((loop_completion_state, env_before_guard, None))
            }
        };

        let make_completion_states =
            |loop_completion_state: Option<AbruptCompletion>| (loop_completion_state, vec![]);

        self.env_loop(
            Some(&stmt.test),
            scout,
            visit_guard_and_body,
            make_completion_states,
            false,
            continues,
        )
    }

    fn for_statement(
        &mut self,
        loc: &ALoc,
        stmt: &flow_parser::ast::statement::For<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::for_statement(self, self.enable_enums, loc, stmt, |this, loc, stmt| {
            this.scoped_for_statement(loc.dupe(), stmt)
        })
    }

    fn for_in_statement(
        &mut self,
        loc: &ALoc,
        stmt: &flow_parser::ast::statement::ForIn<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::for_in_statement(self, self.enable_enums, loc, stmt, |this, loc, stmt| {
            this.scoped_for_in_statement(loc.dupe(), stmt)
        })
    }

    fn for_of_statement(
        &mut self,
        loc: &ALoc,
        stmt: &flow_parser::ast::statement::ForOf<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::for_of_statement(self, self.enable_enums, loc, stmt, |this, loc, stmt| {
            this.scoped_for_of_statement(loc.dupe(), stmt)
        })
    }

    fn for_in_left_declaration(
        &mut self,
        left: &(
            ALoc,
            flow_parser::ast::statement::VariableDeclaration<ALoc, ALoc>,
        ),
    ) -> Result<(), AbruptCompletion> {
        self.for_in_or_of_left_declaration(left)
    }

    fn for_of_left_declaration(
        &mut self,
        left: &(
            ALoc,
            flow_parser::ast::statement::VariableDeclaration<ALoc, ALoc>,
        ),
    ) -> Result<(), AbruptCompletion> {
        self.for_in_or_of_left_declaration(left)
    }

    // Block hoisting for lexical declarations
    fn block(
        &mut self,
        loc: &ALoc,
        stmt: &flow_parser::ast::statement::Block<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let mut lexical_hoist = LexicalHoister::new(self.enable_enums);
        for s in stmt.body.iter() {
            let Ok(()) = lexical_hoist.statement(s);
        }
        let lexical_bindings = lexical_hoist.into_bindings();
        self.with_bindings(true, loc.dupe(), lexical_bindings, |r| {
            ast_visitor::block_default(r, loc, stmt)
        })
    }

    // Function and component bodies bypass the block's lexical hoisting since
    // they have their own scoping handled in lambda/component methods

    fn function_body(
        &mut self,
        body: &(ALoc, flow_parser::ast::statement::Block<ALoc, ALoc>),
    ) -> Result<(), AbruptCompletion> {
        ast_visitor::block_default(self, &body.0, &body.1)
    }

    fn component_body(
        &mut self,
        body: &(ALoc, flow_parser::ast::statement::Block<ALoc, ALoc>),
    ) -> Result<(), AbruptCompletion> {
        ast_visitor::block_default(self, &body.0, &body.1)
    }

    fn catch_clause(
        &mut self,
        clause: &flow_parser::ast::statement::try_::CatchClause<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::catch_clause(self, self.enable_enums, clause, |this, clause| {
            ast_visitor::catch_clause_default(this, clause)
        })
    }

    fn match_case<B>(
        &mut self,
        case: &'ast flow_parser::ast::match_::Case<ALoc, ALoc, B>,
        on_case_body: &mut impl FnMut(&mut Self, &'ast B) -> Result<(), AbruptCompletion>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::match_case(self, self.enable_enums, case, on_case_body)
    }

    fn switch(
        &mut self,
        loc: &ALoc,
        switch: &flow_parser::ast::statement::Switch<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::statement::Switch;

        let incoming_env = self.env_snapshot();
        let Switch {
            discriminant,
            cases,
            comments: _,
            exhaustive_out,
        } = switch;
        self.expression(discriminant)?;
        let mut lexical_hoist = LexicalHoister::new(self.enable_enums);

        let cases_with_lexical_bindings: Vec<_> = cases
            .iter()
            .map(|case| {
                let bindings = lexical_hoist.acc().to_map();
                for stmt in case.consequent.iter() {
                    let Ok(()) = lexical_hoist.statement(stmt);
                }
                (case, bindings)
            })
            .collect();

        self.run(
            |resolver| {
                resolver.with_bindings(true, loc.dupe(), lexical_hoist.into_bindings(), |r| {
                    r.switch_cases_with_lexical_bindings(
                        loc,
                        exhaustive_out,
                        discriminant,
                        &cases_with_lexical_bindings,
                    )
                })
            },
            |resolver| {
                let post_env = resolver.env_snapshot();
                // After all refinements and potential shadowing inside switch,
                // we need to re-read the discriminant to restore it.
                resolver.reset_env(&incoming_env);
                resolver.expression(discriminant)?;
                resolver.reset_env(&post_env);
                Ok(())
            },
        )?;
        Ok(())
    }

    fn try_catch(
        &mut self,
        _loc: &ALoc,
        stmt: &flow_parser::ast::statement::Try<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::statement::Try;

        self.expecting_abrupt_completions(|resolver| {
            let Try {
                block: (loc, block),
                handler,
                finalizer,
                comments: _,
            } = stmt;

            let try_entry_env = resolver.env_snapshot();
            let try_completion_state = resolver.run_to_completion(|r| r.block(loc, block));
            let try_exit_env = resolver.env_snapshot();
            // The catch entry env must take into account the fact that any line in the try may
            // have thrown. We conservatively approximate this by assuming that the very first
            // line may have thrown, so we merge the entrance env. The other possible states that
            //can bring us to the catch are the envs at exceptions explicitly thrown in try, so
            // we merge those in as well.
            resolver.merge_self_env(&try_entry_env);
            // Merge in all the throw envs
            resolver.commit_abrupt_completion_matching(
                |c| matches!(c, AbruptCompletion::Throw),
                None,
            )?;

            let catch_completion_state = match handler {
                Some(clause) => resolver.run_to_completion(|r| r.catch_clause(clause)),
                None => {
                    // No catch is like having a catch that always re-throws the error from the try block.
                    resolver
                        .run_to_completion(|r| r.raise_abrupt_completion(AbruptCompletion::Throw))
                }
            };
            let catch_exit_env = resolver.env_snapshot();
            let completion_state = match (&try_completion_state, &catch_completion_state) {
                (Some(AbruptCompletion::Throw), Some(AbruptCompletion::Throw))
                | (Some(AbruptCompletion::Return), Some(_)) => try_completion_state.dupe(),
                (Some(AbruptCompletion::Throw), Some(AbruptCompletion::Return)) => {
                    catch_completion_state.dupe()
                }
                _ => None,
            };

            // Finalizers must be checked twice under two environments. We need one to determine
            // what the env should be after the try/catch/finally, which assumes that either try or
            // catch did not throw. This assumption, however, is too optimistic, so checking the
            // finally under this assumption would be unsound. To soundly check the finally, we make
            // no assumptions about throws. In that case, the entry env here is a merge of the try
            // start env, try exit env, and catch exit env, along with every abrupt completion env
            // from both the try and the catch.
            let finally_completion_state = match finalizer {
                Some((_fin_loc, fin_block)) => {
                    resolver.expecting_abrupt_completions(|r| {
                        match &catch_completion_state {
                            None => r.merge_env(&try_exit_env, &catch_exit_env),
                            Some(_) => r.reset_env(&try_exit_env),
                        }
                        r.run_to_completion(|r2| r2.block(loc, fin_block));
                    });
                    let exit_env = resolver.env_snapshot();
                    // Now check assuming that we may throw anywhere in try or catch so that
                    // we can conservatively check the finally case. The starting env here is modeled as
                    // the merge of the try_entry_env with the catch_exit env, and we include all abrupt
                    // completion envs
                    resolver.merge_env(&try_entry_env, &try_exit_env);
                    resolver.merge_self_env(&catch_exit_env);
                    resolver.commit_abrupt_completion_matching(
                        AbruptCompletion::all,
                        completion_state.as_ref(),
                    )?;
                    let completion_state = resolver.run_to_completion(|r| r.block(loc, fin_block));
                    resolver.reset_env(&exit_env);
                    completion_state
                }
                None => {
                    match &catch_completion_state {
                        None => resolver.merge_env(&try_exit_env, &catch_exit_env),
                        Some(_) => resolver.reset_env(&try_exit_env),
                    }
                    None
                }
            };

            // If finally has some sort of abnormal completion then we re-raise it. If not, we
            // consider the completion states from try/catch
            Self::from_completion(finally_completion_state)?;
            Self::from_completion(completion_state)
        })
    }

    fn function_param_pattern(
        &mut self,
        pattern: &flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ploc = pattern.loc().dupe();
        self.visit_function_or_component_param_pattern(false, ploc, pattern);
        ast_visitor::function_param_pattern_default(self, pattern)
    }

    fn function_rest_param(
        &mut self,
        rest_param: &flow_parser::ast::function::RestParam<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let argument = &rest_param.argument;
        let ploc = argument.loc().dupe();
        self.visit_function_or_component_param_pattern(true, ploc, argument);
        ast_visitor::function_param_pattern_default(self, argument)?;
        Ok(())
    }

    fn component_param_pattern(
        &mut self,
        pattern: &flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let ploc = pattern.loc().dupe();
        self.visit_function_or_component_param_pattern(false, ploc, pattern);
        ast_visitor::component_param_pattern_default(self, pattern)?;
        Ok(())
    }

    fn component_rest_param(
        &mut self,
        rest_param: &flow_parser::ast::statement::component_params::RestParam<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let argument = &rest_param.argument;
        let ploc = argument.loc().dupe();
        self.visit_function_or_component_param_pattern(true, ploc, argument);
        ast_visitor::component_param_pattern_default(self, argument)?;
        Ok(())
    }

    fn function_declaration(
        &mut self,
        loc: &ALoc,
        func: &flow_parser::ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::function_declaration(
            self,
            self.enable_enums,
            true,
            loc,
            func,
            &|this, f| this.hoist_annotations(|this_inner| f(this_inner)),
            |this, fun_loc, has_this_annot, id| {
                this.this_binding_function_id_opt(fun_loc.dupe(), has_this_annot, id)
            },
            |this,
             _enable_enums,
             _with_types,
             is_arrow,
             fun_loc,
             generator_return_loc,
             params,
             return_,
             predicate,
             body| {
                this.lambda(
                    is_arrow,
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
        loc: &ALoc,
        func: &flow_parser::ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.function_expression_without_name(false, loc, func)
    }

    fn arrow_function(
        &mut self,
        loc: &ALoc,
        func: &flow_parser::ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.function_expression_without_name(true, loc, func)
    }

    fn type_alias(
        &mut self,
        _loc: &ALoc,
        alias: &flow_parser::ast::statement::TypeAlias<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::type_alias(self, true, alias, &|this, f| {
            this.hoist_annotations(|this_inner| f(this_inner))
        })
    }

    fn opaque_type(
        &mut self,
        _loc: &ALoc,
        opaque: &flow_parser::ast::statement::OpaqueType<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::opaque_type(self, true, opaque, &|this, f| {
            this.hoist_annotations(|this_inner| f(this_inner))
        })
    }

    fn interface(
        &mut self,
        _loc: &ALoc,
        iface: &flow_parser::ast::statement::Interface<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::interface(self, true, iface, &|this, f| {
            this.hoist_annotations(|this_inner| f(this_inner))
        })
    }

    fn function_type(
        &mut self,
        ft: &flow_parser::ast::types::Function<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::function_type(self, true, ft, &|this, f| {
            this.hoist_annotations(|this_inner| f(this_inner))
        })
    }

    fn component_declaration(
        &mut self,
        loc: &ALoc,
        component: &flow_parser::ast::statement::ComponentDeclaration<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::component_declaration(
            self,
            true,
            loc,
            component,
            &|this, f| this.hoist_annotations(|this_inner| f(this_inner)),
            |this, body, params| this.component_body_with_params(loc, body, params),
        )
    }

    fn declare_component(
        &mut self,
        _loc: &ALoc,
        decl: &flow_parser::ast::statement::DeclareComponent<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::declare_component(self, true, decl, &|this, f| {
            this.hoist_annotations(|this_inner| f(this_inner))
        })
    }

    fn component_type(
        &mut self,
        _loc: &ALoc,
        t: &flow_parser::ast::types::Component<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::component_type(self, true, t, &|this, f| {
            this.hoist_annotations(|this_inner| f(this_inner))
        })
    }

    fn object_mapped_type_property(
        &mut self,
        mt: &flow_parser::ast::types::object::MappedType<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::object_mapped_type_property(self, true, mt, &|this, f| {
            this.hoist_annotations(|this_inner| f(this_inner))
        })
    }

    // =========================================================================
    // Stopper methods - prevent certain identifiers from being visited as reads
    // =========================================================================

    fn member_property_identifier(
        &mut self,
        _id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn typeof_member_identifier(
        &mut self,
        _ident: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn member_type_identifier(
        &mut self,
        _id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn pattern_object_property_identifier_key(
        &mut self,
        _kind: Option<flow_parser::ast::VariableKind>,
        _id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn match_object_pattern_property_key(
        &mut self,
        _key: &flow_parser::ast::match_pattern::object_pattern::Key<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn match_object_pattern_property(
        &mut self,
        prop: &flow_parser::ast::match_pattern::object_pattern::Property<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::match_pattern::object_pattern::Property;
        match prop {
            Property::Valid { .. } => {
                ast_visitor::match_object_pattern_property_default(self, prop)
            }
            Property::InvalidShorthand { .. } => Ok(()),
        }
    }

    fn enum_member_identifier(
        &mut self,
        _id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn enum_declaration(
        &mut self,
        loc: &ALoc,
        enum_decl: &flow_parser::ast::statement::EnumDeclaration<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        if !self.enable_enums {
            Ok(())
        } else {
            ast_visitor::enum_declaration_default(self, loc, enum_decl)
        }
    }

    fn object_key_identifier(
        &mut self,
        _id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn component_param_name(
        &mut self,
        _param_name: &flow_parser::ast::statement::component_params::ParamName<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    // Import/export handling - avoid visiting aliased names that aren't variables
    fn import_named_specifier(
        &mut self,
        import_kind: flow_parser::ast::statement::ImportKind,
        specifier: &flow_parser::ast::statement::import_declaration::NamedSpecifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::import_named_specifier(self, true, import_kind, specifier)
    }

    // The Identifier form of import_equals_declaration (import X = A.B.C) is
    // not supported. The hoister does not create a binding for it, and name_def
    // does not create a definition for it. We must skip calling
    // pattern_identifier/binding_type_identifier here to avoid creating write
    // entries for a location that has no corresponding name_def entry, which
    // would cause NameDefOrderingFailure.
    fn import_equals_declaration(
        &mut self,
        loc: &ALoc,
        decl: &flow_parser::ast::statement::ImportEqualsDeclaration<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::statement::import_equals_declaration::ModuleReference;
        match &decl.module_reference {
            ModuleReference::ExternalModuleReference(..) => {
                flow_parser::ast_visitor::import_equals_declaration_default(self, loc, decl)
            }
            ModuleReference::Identifier(..) => Ok(()),
        }
    }

    // don't rename the `bar` in `export {foo as bar}`
    fn export_named_declaration_specifier(
        &mut self,
        spec: &flow_parser::ast::statement::export_named_declaration::ExportSpecifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.identifier(&spec.local)?;
        Ok(())
    }

    fn conditional_type(
        &mut self,
        conditional: &flow_parser::ast::types::Conditional<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::conditional_type(
            self,
            conditional,
            |this, extends_type| this.extends_in_infer_type(|v| v.type_(extends_type)),
            |this, loc, tps, in_tparam_scope| {
                scope_builder::scoped_infer_type_params(
                    this,
                    true,
                    loc,
                    tps,
                    |s, id| {
                        s.binding_infer_type_identifier(id.loc.dupe(), &id.name);
                        Ok(())
                    },
                    in_tparam_scope,
                )
            },
        )
    }

    // Visits of infer type are skipped, because they are handled in conditional type above
    fn infer_type(
        &mut self,
        _infer: &flow_parser::ast::types::Infer<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        Ok(())
    }

    fn class_expression(
        &mut self,
        loc: &ALoc,
        cls: &flow_parser::ast::class::Class<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        // Give class body the location of the entire class,
        // so class body visitor can use it as the def loc of super.
        let modified_cls = flow_parser::ast::class::Class {
            body: flow_parser::ast::class::Body {
                loc: loc.dupe(),
                ..cls.body.clone()
            },
            ..cls.clone()
        };
        scope_builder::class_expression(self, loc, &modified_cls, |this, loc, cls| {
            this.class_impl(loc, cls)
        })
    }

    fn declare_class(
        &mut self,
        _loc: &ALoc,
        decl: &flow_parser::ast::statement::DeclareClass<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::declare_class(self, true, decl, &|this, f| {
            this.hoist_annotations(|this_inner| f(this_inner))
        })
    }

    fn declare_function(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::statement::DeclareFunction<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        scope_builder::declare_function(self, loc, expr, &|this, f| {
            this.hoist_annotations(|this_inner| f(this_inner))
        })
    }

    fn function_params(
        &mut self,
        params: &flow_parser::ast::function::Params<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        if let Some((loc, name)) = &self.env_state.inferred_type_guard_candidate {
            let entry = self.env_read(name);
            let id = entry.val_ref.borrow().id;
            let info = TypeGuardNameInfo(Rc::new(TypeGuardNameInfoInner {
                loc: loc.dupe(),
                name: name.dupe(),
                id,
                havoced: RefCell::new(None),
                inferred: true,
            }));
            self.env_state.type_guard_name = Some(info);
        }
        ast_visitor::function_params_default(self, params)
    }

    fn type_guard_annotation(
        &mut self,
        tg: &flow_parser::ast::types::TypeGuardAnnotation<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let guard_id = &tg.guard.guard.0;
        let loc = guard_id.loc.dupe();
        let name = guard_id.name.dupe();
        let entry = self.env_read(&name);
        let id = entry.val_ref.borrow().id;
        let info = TypeGuardNameInfo(Rc::new(TypeGuardNameInfoInner {
            loc,
            name,
            id,
            havoced: RefCell::new(None),
            inferred: false,
        }));
        self.env_state.type_guard_name = Some(info);
        ast_visitor::type_guard_annotation_default(self, tg)
    }

    fn class_(
        &mut self,
        loc: &ALoc,
        cls: &flow_parser::ast::class::Class<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        super_call_in_derived_ctor_checker::check(
            self.enable_enums,
            &mut |msg| Fl::add_output(self.cx, msg),
            loc.dupe(),
            cls,
        );
        self.class_stack.push_back(loc.dupe());
        let modified_cls = flow_parser::ast::class::Class {
            body: flow_parser::ast::class::Body {
                loc: loc.dupe(),
                ..cls.body.clone()
            },
            ..cls.clone()
        };
        // Give class body the location of the entire class,
        // so class body visitor can use it as the def loc of super.
        self.class_impl(loc, &modified_cls)?;
        self.class_stack.pop_back();
        Ok(())
    }

    fn class_identifier(
        &mut self,
        ident: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        // Called by `declare class`
        self.check_class_name(ident.loc.dupe(), &ident.name);
        ast_visitor::class_identifier_default(self, ident)
    }

    fn class_body(
        &mut self,
        cls_body: &flow_parser::ast::class::Body<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_analysis::bindings;
        use flow_parser::ast::class::BodyElement;

        let loc = &cls_body.loc;
        let body = &cls_body.body;

        let (static_body, instance_body): (Vec<_>, Vec<_>) =
            body.iter().partition(|element| match element {
                BodyElement::Method(method) => method.static_,
                BodyElement::Property(prop) => prop.static_,
                BodyElement::PrivateField(field) => field.static_,
                BodyElement::DeclareMethod(dm) => dm.static_,
                BodyElement::StaticBlock(_) => true,
                BodyElement::AbstractMethod(_) => false,
                BodyElement::AbstractProperty(_) => false,
                BodyElement::IndexSignature(idx) => idx.static_,
            });

        let mut this_super_bindings = bindings::Bindings::empty();
        this_super_bindings.add(bindings::Entry {
            loc: loc.dupe(),
            name: FlowSmolStr::from("this"),
            kind: bindings::Kind::Const,
        });
        this_super_bindings.add(bindings::Entry {
            loc: loc.dupe(),
            name: FlowSmolStr::from("super"),
            kind: bindings::Kind::Const,
        });
        self.with_scoped_bindings(
            ThisSuperBindingEnv::ClassInstanceEnv,
            &this_super_bindings,
            |this| {
                for element in &instance_body {
                    this.class_element(element)?;
                }
                Ok(())
            },
        )?;
        self.with_scoped_bindings(
            ThisSuperBindingEnv::ClassStaticEnv,
            &this_super_bindings,
            |this| {
                for element in &static_body {
                    this.class_element(element)?;
                }
                Ok(())
            },
        )?;
        Ok(())
    }

    fn class_method(
        &mut self,
        meth: &flow_parser::ast::class::Method<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let key = &meth.key;
        let (f_loc, f) = &meth.value;
        let decorators = &meth.decorators;
        self.object_key(key)?;
        // When there is no `this` annotation, we treat the method like an arrow function:
        // it will now inherit `this` from parent scope.
        let has_this_param = f.params.this_.is_some();
        // Use the method key loc as the loc of the function.
        if has_this_param {
            self.function_expression_or_method(f_loc, f)?;
        } else {
            self.non_this_binding_function(f_loc, f)?;
        }
        for d in decorators.iter() {
            self.class_decorator(d)?;
        }
        Ok(())
    }

    fn class_property(
        &mut self,
        prop: &flow_parser::ast::class::Property<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let static_ = prop.static_;
        if static_ {
            ast_visitor::class_property_default(self, prop)
        } else {
            self.under_uninitialized_env(|this| {
                let env = this.env_snapshot();
                this.run(
                    |this2| ast_visitor::class_property_default(this2, prop),
                    |this2| {
                        this2.reset_env(&env);
                        Ok(())
                    },
                )
            })
        }
    }

    fn class_private_field(
        &mut self,
        field: &flow_parser::ast::class::PrivateField<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let static_ = field.static_;
        if static_ {
            ast_visitor::class_private_field_default(self, field)
        } else {
            self.under_uninitialized_env(|this| {
                let env = this.env_snapshot();
                this.run(
                    |this2| ast_visitor::class_private_field_default(this2, field),
                    |this2| {
                        this2.reset_env(&env);
                        Ok(())
                    },
                )
            })
        }
    }

    fn record_declaration(
        &mut self,
        loc: &ALoc,
        record: &flow_parser::ast::statement::RecordDeclaration<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let name_loc = record.id.loc.dupe();
        let name = &record.id.name;

        self.class_stack.push_back(loc.dupe());
        self.check_class_name(name_loc.dupe(), name);

        let modified_record = flow_parser::ast::statement::RecordDeclaration {
            body: flow_parser::ast::statement::record_declaration::Body {
                loc: loc.dupe(),
                ..record.body.clone()
            },
            ..record.clone()
        };
        scope_builder::record_declaration(self, true, &modified_record, &|v, f| {
            v.hoist_annotations(f)
        })?;

        use flow_common::reason::Name;
        use flow_common::reason::VirtualReason;
        use flow_common::reason::VirtualReasonDesc;

        let record_self_reason = VirtualReason::new(
            VirtualReasonDesc::RType(Name::new(name.dupe())),
            name_loc.dupe(),
        );
        if self.is_assigning_write(&env_api::EnvKey::ordinary(name_loc.dupe())) {
            let self_write = env_api::EnvEntry::AssigningWrite(record_self_reason);
            let this_write = env_api::EnvEntry::AssigningWrite(VirtualReason::new(
                VirtualReasonDesc::RThis,
                loc.dupe(),
            ));
            let super_write = env_api::EnvEntry::AssigningWrite(VirtualReason::new(
                VirtualReasonDesc::RSuper,
                loc.dupe(),
            ));

            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::ClassSelfLoc, loc.dupe()),
                self_write,
            );
            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::ClassInstanceThisLoc, loc.dupe()),
                this_write.dupe(),
            );
            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::ClassInstanceSuperLoc, loc.dupe()),
                super_write.dupe(),
            );
            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::ClassStaticThisLoc, loc.dupe()),
                this_write,
            );
            self.env_state.write_entries.insert(
                env_api::EnvKey::new(env_api::DefLocType::ClassStaticSuperLoc, loc.dupe()),
                super_write,
            );
        }

        self.class_stack.pop_back();
        Ok(())
    }

    fn record_declaration_body(
        &mut self,
        body: &flow_parser::ast::statement::record_declaration::Body<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_analysis::bindings;
        use flow_parser::ast::statement::record_declaration::BodyElement;

        let loc = &body.loc;
        let elements = &body.body;

        let (static_elements, instance_elements): (Vec<_>, Vec<_>) =
            elements.iter().partition(|element| match element {
                BodyElement::Method(method) => method.static_,
                BodyElement::Property(_) => false,
                BodyElement::StaticProperty(_) => true,
            });

        let mut this_super_bindings = bindings::Bindings::empty();
        this_super_bindings.add(bindings::Entry {
            loc: loc.dupe(),
            name: FlowSmolStr::from("this"),
            kind: bindings::Kind::Const,
        });
        this_super_bindings.add(bindings::Entry {
            loc: loc.dupe(),
            name: FlowSmolStr::from("super"),
            kind: bindings::Kind::Const,
        });

        self.with_scoped_bindings(
            ThisSuperBindingEnv::ClassInstanceEnv,
            &this_super_bindings,
            |this| {
                for element in &instance_elements {
                    this.record_declaration_body_element(element)?;
                }
                Ok(())
            },
        )?;
        self.with_scoped_bindings(
            ThisSuperBindingEnv::ClassStaticEnv,
            &this_super_bindings,
            |this| {
                for element in &static_elements {
                    this.record_declaration_body_element(element)?;
                }
                Ok(())
            },
        )?;

        Ok(())
    }

    fn record_declaration_property(
        &mut self,
        prop: &flow_parser::ast::statement::record_declaration::Property<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.under_uninitialized_env(|this| {
            let env = this.env_snapshot();
            this.run(
                |this2| ast_visitor::record_declaration_property_default(this2, prop),
                |this2| {
                    this2.reset_env(&env);
                    Ok(())
                },
            )
        })?;
        Ok(())
    }

    fn object(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::expression::Object<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let properties = &expr.properties;
        let checks =
            flow_env_builder::eq_test::object_properties_possible_sentinel_refinements(properties);
        self.add_sentinel_check_writes(checks);
        ast_visitor::object_default(self, loc, expr)
    }

    fn object_property(
        &mut self,
        prop: &flow_parser::ast::expression::object::NormalProperty<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::expression::object::NormalProperty;

        match prop {
            NormalProperty::Init { .. } => ast_visitor::object_property_default(self, prop),
            NormalProperty::Method { key, value, .. }
            | NormalProperty::Get { key, value, .. }
            | NormalProperty::Set { key, value, .. } => {
                self.object_key(key)?;
                let (loc, func) = value;
                let mut illegal_this_super_binding =
                    flow_analysis::bindings::Bindings::singleton(flow_analysis::bindings::Entry {
                        loc: loc.dupe(),
                        name: FlowSmolStr::new_inline("this"),
                        kind: flow_analysis::bindings::Kind::Const,
                    });
                illegal_this_super_binding.add(flow_analysis::bindings::Entry {
                    loc: loc.dupe(),
                    name: FlowSmolStr::new_inline("super"),
                    kind: flow_analysis::bindings::Kind::Const,
                });
                // Do not bind this to a function-level this as usual. We use arrow function visitor
                // so that we purposely skip this binding, and instead we bind this under a special
                // IllegalThisEnv.
                self.with_scoped_bindings(
                    ThisSuperBindingEnv::IllegalThisEnv,
                    &illegal_this_super_binding,
                    |this| this.non_this_binding_function(loc, func),
                )
            }
        }
    }

    fn call(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::expression::Call<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::mk_expression_reason;
        use flow_common::refinement_invalidation::Reason;
        use flow_parser::ast::expression::ExpressionOrSpread;

        let callee = &expr.callee;
        let targs = &expr.targs;
        let arguments = &expr.arguments;

        match flow_parser::ast_utils::get_call_to_object_dot_freeze_arg(callee, targs, arguments) {
            Some((obj_loc, obj)) => {
                ast_visitor::expression_default(self, callee)?;
                ast_visitor::object_default(self, obj_loc, obj)?;
                let call_expr = flow_parser::ast::expression::Expression::new(
                    flow_parser::ast::expression::ExpressionInner::Call {
                        loc: loc.dupe(),
                        inner: Arc::new(expr.clone()),
                    },
                );
                let reason = mk_expression_reason(&call_expr);
                self.env_state.write_entries.insert(
                    env_api::EnvKey::new(env_api::DefLocType::ExpressionLoc, loc.dupe()),
                    env_api::EnvEntry::AssigningWrite(reason),
                );
            }
            None => {
                ast_visitor::call_default(self, loc, expr)?;
                if ast_utils::is_call_to_invariant(callee) {
                    let args = &arguments.arguments;
                    match (targs, args.first()) {
                        // invariant() with no args - treated like throw
                        (None, None) => {
                            self.raise_abrupt_completion(AbruptCompletion::Throw)?;
                        }
                        (None, Some(ExpressionOrSpread::Expression(bool_lit))) if matches!(bool_lit.deref() , flow_parser::ast::expression::ExpressionInner::BooleanLiteral { inner, .. } if !inner.value) =>
                        {
                            for arg in args.iter().skip(1) {
                                self.expression_or_spread(arg)?;
                            }
                            self.raise_abrupt_completion(AbruptCompletion::Throw)?;
                        }
                        (None, Some(ExpressionOrSpread::Expression(cond))) => {
                            self.push_refinement_scope(empty_refinements());
                            self.expression_refinement(cond)?;
                            for arg in args.iter().skip(1) {
                                self.expression_or_spread(arg)?;
                            }
                            self.pop_refinement_scope_without_unrefining();
                        }
                        _ => {}
                    }
                } else {
                    self.havoc_current_env(Reason::FunctionCall, loc.dupe());
                }
            }
        }
        Ok(())
    }

    fn arg_list(
        &mut self,
        arg_list: &flow_parser::ast::expression::ArgList<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::mk_expression_reason;
        use flow_parser::ast::expression::ExpressionOrSpread;

        let arguments = &arg_list.arguments;
        for arg in arguments.iter() {
            let expr = match arg {
                ExpressionOrSpread::Expression(expr) => expr,
                ExpressionOrSpread::Spread(spread) => &spread.argument,
            };
            let loc = expr.loc().dupe();
            if !self.provider_info.is_array_provider(&loc) {
                let reason = mk_expression_reason(expr);
                self.env_state.write_entries.insert(
                    env_api::EnvKey::new(env_api::DefLocType::ExpressionLoc, loc),
                    env_api::EnvEntry::AssigningWrite(reason),
                );
            }
        }
        ast_visitor::arg_list_default(self, arg_list)
    }

    fn new(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::expression::New<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::refinement_invalidation::Reason;

        ast_visitor::new_default(self, loc, expr)?;
        self.havoc_current_env(Reason::ConstructorCall, loc.dupe());
        Ok(())
    }

    fn unary_expression(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::expression::Unary<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::refinement_invalidation::Reason;
        use flow_parser::ast::expression::UnaryOperator;

        let argument = &expr.argument;
        let operator = &expr.operator;
        self.expression(argument)?;
        match operator {
            UnaryOperator::Await => {
                self.havoc_current_env(Reason::Await, loc.dupe());
            }
            UnaryOperator::Delete => {
                self.delete(loc.dupe(), argument);
            }
            _ => {}
        }
        Ok(())
    }

    fn yield_(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::expression::Yield<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::refinement_invalidation::Reason;
        let next_name = FlowSmolStr::new_inline(NEXT_VAR_NAME);
        self.any_identifier(loc.dupe(), &next_name);
        ast_visitor::yield_default(self, loc, expr)?;
        self.havoc_current_env(Reason::Yield, loc.dupe());
        Ok(())
    }

    fn object_key_computed(
        &mut self,
        key: &flow_parser::ast::ComputedKey<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_common::reason::mk_expression_reason;

        let expression = &key.expression;
        let expression_loc = expression.loc().dupe();
        let reason = mk_expression_reason(expression);
        self.env_state.write_entries.insert(
            env_api::EnvKey::new(env_api::DefLocType::ExpressionLoc, expression_loc),
            env_api::EnvEntry::AssigningWrite(reason),
        );
        self.expression(expression)?;
        Ok(())
    }

    // Labeled statements handle labeled breaks, but also push labeled continues
    // that are expected to be handled by immediately nested loops.
    fn labeled_statement(
        &mut self,
        _loc: &ALoc,
        stmt: &flow_parser::ast::statement::Labeled<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.expecting_abrupt_completions(|this| {
            let label = &stmt.label;
            let body = &stmt.body;

            let label_name: Option<abrupt_completion::Label> = Some(label.name.dupe().into());
            this.env_state
                .possible_labeled_continues
                .push(AbruptCompletion::Continue(label_name.dupe()));
            let completion_state = this.run_to_completion(|r| r.statement(body));
            this.env_state.possible_labeled_continues.clear();
            this.commit_abrupt_completion_matching(
                |c| matches!(c, AbruptCompletion::Break(l) if *l == label_name),
                completion_state.as_ref(),
            )
        })
    }

    fn statement(
        &mut self,
        stmt: &flow_parser::ast::statement::Statement<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        match stmt.deref() {
            StatementInner::While { .. }
            | StatementInner::DoWhile { .. }
            | StatementInner::For { .. }
            | StatementInner::ForIn { .. }
            | StatementInner::ForOf { .. }
            | StatementInner::Labeled { .. } => {}
            _ => {
                self.env_state.possible_labeled_continues.clear();
            }
        }
        ast_visitor::statement_default(self, stmt)
    }

    fn statement_list(
        &mut self,
        stmts: &[flow_parser::ast::statement::Statement<ALoc, ALoc>],
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::statement::VariableDeclaration;
        use flow_parser::ast::statement::variable::Declarator;

        // Function declarations are hoisted to the top of a block, so that they may be considered
        // initialized before they are read.
        let stmts_hoisted =
            flow_parser::ast_utils::hoist_function_and_component_declarations(stmts.to_vec());
        // If there is any abnormal control flow, add errors on any statements that are
        // lexically after the place where abnormal control was raised.
        let abrupt_completion = stmts_hoisted.iter().fold(None, |abrupt_completion, stmt| {
            if abrupt_completion.is_some() {
                match stmt.deref() {
                    StatementInner::Empty { .. } => {}
                    StatementInner::VariableDeclaration { loc: _, inner } => {
                        let VariableDeclaration {
                            declarations,
                            kind: _,
                            comments: _,
                        } = &**inner;
                        for declarator in declarations.iter() {
                            match declarator {
                                Declarator {
                                    loc: _,
                                    id: _,
                                    init: Some(init),
                                } => {
                                    Fl::add_output(
                                        self.cx,
                                        ErrorMessage::EUnreachable(init.loc().dupe()),
                                    );
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {
                        Fl::add_output(self.cx, ErrorMessage::EUnreachable(stmt.loc().dupe()));
                    }
                }
                abrupt_completion
            } else {
                self.statement(stmt).err()
            }
        });
        Self::from_completion(abrupt_completion)
    }

    fn binary(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::expression::Binary<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::expression::BinaryOperator;

        let operator = &expr.operator;
        let left = &expr.left;
        let right = &expr.right;

        let eq_test = |_this: &mut Self, strict: bool, sense: bool| {
            flow_env_builder::eq_test::dispatch_eq_test(
                false, // is_switch_cond_context
                strict,
                sense,
                loc.dupe(),
                left,
                right,
            );
        };

        match operator {
            BinaryOperator::StrictEqual => {
                eq_test(self, true, true);
                ast_visitor::binary_default(self, loc, expr)
            }
            BinaryOperator::StrictNotEqual => {
                eq_test(self, true, false);
                ast_visitor::binary_default(self, loc, expr)
            }
            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::Instanceof
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual
            | BinaryOperator::In
            | BinaryOperator::LShift
            | BinaryOperator::RShift
            | BinaryOperator::RShift3
            | BinaryOperator::Plus
            | BinaryOperator::Minus
            | BinaryOperator::Mult
            | BinaryOperator::Exp
            | BinaryOperator::Div
            | BinaryOperator::Mod
            | BinaryOperator::BitOr
            | BinaryOperator::Xor
            | BinaryOperator::BitAnd => ast_visitor::binary_default(self, loc, expr),
        }
    }

    fn logical(
        &mut self,
        _loc: &ALoc,
        expr: &flow_parser::ast::expression::Logical<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use env_api::RefinementKind;
        use flow_parser::ast::expression::LogicalOperator;

        let operator = &expr.operator;
        let left = &expr.left;
        let left_loc = left.loc().dupe();
        let right = &expr.right;
        self.push_refinement_scope(empty_refinements());
        // THe LHS is unconditionally evaluated, so we don't run-to-completion and catch the error here
        match operator {
            LogicalOperator::Or | LogicalOperator::And => {
                self.expression_refinement(left)?;
            }
            LogicalOperator::NullishCoalesce => {
                self.expression(left)?;
                let mut refining_locs = empty_refining_locs();
                refining_locs.insert(left_loc.dupe());
                self.add_refinement_to_expr(
                    left,
                    refining_locs,
                    RefinementKind::NotR(Rc::new(RefinementKind::MaybeR)),
                );
            }
        }
        let env1 = self.env_snapshot_without_latest_refinements();
        let env1_with_refinements = self.env_snapshot();
        match operator {
            LogicalOperator::NullishCoalesce | LogicalOperator::Or => {
                self.negate_new_refinements();
            }
            LogicalOperator::And => {}
        }
        // The RHS is _only_ evaluated if the LHS fails its check. That means that patterns like
        // x || invariant(false) should propagate the truthy refinement to the next line. We keep track
        // of the completion state on the rhs to do that. If the LHS throws then the entire expression
        // throws, so there's no need to catch the exception from the LHS
        let rhs_completion_state = self.run_to_completion(|this| this.expression(right));
        match rhs_completion_state {
            Some(AbruptCompletion::Throw) => {
                let env2 = self.env_snapshot();
                self.reset_env(&env1_with_refinements);
                self.pop_refinement_scope_without_unrefining();
                self.merge_self_env(&env2);
            }
            _ => {
                self.pop_refinement_scope();
                self.merge_self_env(&env1);
            }
        }
        Ok(())
    }

    fn expression(
        &mut self,
        expr: &flow_parser::ast::expression::Expression<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::expression::ExpressionInner;

        self.handle_array_providers(expr);
        match expr.deref() {
            ExpressionInner::Call { .. }
            | ExpressionInner::OptionalCall { .. }
            | ExpressionInner::Member { .. }
            | ExpressionInner::OptionalMember { .. } => {
                self.push_refinement_scope(empty_refinements());
                let res = self.optional_chain_must_be_able_to_refine_base_object_to_non_maybe(
                    // The refinement made here is local to the expression and won't escape
                    OptionalChainingRefinement::CanApplyPropNonNullishRefi,
                    expr,
                );
                self.pop_refinement_scope();
                res
            }
            _ => ast_visitor::expression_default(self, expr),
        }
    }

    fn component_param(
        &mut self,
        param: &flow_parser::ast::statement::component_params::Param<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::statement::component_params::ParamName;

        let name = &param.name;
        let local = &param.local;
        let default = &param.default;
        match name {
            ParamName::Identifier(id) if id.name.as_str() == "ref" => {
                self.any_identifier(id.loc.dupe(), &FlowSmolStr::new_inline("React"));
            }
            ParamName::StringLiteral((loc, string_lit)) if string_lit.value.as_str() == "ref" => {
                self.any_identifier(loc.dupe(), &FlowSmolStr::new_inline("React"));
            }
            _ => {}
        }
        self.component_param_name(name)?;
        let ploc = local.loc().dupe();
        self.visit_function_or_component_param_pattern(false, ploc, local);
        self.with_current_pattern_bindings(local, |this| {
            ast_visitor::component_param_pattern_default(this, local)?;
            if let Some(default_expr) = default {
                this.with_in_param_default(|this| this.expression(default_expr))?;
            }
            Ok(())
        })
    }

    fn jsx_element(
        &mut self,
        loc: &ALoc,
        element: &flow_parser::ast::jsx::Element<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        let name = &element.opening_element.name;
        let opening_attributes = &element.opening_element.attributes;
        let checks = flow_env_builder::eq_test::jsx_attributes_possible_sentinel_refinements(
            opening_attributes,
        );
        self.add_sentinel_check_writes(checks);
        // Record a phantom read of "stylex" when the configured shorthand prop is used on
        // lowercase elements, but only if stylex is actually in scope (i.e. imported).
        if let Some(shorthand_prop_name) = self.cx.stylex_shorthand_prop() {
            match name {
                flow_parser::ast::jsx::Name::Identifier(ident)
                    if ident
                        .name
                        .as_bytes()
                        .first()
                        .is_some_and(u8::is_ascii_lowercase) =>
                {
                    let stylex_name = FlowSmolStr::new_inline("stylex");
                    if self.env_read_opt(&stylex_name).is_some() {
                        for attr in opening_attributes.iter() {
                            match attr {
                                flow_parser::ast::jsx::OpeningAttribute::Attribute(attr_inner) => {
                                    match &attr_inner.name {
                                        flow_parser::ast::jsx::attribute::Name::Identifier(id)
                                            if id.name.as_str() == shorthand_prop_name =>
                                        {
                                            self.any_identifier(id.loc.dupe(), &stylex_name);
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        self.jsx_function_call(loc.dupe())?;
        ast_visitor::jsx_element_default(self, loc, element)
    }

    fn jsx_fragment(
        &mut self,
        loc: &ALoc,
        fragment: &flow_parser::ast::jsx::Fragment<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        self.jsx_function_call(loc.dupe())?;
        ast_visitor::jsx_fragment_default(self, loc, fragment)
    }

    fn declare_module(
        &mut self,
        _loc: &ALoc,
        m: &flow_parser::ast::statement::DeclareModule<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_analysis::hoister::Hoister;

        let (block_loc, block) = &m.body;
        let statements = &block.body;
        let mut hoister = Hoister::new(self.enable_enums, true);
        for stmt in statements.iter() {
            let Ok(()) = hoister.statement(stmt);
        }
        let bindings = hoister.into_bindings();
        let saved_exclude_syms = std::mem::take(&mut self.env_state.exclude_syms);
        self.statements_with_bindings(block_loc.dupe(), bindings, statements);
        self.env_state.exclude_syms = saved_exclude_syms;
        Ok(())
    }

    fn declare_namespace(
        &mut self,
        loc: &ALoc,
        m: &flow_parser::ast::statement::DeclareNamespace<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_analysis::hoister::Hoister;
        use flow_parser::ast::statement::Statement;
        use flow_parser::ast::statement::declare_namespace::Id;

        match &m.id {
            Id::Global(_) => {}
            Id::Local(id) => {
                let merged_with_declaration = self.env_read_opt(&id.name).is_some_and(|env_val| {
                    env_val
                        .def_loc
                        .as_ref()
                        .is_some_and(|def_loc| def_loc < &id.loc)
                        && matches!(
                            env_val.kind,
                            BindingsKind::Function
                                | BindingsKind::DeclaredFunction
                                | BindingsKind::Class
                                | BindingsKind::DeclaredClass
                        )
                });
                if !merged_with_declaration {
                    let stmt = Statement::new(StatementInner::DeclareNamespace {
                        loc: loc.dupe(),
                        inner: Arc::new(m.clone()),
                    });
                    if flow_parser::ast_utils::is_type_only_declaration_statement(&stmt) {
                        self.binding_type_identifier(id)?;
                    } else {
                        self.pattern_identifier(Some(flow_parser::ast::VariableKind::Const), id)?;
                    }
                }
            }
        }

        let (block_loc, block) = &m.body;
        let statements = &block.body;
        let mut hoister = Hoister::new(self.enable_enums, true);
        for stmt in statements.iter() {
            let Ok(()) = hoister.statement(stmt);
        }
        let bindings = hoister.into_bindings();
        let saved_exclude_syms = std::mem::take(&mut self.env_state.exclude_syms);
        self.statements_with_bindings(block_loc.dupe(), bindings, statements);
        self.env_state.exclude_syms = saved_exclude_syms;
        Ok(())
    }

    // Skip destructuring patterns in function type params (e.g. in
    // `declare function f({a}: T): R`), as they are not runtime bindings.
    // But still visit the type annotation so type references get registered.
    fn function_param_type_pattern(
        &mut self,
        patt: &'ast flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
    ) -> Result<(), AbruptCompletion> {
        use flow_parser::ast::pattern::Pattern;
        let annot = match patt {
            Pattern::Object { inner, .. } => &inner.annot,
            Pattern::Array { inner, .. } => &inner.annot,
            Pattern::Identifier { inner, .. } => &inner.annot,
            Pattern::Expression { .. } => return Ok(()),
        };
        self.type_annotation_hint(annot)?;
        Ok(())
    }
}

// The EnvBuilder does not traverse dead code, but statement.ml does. Dead code
// is an error in Flow, so type checking after that point is not very meaningful.
// In order to support statement.ml's queries, we must ensure that the value map we
// send to it has the dead code reads filled in. An alternative approach to this visitor
// would be to assume that if the entry does not exist in the map then it is unreachable,
// but that assumes that the EnvBuilder is 100% correct. This approach lets us discriminate
// between real dead code and issues with the EnvBuilder, which seems far better than
// the alternative.
struct DeadCodeMarker<'a, Cx: Context> {
    cx: &'a Cx,
    values: Values<ALoc>,
    write_entries: EnvMap<ALoc, EnvEntry<ALoc>>,
    jsx_base_name: Option<FlowSmolStr>,
}

impl<'a, Cx: Context> DeadCodeMarker<'a, Cx> {
    fn new(
        cx: &'a Cx,
        jsx_base_name: Option<FlowSmolStr>,
        values: Values<ALoc>,
        write_entries: EnvMap<ALoc, EnvEntry<ALoc>>,
    ) -> Self {
        DeadCodeMarker {
            cx,
            values,
            write_entries,
            jsx_base_name,
        }
    }

    fn any_identifier(&mut self, loc: ALoc, name: FlowSmolStr) {
        use env_api::Read;
        use env_api::WriteLoc;

        self.values.entry(loc.dupe()).or_insert_with(|| Read {
            def_loc: None,
            write_locs: vec![WriteLoc::Unreachable(loc)],
            val_kind: env_api::ValKind::Value,
            name: Some(name),
            id: None,
        });
    }

    fn jsx_function_call(&mut self, loc: ALoc) {
        use flow_common::options::JsxMode;
        use flow_common::options::ReactRuntime;
        match (self.cx.react_runtime(), &self.jsx_base_name, self.cx.jsx()) {
            (ReactRuntime::Classic, Some(name), JsxMode::JsxReact) => {
                self.any_identifier(loc, name.dupe());
            }
            (_, Some(name), JsxMode::JsxPragma(_, _)) => {
                self.any_identifier(loc, name.dupe());
            }
            (ReactRuntime::Classic, None, JsxMode::JsxPragma(_, ref ast)) => {
                let Ok(()) = AstVisitor::expression(self, ast);
            }
            _ => {}
        }
    }

    fn mark_dead_write(&mut self, key: env_api::EnvKey<ALoc>) {
        self.write_entries
            .entry(key)
            .or_insert(EnvEntry::NonAssigningWrite);
    }

    fn function_expression_without_name(
        &mut self,
        is_arrow: bool,
        loc: &ALoc,
        expr: &flow_parser::ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), !> {
        scope_builder::function_expression_without_name(
            self,
            true,
            true,
            is_arrow,
            loc,
            expr,
            &|this, f| f(this),
            |this, _fun_loc, _has_this_annot, id| {
                if let Some(id) = id {
                    let Ok(()) = AstVisitor::function_identifier(this, id);
                }
                Ok(())
            },
            |this,
             _enable_enums,
             _with_types,
             is_arrow,
             fun_loc,
             generator_return_loc,
             params,
             return_,
             predicate,
             body| {
                this.lambda(
                    is_arrow,
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

    fn visit_function_or_component_param_pattern(&mut self, loc: ALoc) {
        self.write_entries
            .entry(env_api::EnvKey {
                def_loc_type: env_api::DefLocType::FunctionParamLoc,
                loc,
            })
            .or_insert(EnvEntry::NonAssigningWrite);
    }

    fn lambda(
        &mut self,
        is_arrow: bool,
        _fun_loc: &ALoc,
        generator_return_loc: Option<&ALoc>,
        params: &flow_parser::ast::function::Params<ALoc, ALoc>,
        return_: &flow_parser::ast::function::ReturnAnnot<ALoc, ALoc>,
        predicate: Option<&flow_parser::ast::types::Predicate<ALoc, ALoc>>,
        body: &flow_parser::ast::function::Body<ALoc, ALoc>,
    ) -> Result<(), !> {
        let loc = match body {
            flow_parser::ast::function::Body::BodyBlock((loc, _)) => loc,
            flow_parser::ast::function::Body::BodyExpression(expr) => expr.loc(),
        };
        if let Some(return_loc) = generator_return_loc {
            self.write_entries
                .entry(env_api::EnvKey::new(
                    env_api::DefLocType::OrdinaryNameLoc,
                    return_loc.dupe(),
                ))
                .or_insert(EnvEntry::NonAssigningWrite);
        }
        if self.cx.exhaustive_check(loc).is_none() {
            self.cx.add_exhaustive_check(loc.dupe(), (vec![], false));
        }
        scope_builder::lambda(
            self,
            true,
            true,
            is_arrow,
            loc,
            generator_return_loc,
            params,
            return_,
            predicate,
            body,
        )
    }

    fn run(&mut self, program: &Program<ALoc, ALoc>) {
        let Ok(()) = AstVisitor::program(self, program);
    }

    fn into_values_and_entries(self) -> (Values<ALoc>, EnvMap<ALoc, EnvEntry<ALoc>>) {
        (self.values, self.write_entries)
    }
}

impl<'a, Cx: Context> scope_builder::WithBindings<ALoc, !> for DeadCodeMarker<'a, Cx> {
    fn with_bindings<T>(
        &mut self,
        _lexical: bool,
        _loc: ALoc,
        _bindings: flow_analysis::bindings::Bindings<ALoc>,
        visit: impl FnOnce(&mut Self) -> Result<T, !>,
    ) -> Result<T, !> {
        visit(self)
    }
}

impl<'ast, 'a, Cx: Context> AstVisitor<'ast, ALoc, ALoc, &'ast ALoc, !> for DeadCodeMarker<'a, Cx> {
    fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
        loc
    }

    fn normalize_type(type_: &'ast ALoc) -> &'ast ALoc {
        type_
    }

    // =========================================================================
    // Non-binding identifiers
    // =========================================================================

    fn member_property_identifier(
        &mut self,
        _id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn typeof_member_identifier(
        &mut self,
        _ident: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn member_type_identifier(
        &mut self,
        _id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn pattern_object_property_identifier_key(
        &mut self,
        _kind: Option<flow_parser::ast::VariableKind>,
        _id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn match_object_pattern_property_key(
        &mut self,
        _key: &flow_parser::ast::match_pattern::object_pattern::Key<ALoc, ALoc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn match_object_pattern_property(
        &mut self,
        prop: &flow_parser::ast::match_pattern::object_pattern::Property<ALoc, ALoc>,
    ) -> Result<(), !> {
        use flow_parser::ast::match_pattern::object_pattern::Property;
        match prop {
            Property::Valid { .. } => {
                ast_visitor::match_object_pattern_property_default(self, prop)
            }
            Property::InvalidShorthand { .. } => Ok(()),
        }
    }

    fn enum_member_identifier(
        &mut self,
        _id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn object_key_identifier(
        &mut self,
        _id: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn component_param_name(
        &mut self,
        _param_name: &flow_parser::ast::statement::component_params::ParamName<ALoc, ALoc>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn import_named_specifier(
        &mut self,
        import_kind: flow_parser::ast::statement::ImportKind,
        specifier: &flow_parser::ast::statement::import_declaration::NamedSpecifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        scope_builder::import_named_specifier(self, true, import_kind, specifier)
    }

    // Skip the Identifier form of import_equals_declaration (import X = A.B.C)
    // to avoid creating write entries without corresponding name_def entries.
    fn import_equals_declaration(
        &mut self,
        loc: &ALoc,
        decl: &flow_parser::ast::statement::ImportEqualsDeclaration<ALoc, ALoc>,
    ) -> Result<(), !> {
        use flow_parser::ast::statement::import_equals_declaration::ModuleReference;
        match &decl.module_reference {
            ModuleReference::ExternalModuleReference(..) => {
                flow_parser::ast_visitor::import_equals_declaration_default(self, loc, decl)
            }
            ModuleReference::Identifier(..) => Ok(()),
        }
    }

    fn export_named_declaration_specifier(
        &mut self,
        specifier: &flow_parser::ast::statement::export_named_declaration::ExportSpecifier<
            ALoc,
            ALoc,
        >,
    ) -> Result<(), !> {
        let Ok(()) = self.identifier(&specifier.local);
        Ok(())
    }

    fn this_expression(
        &mut self,
        loc: &ALoc,
        _this_: &flow_parser::ast::expression::This<ALoc>,
    ) -> Result<(), !> {
        self.any_identifier(loc.dupe(), FlowSmolStr::new("this"));
        Ok(())
    }

    fn super_expression(
        &mut self,
        loc: &ALoc,
        _super_: &flow_parser::ast::expression::Super<ALoc>,
    ) -> Result<(), !> {
        self.any_identifier(loc.dupe(), FlowSmolStr::new("super"));
        Ok(())
    }

    fn binding_type_identifier(
        &mut self,
        ident: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        ast_visitor::identifier_default(self, ident)
    }

    fn identifier(&mut self, ident: &flow_parser::ast::Identifier<ALoc, ALoc>) -> Result<(), !> {
        let loc = ident.loc.dupe();
        let name = ident.name.dupe();
        self.any_identifier(loc, name);
        ast_visitor::identifier_default(self, ident)
    }

    fn jsx_element_name_identifier(
        &mut self,
        ident: &flow_parser::ast::jsx::Identifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        let loc = ident.loc.dupe();
        let name = ident.name.dupe();
        self.any_identifier(loc, name);
        ast_visitor::jsx_identifier_default(self, ident)
    }

    fn component_param(
        &mut self,
        param: &flow_parser::ast::statement::component_params::Param<ALoc, ALoc>,
    ) -> Result<(), !> {
        use flow_common::options::JsxMode;
        use flow_common::options::ReactRuntime;
        use flow_parser::ast::statement::component_params::ParamName;

        let name = &param.name;
        match (
            name,
            self.cx.react_runtime(),
            &self.jsx_base_name,
            self.cx.jsx(),
        ) {
            (
                ParamName::Identifier(ident),
                ReactRuntime::Classic,
                Some(base_name),
                JsxMode::JsxReact,
            ) if ident.name.as_str() == "ref" => {
                self.any_identifier(ident.loc.dupe(), base_name.dupe());
            }
            (
                ParamName::StringLiteral((loc, lit)),
                ReactRuntime::Classic,
                Some(base_name),
                JsxMode::JsxReact,
            ) if lit.value.as_str() == "ref" => {
                self.any_identifier(loc.dupe(), base_name.dupe());
            }
            _ => {}
        }
        ast_visitor::component_param_default(self, param)
    }

    fn jsx_element(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::jsx::Element<ALoc, ALoc>,
    ) -> Result<(), !> {
        let closing_element = &expr.closing_element;
        let call_loc = match closing_element {
            None => expr.opening_element.loc.dupe(),
            _ => loc.dupe(),
        };
        self.jsx_function_call(call_loc);
        ast_visitor::jsx_element_default(self, loc, expr)
    }

    fn jsx_fragment(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::jsx::Fragment<ALoc, ALoc>,
    ) -> Result<(), !> {
        self.jsx_function_call(loc.dupe());
        ast_visitor::jsx_fragment_default(self, loc, expr)
    }

    fn pattern_identifier(
        &mut self,
        _kind: Option<flow_parser::ast::VariableKind>,
        ident: &flow_parser::ast::Identifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        let loc = ident.loc.dupe();
        self.write_entries
            .entry(env_api::EnvKey::new(
                env_api::DefLocType::OrdinaryNameLoc,
                loc,
            ))
            .or_insert(EnvEntry::NonAssigningWrite);
        Ok(())
    }

    fn assignment(
        &mut self,
        loc: &ALoc,
        expr: &flow_parser::ast::expression::Assignment<ALoc, ALoc>,
    ) -> Result<(), !> {
        use flow_parser::ast::pattern::Pattern;

        let operator = &expr.operator;
        let (left, _) = ast_utils::unwrap_nonnull_lhs(&expr.left);
        if operator.is_some() {
            if let Pattern::Identifier { inner, .. } = &*left {
                let Ok(()) = self.identifier(&inner.name);
            }
        }
        ast_visitor::assignment_default(self, loc, expr)
    }

    fn update_expression(
        &mut self,
        _loc: &ALoc,
        expr: &flow_parser::ast::expression::Update<ALoc, ALoc>,
    ) -> Result<(), !> {
        use flow_parser::ast::expression::ExpressionInner;

        let argument = &expr.argument;
        match argument.deref() {
            ExpressionInner::Identifier { inner, .. } => {
                let Ok(()) = self.identifier(inner);
                let Ok(()) = self.pattern_identifier(None, inner);
            }
            _ => {
                let Ok(()) = ast_visitor::expression_default(self, argument);
            }
        }
        Ok(())
    }

    fn variable_declarator(
        &mut self,
        kind: flow_parser::ast::VariableKind,
        decl: &flow_parser::ast::statement::variable::Declarator<ALoc, ALoc>,
    ) -> Result<(), !> {
        let init = &decl.init;
        match init {
            Some(_) => ast_visitor::variable_declarator_default(self, kind, decl),
            // When there is no init, we should avoid calls to pattern_identifier so that we won't
            // mark normal declaration without initialization as non-assigning writes.
            None => Ok(()),
        }
    }

    fn yield_(
        &mut self,
        loc: &ALoc,
        yield_: &flow_parser::ast::expression::Yield<ALoc, ALoc>,
    ) -> Result<(), !> {
        self.any_identifier(loc.dupe(), FlowSmolStr::new_inline(NEXT_VAR_NAME));
        ast_visitor::yield_default(self, loc, yield_)
    }

    fn class_(
        &mut self,
        loc: &ALoc,
        cls: &flow_parser::ast::class::Class<ALoc, ALoc>,
    ) -> Result<(), !> {
        if cls.id.is_none() {
            self.mark_dead_write(env_api::EnvKey::new(
                env_api::DefLocType::OrdinaryNameLoc,
                loc.dupe(),
            ));
        }
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::ClassSelfLoc,
            loc.dupe(),
        ));
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::ClassInstanceThisLoc,
            loc.dupe(),
        ));
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::ClassInstanceSuperLoc,
            loc.dupe(),
        ));
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::ClassStaticThisLoc,
            loc.dupe(),
        ));
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::ClassStaticSuperLoc,
            loc.dupe(),
        ));
        ast_visitor::class_default(self, loc, cls)
    }

    fn record_declaration(
        &mut self,
        loc: &ALoc,
        record: &flow_parser::ast::statement::RecordDeclaration<ALoc, ALoc>,
    ) -> Result<(), !> {
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::ClassSelfLoc,
            loc.dupe(),
        ));
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::ClassInstanceThisLoc,
            loc.dupe(),
        ));
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::ClassInstanceSuperLoc,
            loc.dupe(),
        ));
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::ClassStaticThisLoc,
            loc.dupe(),
        ));
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::ClassStaticSuperLoc,
            loc.dupe(),
        ));
        scope_builder::record_declaration(self, true, record, &|this, f| f(this))
    }

    fn function_(
        &mut self,
        loc: &ALoc,
        func: &flow_parser::ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), !> {
        if func.id.is_none() {
            self.mark_dead_write(env_api::EnvKey::new(
                env_api::DefLocType::OrdinaryNameLoc,
                loc.dupe(),
            ));
        }
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::FunctionThisLoc,
            loc.dupe(),
        ));
        self.function_expression_without_name(false, loc, func)
    }

    fn arrow_function(
        &mut self,
        loc: &ALoc,
        func: &flow_parser::ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), !> {
        self.function_expression_without_name(true, loc, func)
    }

    fn function_declaration(
        &mut self,
        loc: &ALoc,
        func: &flow_parser::ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), !> {
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::FunctionThisLoc,
            loc.dupe(),
        ));
        scope_builder::function_declaration(
            self,
            true,
            true,
            loc,
            func,
            &|this, f| f(this),
            |this, _fun_loc, _has_this_annot, id| {
                if let Some(id) = id {
                    let Ok(()) = AstVisitor::function_identifier(this, id);
                }
                Ok(())
            },
            |this,
             _enable_enums,
             _with_types,
             is_arrow,
             fun_loc,
             generator_return_loc,
             params,
             return_,
             predicate,
             body| {
                this.lambda(
                    is_arrow,
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

    fn function_param_pattern(
        &mut self,
        patt: &flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
    ) -> Result<(), !> {
        let loc = patt.loc();
        self.visit_function_or_component_param_pattern(loc.dupe());
        ast_visitor::function_param_pattern_default(self, patt)
    }

    fn component_declaration(
        &mut self,
        loc: &ALoc,
        component: &flow_parser::ast::statement::ComponentDeclaration<ALoc, ALoc>,
    ) -> Result<(), !> {
        self.mark_dead_write(env_api::EnvKey::new(
            env_api::DefLocType::FunctionThisLoc,
            loc.dupe(),
        ));
        scope_builder::component_declaration(
            self,
            true,
            loc,
            component,
            &|this, f| f(this),
            |this, body, params| {
                scope_builder::component_body_with_params(this, true, true, body, params)
            },
        )
    }

    fn declare_component(
        &mut self,
        _loc: &ALoc,
        component: &flow_parser::ast::statement::DeclareComponent<ALoc, ALoc>,
    ) -> Result<(), !> {
        scope_builder::declare_component(self, true, component, &|this, f| f(this))
    }

    fn declare_namespace(
        &mut self,
        loc: &ALoc,
        n: &flow_parser::ast::statement::DeclareNamespace<ALoc, ALoc>,
    ) -> Result<(), !> {
        use flow_parser::ast::statement::Statement;
        use flow_parser::ast::statement::declare_namespace::Id;

        // (match id with
        // | Ast.Statement.DeclareNamespace.Global _ -> ()
        // | Ast.Statement.DeclareNamespace.Local id ->
        //   if Flow_ast_utils.is_type_only_declaration_statement (loc, Ast.Statement.DeclareNamespace m)
        //   then ignore @@ this#binding_type_identifier id
        //   else ignore @@ this#pattern_identifier ~kind:Ast.Variable.Const id);
        match &n.id {
            Id::Global(_) => {}
            Id::Local(id_ident) => {
                let stmt = Statement::new(StatementInner::DeclareNamespace {
                    loc: loc.dupe(),
                    inner: Arc::new(n.clone()),
                });
                if flow_parser::ast_utils::is_type_only_declaration_statement(&stmt) {
                    let Ok(()) = self.binding_type_identifier(id_ident);
                } else {
                    let Ok(()) = self
                        .pattern_identifier(Some(flow_parser::ast::VariableKind::Const), id_ident);
                }
            }
        }
        let body_loc = &n.body.0;
        let body_block = &n.body.1;
        let Ok(()) = self.block(body_loc, body_block);
        Ok(())
    }

    // Skip destructuring patterns in function type params (e.g. in
    // `declare function f({a}: T): R`), as they are not runtime bindings.
    // But still visit the type annotation so type references get registered.
    fn function_param_type_pattern(
        &mut self,
        patt: &'ast flow_parser::ast::pattern::Pattern<ALoc, ALoc>,
    ) -> Result<(), !> {
        use flow_parser::ast::pattern::Pattern;
        let annot = match patt {
            Pattern::Object { inner, .. } => &inner.annot,
            Pattern::Array { inner, .. } => &inner.annot,
            Pattern::Identifier { inner, .. } => &inner.annot,
            Pattern::Expression { .. } => return Ok(()),
        };
        let Ok(()) = self.type_annotation_hint(annot);
        Ok(())
    }
}

// =============================================================================
// Public API
// =============================================================================

use env_api::Values;

#[derive(Clone, Dupe)]
pub struct RefinementOfId {
    refinement_heap: FlowOrdMap<RefinementId, RefinementChain>,
}

impl RefinementOfId {
    pub fn new(refinement_heap: FlowOrdMap<RefinementId, RefinementChain>) -> Self {
        Self { refinement_heap }
    }

    pub fn get(&self, id: i32) -> Refinement<ALoc> {
        let chain = self
            .refinement_heap
            .get(&RefinementId(id as usize))
            .unwrap();
        self.chain_to_refinement(chain)
    }

    fn chain_to_refinement(&self, chain: &RefinementChain) -> Refinement<ALoc> {
        match chain {
            RefinementChain::Base(refinement) => refinement.dupe(),
            RefinementChain::And(id1, id2) => {
                let chain1 = self.refinement_heap.get(&RefinementId(*id1)).unwrap();
                let Refinement {
                    refining_locs: locs1,
                    kind: ref1,
                } = self.chain_to_refinement(chain1);
                let chain2 = self.refinement_heap.get(&RefinementId(*id2)).unwrap();
                let Refinement {
                    refining_locs: locs2,
                    kind: ref2,
                } = self.chain_to_refinement(chain2);
                let mut refining_locs = locs1;
                refining_locs.extend(locs2);
                Refinement {
                    refining_locs,
                    kind: env_api::RefinementKind::AndR(Rc::new(ref1), Rc::new(ref2)),
                }
            }
            RefinementChain::Or(id1, id2) => {
                let chain1 = self.refinement_heap.get(&RefinementId(*id1)).unwrap();
                let Refinement {
                    refining_locs: locs1,
                    kind: ref1,
                } = self.chain_to_refinement(chain1);
                let chain2 = self.refinement_heap.get(&RefinementId(*id2)).unwrap();
                let Refinement {
                    refining_locs: locs2,
                    kind: ref2,
                } = self.chain_to_refinement(chain2);
                let mut refining_locs = locs1;
                refining_locs.extend(locs2);
                Refinement {
                    refining_locs,
                    kind: env_api::RefinementKind::OrR(Rc::new(ref1), Rc::new(ref2)),
                }
            }
            RefinementChain::Not(id1) => {
                let chain1 = self.refinement_heap.get(&RefinementId(*id1)).unwrap();
                let Refinement {
                    refining_locs: locs1,
                    kind: ref1,
                } = self.chain_to_refinement(chain1);
                Refinement {
                    refining_locs: locs1,
                    kind: env_api::RefinementKind::NotR(Rc::new(ref1)),
                }
            }
        }
    }
}

pub struct NameResolverResult {
    pub scopes: flow_analysis::scope_api::ScopeInfo<ALoc>,
    pub env_values: Values<ALoc>,
    pub env_refinement_invalidation_info: FlowOrdMap<ALoc, RefinementInvalidation>,
    pub env_entries: EnvMap<ALoc, EnvEntry<ALoc>>,
    pub providers: Rc<provider_api::Info<ALoc>>,
    pub type_guard_consistency_maps: TypeGuardConsistencyMaps<ALoc>,
    pub refinement_of_id: RefinementOfId,
    pub pred_func_map: FlowOrdMap<ALoc, env_api::PredFuncInfo<ALoc>>,
    pub interface_merge_conflicts: FlowOrdMap<ALoc, Vec<ALoc>>,
}

impl NameResolverResult {
    pub fn to_env_info(self) -> env_api::EnvInfo<ALoc> {
        let refinement_of_id = self.refinement_of_id;
        env_api::EnvInfo {
            scopes: self.scopes,
            env_values: self.env_values,
            env_refinement_invalidation_info: self.env_refinement_invalidation_info,
            env_entries: self.env_entries,
            type_guard_consistency_maps: self.type_guard_consistency_maps,
            providers: self.providers,
            refinement_of_id: Box::new(move |id| refinement_of_id.get(id)),
            pred_func_map: self.pred_func_map,
            interface_merge_conflicts: self.interface_merge_conflicts,
        }
    }
}

pub fn program_with_scope<Cx: Context, Fl: Flow<Cx = Cx>>(
    cx: &Cx,
    is_lib: bool,
    exclude_syms: FlowOrdSet<FlowSmolStr>,
    program: &Program<ALoc, ALoc>,
) -> (Option<AbruptCompletion>, NameResolverResult) {
    use flow_analysis::scope_builder;
    use flow_analysis::ssa_builder;

    let (loc, _) = (program.loc.dupe(), &program.statements);
    let jsx_mode = cx.jsx();
    let jsx_ast = match &jsx_mode {
        JsxMode::JsxReact => None,
        JsxMode::JsxPragma(_, ast) => Some(ast),
    };
    let enable_enums = cx.enable_enums();
    let scopes = scope_builder::program(enable_enums, true, program);
    let (_ssa_completion_state, (ssa_values, unbound_names)) =
        ssa_builder::program_with_scope_and_jsx_pragma(enable_enums, jsx_ast, program);
    let prepass = (&scopes, &ssa_values, &unbound_names);
    let providers = Rc::new(provider_api::find_providers(program));

    let mut env_walk =
        NameResolver::<Cx, Fl>::new(cx, is_lib, exclude_syms, prepass, providers.dupe(), loc);
    let completion_state = env_walk.visit_program(program);
    env_walk.cache.borrow_mut().clear();
    // Fill in dead code reads
    let mut dead_code_marker = DeadCodeMarker::new(
        cx,
        env_walk.jsx_base_name(),
        env_walk.values(),
        env_walk.take_write_entries(),
    );
    dead_code_marker.run(program);
    let (env_values, env_entries) = dead_code_marker.into_values_and_entries();
    let env_refinement_invalidation_info = env_walk.refinement_invalidation_info();
    let type_guard_consistency_maps = env_walk.take_type_guard_consistency_maps();
    let refinement_heap = env_walk.env_state.refinement_heap.borrow().dupe();
    let refinement_of_id = RefinementOfId::new(refinement_heap);
    let pred_func_map = env_walk.pred_func_map();
    let interface_merge_conflicts = env_walk.interface_merge_conflicts();
    (
        completion_state,
        NameResolverResult {
            scopes,
            env_values,
            env_refinement_invalidation_info,
            env_entries,
            providers,
            type_guard_consistency_maps,
            refinement_of_id,
            pred_func_map,
            interface_merge_conflicts,
        },
    )
}

pub fn program<Cx: Context, Fl: Flow<Cx = Cx>>(
    cx: &Cx,
    is_lib: bool,
    exclude_syms: FlowOrdSet<FlowSmolStr>,
    program: &Program<ALoc, ALoc>,
) -> NameResolverResult {
    let (_, result) = program_with_scope::<Cx, Fl>(cx, is_lib, exclude_syms, program);
    result
}
