/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use dupe::Dupe;
use dupe::Dupe_;
use flow_analysis::bindings::Kind as BindingKind;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_data_structure_wrapper::vector::FlowVector;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::types;
use flow_parser::ast_utils;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc_sig::LocSig;

#[derive(Debug, Clone)]
pub struct ImpossibleStateError(pub String);

impl std::fmt::Display for ImpossibleStateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Environment invariant violated: {}", self.0)
    }
}

impl std::error::Error for ImpossibleStateError {}

/// This describes the state of a variable AFTER the provider analysis, suitable for external consumption
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum State {
    AnnotatedVar { contextual: bool },
    InitializedVar,
    ArrayInitializedVar,
    EmptyArrayInitializedVar,
    NullInitializedVar,
    UninitializedVar,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WriteKind {
    EmptyArray,
    Ordinary,
}

/// Describes the state of a variable DURING the provider process, including
/// the scope depths at which initializers were discovered. They will be simplified later on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IntermediateState {
    Annotated {
        contextual: bool,
    },
    /// number of var scope levels deep that the initializer, and null initializer if applicable,
    /// was found from the scope in which it was declared--lets us prioritize initializers from
    /// nearer scopes even if they're lexically later in the program.
    ///
    /// If a variable was initialized to null in one scope, and a non-null value in a deeper scope,
    /// the int option records the depth of the null. This depth should always be <= the depth of
    /// the nonnull initializer, since if we initialize something without null in a parent scope,
    /// an assignment of null in a child scope should not be a provider, even if it's lexically
    /// earlier.
    Initialized(i32, Option<i32>),
    /// For variables that have, so far, only been initialized to null, this records the depth of the assignment
    NullInitialized(i32),
    EmptyArrInitialized,
    ArrInitialized(i32),
    Uninitialized,
}

/// Describes a single assignment/initialization of a variable (rather than the variable's
/// state as a whole). Integers are the number of scopes deeper than the variable's declaration
/// that the assignment occurs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WriteState {
    Annotation { contextual: bool },
    Value(i32),
    ArrayValue(i32),
    Null(i32),
    EmptyArr,
    ArrWrite(i32),
    Nothing,
}

#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq)]
enum ScopeKind {
    Var,
    Lex,
    Polymorphic,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseEntryInner<L: LocSig, Locs: Dupe, S: Copy> {
    pub entry_id: usize,
    pub name: FlowSmolStr,
    pub state: S,
    pub declare_locs: FlowOrdSet<L>,
    pub def_locs: FlowOrdSet<L>,
    pub provider_locs: Locs,
    pub possible_generic_escape_locs: FlowOrdSet<L>,
    pub binding_kind: BindingKind,
}

#[derive(Debug, Clone, Dupe_, PartialEq, Eq)]
pub struct BaseEntry<L: LocSig, Locs: Dupe, S: Copy>(Rc<BaseEntryInner<L, Locs, S>>);

impl<L: LocSig, Locs: Dupe, S: Copy> BaseEntry<L, Locs, S> {
    pub fn new(inner: BaseEntryInner<L, Locs, S>) -> Self {
        BaseEntry(Rc::new(inner))
    }
}

impl<L: LocSig, Locs: Dupe, S: Copy> Deref for BaseEntry<L, Locs, S> {
    type Target = BaseEntryInner<L, Locs, S>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<L: LocSig, Locs: Dupe, S: Copy> Ord for BaseEntry<L, Locs, S>
where
    Self: Eq,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.entry_id.cmp(&other.entry_id)
    }
}

impl<L: LocSig, Locs: Dupe, S: Copy> PartialOrd for BaseEntry<L, Locs, S>
where
    Self: Eq,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

type IntermediateEntry<L> = BaseEntry<L, FlowOrdMap<L, WriteState>, IntermediateState>;

#[derive(Debug, Clone, Dupe, PartialEq, Eq)]
pub struct Providers<L: LocSig> {
    pub writes: FlowOrdMap<L, WriteKind>,
    pub array_writes: FlowOrdSet<L>,
}

impl<L: LocSig> Default for Providers<L> {
    fn default() -> Self {
        Self::new()
    }
}

impl<L: LocSig> Providers<L> {
    pub fn new() -> Self {
        Providers {
            writes: FlowOrdMap::new(),
            array_writes: FlowOrdSet::new(),
        }
    }
}

pub type Entry<L> = BaseEntry<L, Providers<L>, State>;

#[derive(Debug, Clone, Dupe)]
pub(crate) struct Scope<L: LocSig> {
    kind: ScopeKind,
    entries: FlowOrdMap<FlowSmolStr, IntermediateEntry<L>>,
    children: FlowOrdMap<L, Scope<L>>,
}

impl<L: LocSig> Scope<L> {
    fn new(kind: ScopeKind) -> Self {
        Scope {
            kind,
            entries: FlowOrdMap::new(),
            children: FlowOrdMap::new(),
        }
    }
}

#[derive(Debug, Clone, Dupe)]
pub struct Env<L: LocSig>(FlowVector<Scope<L>>);

/// Compute the joined state of a variable, when modified in separate branches
fn combine_states(init1: IntermediateState, init2: IntermediateState) -> IntermediateState {
    match (init1, init2) {
        (
            IntermediateState::Annotated { contextual: c1 },
            IntermediateState::Annotated { contextual: c2 },
        ) => IntermediateState::Annotated {
            contextual: c1 && c2,
        },
        (IntermediateState::Annotated { contextual }, _) => {
            IntermediateState::Annotated { contextual }
        }
        (_, IntermediateState::Annotated { contextual }) => {
            IntermediateState::Annotated { contextual }
        }
        (IntermediateState::Uninitialized, other) | (other, IntermediateState::Uninitialized) => {
            other
        }
        (IntermediateState::ArrInitialized(n), IntermediateState::ArrInitialized(m)) => {
            IntermediateState::ArrInitialized(n.min(m))
        }
        (IntermediateState::ArrInitialized(n), IntermediateState::EmptyArrInitialized)
        | (IntermediateState::EmptyArrInitialized, IntermediateState::ArrInitialized(n)) => {
            IntermediateState::ArrInitialized(n)
        }
        (other, IntermediateState::EmptyArrInitialized | IntermediateState::ArrInitialized(_))
        | (IntermediateState::EmptyArrInitialized | IntermediateState::ArrInitialized(_), other) => {
            other
        }
        (IntermediateState::Initialized(n, i), IntermediateState::Initialized(m, j)) => {
            let p = n.min(m);
            let k = match (i, j) {
                (Some(k), None) | (None, Some(k)) if k <= p => Some(k),
                (Some(i), Some(j)) if i <= p || j <= p => Some(i.min(j)),
                _ => None,
            };
            IntermediateState::Initialized(p, k)
        }
        (IntermediateState::Initialized(n, i), IntermediateState::NullInitialized(j))
        | (IntermediateState::NullInitialized(j), IntermediateState::Initialized(n, i)) => {
            let k = i.map(|i| j.min(i)).unwrap_or(j);
            if k <= n {
                IntermediateState::Initialized(n, Some(k))
            } else {
                IntermediateState::Initialized(n, None)
            }
        }
        (IntermediateState::NullInitialized(n), IntermediateState::NullInitialized(m)) => {
            IntermediateState::NullInitialized(n.min(m))
        }
    }
}

/// This function decides if an incoming write to a variable can possibly be a provider.
/// If this function returns None, then the incoming write described by `write_state` cannot
/// possibly be a provider (e.g. an assignment to an already `Initialized` variable). If it
/// returns Some new state, then the write could be a provider, and the resulting state is
/// the new state of the overall variable after including the write.
fn extended_state_opt(
    write_state: &WriteState,
    state: &IntermediateState,
) -> Option<IntermediateState> {
    match (state, write_state) {
        // var x;
        (_, WriteState::Nothing) => None,

        // var x: string; var x: number; provider is string
        (IntermediateState::Annotated { .. }, _) => None,

        // var x = 42; var x: string, provider is string
        (_, WriteState::Annotation { contextual }) => Some(IntermediateState::Annotated {
            contextual: *contextual,
        }),

        // var x; x = null, provider is null
        (IntermediateState::Uninitialized, WriteState::Null(d)) => {
            Some(IntermediateState::NullInitialized(*d))
        }

        // var x; x = 42, provider is 42
        (IntermediateState::Uninitialized, WriteState::Value(d) | WriteState::ArrayValue(d)) => {
            Some(IntermediateState::Initialized(*d, None))
        }

        // var x; var x = [], [] provider
        (IntermediateState::Uninitialized, WriteState::EmptyArr) => {
            Some(IntermediateState::EmptyArrInitialized)
        }

        // var x; x.push(42) no provider
        (IntermediateState::Uninitialized, WriteState::ArrWrite(_)) => None,

        // var x = null; x = 42; provider is null and 42
        (
            IntermediateState::NullInitialized(n),
            WriteState::Value(d) | WriteState::ArrayValue(d),
        ) if *n <= *d => Some(IntermediateState::Initialized(*d, Some(*n))),

        // var x; (function() { x = null }); x = 42; provider is 42
        (
            IntermediateState::NullInitialized(_),
            WriteState::Value(d) | WriteState::ArrayValue(d),
        ) => Some(IntermediateState::Initialized(*d, None)),

        // var x = null; x = null, provider is first null
        (IntermediateState::NullInitialized(n), WriteState::Null(d)) if *n <= *d => None,

        // var x; (function() { x = null }); x = null, provider is second null
        (IntermediateState::NullInitialized(_), WriteState::Null(d)) => {
            Some(IntermediateState::NullInitialized(*d))
        }

        // var x = null; var x = [], [] provider
        (IntermediateState::NullInitialized(_), WriteState::EmptyArr) => {
            Some(IntermediateState::EmptyArrInitialized)
        }

        // var x = null; x.push(42), null provider
        (IntermediateState::NullInitialized(_), WriteState::ArrWrite(_)) => None,

        // var x = null; x = 42; x = null, providers are first null and 42
        (IntermediateState::Initialized(_, Some(m)), WriteState::Null(d)) if *m <= *d => None,

        // var x; (function () { x = null; x = 42; }); x = null
        (IntermediateState::Initialized(n, Some(_)), WriteState::Null(d)) => {
            Some(IntermediateState::Initialized(*n, Some(*d)))
        }

        // var x = 42; x = null, provider is 42
        (IntermediateState::Initialized(n, None), WriteState::Null(d)) if *n <= *d => None,

        // var x; (function () { x = 42; }); x = null
        (IntermediateState::Initialized(n, None), WriteState::Null(d)) => {
            Some(IntermediateState::Initialized(*n, Some(*d)))
        }

        // var x = 42; x = "a", provider is 42
        (
            IntermediateState::Initialized(n, _),
            WriteState::Value(d) | WriteState::ArrayValue(d),
        ) if *n <= *d => None,

        // var x = null; (function () { x = 42; }); x = "a"
        (
            IntermediateState::Initialized(_, Some(m)),
            WriteState::Value(d) | WriteState::ArrayValue(d),
        ) if *m <= *d => Some(IntermediateState::Initialized(*d, Some(*m))),

        // var x; (function () { x = null; x = 42; }); x = "a"
        (
            IntermediateState::Initialized(_, _),
            WriteState::Value(d) | WriteState::ArrayValue(d),
        ) => Some(IntermediateState::Initialized(*d, None)),

        (IntermediateState::Initialized(_, _), WriteState::EmptyArr) => None,
        (IntermediateState::Initialized(_, _), WriteState::ArrWrite(_)) => None,

        (IntermediateState::EmptyArrInitialized, WriteState::Null(_) | WriteState::EmptyArr) => {
            None
        }
        (IntermediateState::EmptyArrInitialized, WriteState::Value(_)) => None,
        (IntermediateState::EmptyArrInitialized, WriteState::ArrayValue(n)) => {
            Some(IntermediateState::ArrInitialized(*n))
        }
        (IntermediateState::EmptyArrInitialized, WriteState::ArrWrite(n)) => {
            Some(IntermediateState::ArrInitialized(*n))
        }

        (IntermediateState::ArrInitialized(_), WriteState::Null(_) | WriteState::EmptyArr) => None,
        (IntermediateState::ArrInitialized(_), WriteState::Value(_)) => None,
        (IntermediateState::ArrInitialized(n), WriteState::ArrayValue(d)) if *n <= *d => None,
        (IntermediateState::ArrInitialized(_), WriteState::ArrayValue(d)) => {
            Some(IntermediateState::ArrInitialized(*d))
        }
        (IntermediateState::ArrInitialized(n), WriteState::ArrWrite(d)) if *n <= *d => None,
        (IntermediateState::ArrInitialized(_), WriteState::ArrWrite(d)) => {
            Some(IntermediateState::ArrInitialized(*d))
        }
    }
}

fn empty_entry<L: LocSig>(name: FlowSmolStr, binding_kind: BindingKind) -> IntermediateEntry<L> {
    static ENTRY_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);
    BaseEntry::new(BaseEntryInner {
        entry_id: ENTRY_ID_COUNTER.fetch_add(1, Ordering::Relaxed),
        name,
        state: IntermediateState::Uninitialized,
        provider_locs: FlowOrdMap::new(),
        declare_locs: FlowOrdSet::new(),
        def_locs: FlowOrdSet::new(),
        possible_generic_escape_locs: FlowOrdSet::new(),
        binding_kind,
    })
}

fn env_invariant_violated(s: &str) -> ! {
    panic!("Environment invariant violated: {}", s)
}

/// Finds the right set of entries to add a new variable to in the scope chain
/// based on whether it's a let, const, or var.
/// Returns the entries and an index to update the environment.
fn find_entries_for_new_variable<L: LocSig>(
    kind: ast::VariableKind,
    env: &Env<L>,
) -> Option<(&FlowOrdMap<FlowSmolStr, IntermediateEntry<L>>, usize)> {
    for (i, scope) in env.0.iter().enumerate() {
        match (scope.kind, kind) {
            (ScopeKind::Lex, ast::VariableKind::Let | ast::VariableKind::Const)
            | (ScopeKind::Var | ScopeKind::Polymorphic, _) => {
                return Some((&scope.entries, i));
            }
            _ => continue,
        }
    }
    None
}

/// Similar to the above function, but rather than finding the set of entries and a replacement function for it
/// based on where a new variable would live, this finds the set of entries in which some particular variable
/// already exists.
fn find_entry_for_existing_variable<L: LocSig>(
    var: &FlowSmolStr,
    env: &Env<L>,
) -> Option<(IntermediateEntry<L>, i32, Option<usize>, usize)> {
    let mut var_scopes_off = 0i32;

    for (i, scope) in env.0.iter().enumerate() {
        if let Some(entry) = scope.entries.get(var) {
            return Some((entry.dupe(), var_scopes_off, Some(i), i));
        }

        if i == env.0.len() - 1 {
            if scope.kind == ScopeKind::Var {
                return Some((
                    empty_entry(var.dupe(), BindingKind::Var),
                    var_scopes_off,
                    None,
                    i,
                ));
            } else {
                env_invariant_violated("Root environment should always be in Var");
            }
        }

        if scope.kind == ScopeKind::Var {
            var_scopes_off += 1;
        }
    }

    None
}

fn state_of_var<L: LocSig>(var: &FlowSmolStr, env: &Env<L>) -> Option<IntermediateState> {
    for (i, scope) in env.0.iter().enumerate() {
        if let Some(entry) = scope.entries.get(var) {
            return Some(entry.state);
        }

        if i == env.0.len() - 1 {
            if scope.kind == ScopeKind::Var {
                return None;
            } else {
                env_invariant_violated("Root environment should always be Var");
            }
        }
    }
    None
}

fn get_entry<L: LocSig>(
    var: &FlowSmolStr,
    default_binding: BindingKind,
    entries: &FlowOrdMap<FlowSmolStr, IntermediateEntry<L>>,
) -> IntermediateEntry<L> {
    if let Some(entry) = entries.get(var) {
        entry.dupe()
    } else {
        empty_entry(var.dupe(), default_binding)
    }
}

fn join_envs<L: LocSig>(env1: Env<L>, env2: Env<L>) -> Env<L> {
    fn join_entries<L: LocSig>(
        entry1: IntermediateEntry<L>,
        entry2: IntermediateEntry<L>,
    ) -> IntermediateEntry<L> {
        debug_assert_eq!(entry1.name, entry2.name);

        if entry1.entry_id == entry2.entry_id
            && entry1.state == entry2.state
            && entry1.provider_locs.ptr_eq(&entry2.provider_locs)
            && entry1.declare_locs.ptr_eq(&entry2.declare_locs)
            && entry1.def_locs.ptr_eq(&entry2.def_locs)
            && entry1
                .possible_generic_escape_locs
                .ptr_eq(&entry2.possible_generic_escape_locs)
        {
            return entry1;
        }

        let binding_kind = if entry1.binding_kind == entry2.binding_kind {
            entry1.binding_kind
        } else {
            match (&entry1.binding_kind, &entry2.binding_kind) {
                (BindingKind::DeclaredFunction, _) | (_, BindingKind::DeclaredFunction) => {
                    BindingKind::DeclaredFunction
                }
                _ => BindingKind::Var,
            }
        };

        let mut provider_locs = entry1.provider_locs.dupe();
        for (loc, state) in entry2.provider_locs.iter() {
            if let Some(existing) = provider_locs.get(loc) {
                if existing != state {}
            } else {
                provider_locs.insert(loc.dupe(), *state);
            }
        }

        BaseEntry::new(BaseEntryInner {
            entry_id: entry1.entry_id,
            name: entry1.name.dupe(),
            state: combine_states(entry1.state, entry2.state),
            declare_locs: entry1.declare_locs.dupe().union(entry2.declare_locs.dupe()),
            def_locs: entry1.def_locs.dupe().union(entry2.def_locs.dupe()),
            provider_locs,
            possible_generic_escape_locs: entry1
                .possible_generic_escape_locs
                .dupe()
                .union(entry2.possible_generic_escape_locs.dupe()),
            binding_kind,
        })
    }

    fn join_scopes<L: LocSig>(scope1: Scope<L>, scope2: Scope<L>) -> Scope<L> {
        debug_assert_eq!(scope1.kind, scope2.kind);

        if scope1.entries.ptr_eq(&scope2.entries) && scope1.children.ptr_eq(&scope2.children) {
            return scope1;
        }

        let mut entries = scope1.entries;
        for (name, entry2) in scope2.entries {
            if let Some(entry1) = entries.remove(&name) {
                entries.insert(name, join_entries(entry1, entry2));
            } else {
                entries.insert(name, entry2);
            }
        }

        let mut children = scope1.children;
        for (loc, child2) in scope2.children {
            if let Some(child1) = children.remove(&loc) {
                children.insert(loc, join_scopes(child1, child2));
            } else {
                children.insert(loc, child2);
            }
        }

        Scope {
            kind: scope1.kind,
            entries,
            children,
        }
    }

    debug_assert_eq!(env1.0.len(), env2.0.len());

    let all_equal = env1.0.iter().zip(env2.0.iter()).all(|(s1, s2)| {
        s1.kind == s2.kind && s1.entries.ptr_eq(&s2.entries) && s1.children.ptr_eq(&s2.children)
    });
    if all_equal {
        return env1;
    }

    Env(env1
        .0
        .into_iter()
        .zip(env2.0)
        .map(|(s1, s2)| join_scopes(s1, s2))
        .collect())
}

fn exit_lex_child<L: LocSig>(loc: L, env: Env<L>) -> Env<L> {
    if env.0.len() < 2 {
        env_invariant_violated("Popping to empty stack");
    }

    let mut inner = env.0;
    let child = inner
        .pop_front()
        .expect("exit_lex_child: env has at least 2 elements");
    inner
        .front_mut()
        .expect("exit_lex_child: rest has at least 1 element")
        .children
        .insert(loc, child);
    Env(inner)
}

// Root visitor. Uses the `enter_lex_child` function parameter to manipulate the environment when it dives into a scope,
// and calls `exit_lex_child` when it leaves a scope. For branching statements -- If, Try, and Switch -- it uses the same
// initial env when it explores each branch, and then joins them using `join_envs`. It also has a "context" `cx type, which
// is not accumulated (unlike the env) but stores contextual information using `in_context`.
trait FinderBehavior<L: LocSig>: for<'a> AstVisitor<'a, L> {
    type Cx: Copy;

    fn acc(&self) -> (&Env<L>, &Self::Cx);

    fn set_acc(&mut self, env: Env<L>, cx: Self::Cx);

    fn enter_lex_child(&self, kind: ScopeKind, loc: L, env: Env<L>) -> Env<L>;

    fn accumulate_branch_env<F>(
        &mut self,
        env0: Env<L>,
        cx: Self::Cx,
        visit_branch: F,
        acc_env: Env<L>,
    ) -> Env<L>
    where
        F: FnOnce(&mut Self),
    {
        self.set_acc(env0, cx);
        visit_branch(self);
        let (env_prime, _) = self.acc();
        join_envs(env_prime.dupe(), acc_env)
    }

    fn in_context<R, F>(&mut self, mod_cx: Option<&dyn Fn(&Self::Cx) -> Self::Cx>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let (env, cx) = self.acc();
        let env = env.dupe();
        let cx = *cx;
        if let Some(modifier) = mod_cx {
            self.set_acc(env.dupe(), modifier(&cx));
        }
        let res = f(self);
        let (env_after, _) = self.acc();
        let env_after = env_after.dupe();
        self.set_acc(env_after, cx);
        res
    }

    fn enter_scope<R, F>(&mut self, kind: ScopeKind, loc: &L, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let (env, cx) = self.acc();
        let env = env.dupe();
        let cx = *cx;
        let env_prime = self.enter_lex_child(kind, loc.dupe(), env);
        self.set_acc(env_prime, cx);
        let res = self.in_context(None, f);
        let (env_prime, cx) = self.acc();
        let env_prime = env_prime.dupe();
        let cx = *cx;
        self.set_acc(exit_lex_child(loc.dupe(), env_prime), cx);
        res
    }

    fn enter_possibly_polymorphic_scope<R, F>(
        &mut self,
        is_polymorphic: bool,
        kind: ScopeKind,
        loc: &L,
        f: F,
    ) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        if is_polymorphic {
            self.enter_scope(ScopeKind::Polymorphic, loc, |finder| {
                finder.enter_scope(kind, loc, f)
            })
        } else {
            self.enter_scope(kind, loc, f)
        }
    }
}

fn finder_block<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    stmt: &ast::statement::Block<L, L>,
) {
    this.enter_scope(ScopeKind::Lex, loc, |inner| {
        for s in stmt.body.iter() {
            let Ok(()) = inner.statement(s);
        }
    });
}

fn finder_catch_clause<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    clause: &ast::statement::try_::CatchClause<L, L>,
) {
    this.enter_scope(ScopeKind::Lex, &clause.loc, |inner| {
        if let Some(param) = &clause.param {
            let Ok(()) = inner.catch_clause_pattern(param);
        }
        for s in clause.body.1.body.iter() {
            let Ok(()) = inner.statement(s);
        }
    });
}

fn finder_do_while<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    stmt: &ast::statement::DoWhile<L, L>,
) {
    this.enter_scope(ScopeKind::Lex, loc, |inner| {
        let Ok(()) = ast_visitor::do_while_default(inner, loc, stmt);
    });
}

fn finder_for_statement<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    stmt: &ast::statement::For<L, L>,
) {
    this.enter_scope(ScopeKind::Lex, loc, |inner| {
        let Ok(()) = ast_visitor::for_statement_default(inner, loc, stmt);
    });
}

fn finder_for_in_statement<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    stmt: &ast::statement::ForIn<L, L>,
) {
    this.enter_scope(ScopeKind::Lex, loc, |inner| {
        let Ok(()) = ast_visitor::for_in_statement_default(inner, loc, stmt);
    });
}

fn finder_for_of_statement<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    stmt: &ast::statement::ForOf<L, L>,
) {
    this.enter_scope(ScopeKind::Lex, loc, |inner| {
        let Ok(()) = ast_visitor::for_of_statement_default(inner, loc, stmt);
    });
}

fn finder_while<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    stmt: &ast::statement::While<L, L>,
) {
    this.enter_scope(ScopeKind::Lex, loc, |inner| {
        let Ok(()) = ast_visitor::while_default(inner, loc, stmt);
    });
}

fn finder_with<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    stmt: &ast::statement::With<L, L>,
) {
    this.enter_scope(ScopeKind::Lex, loc, |inner| {
        let Ok(()) = ast_visitor::with_default(inner, loc, stmt);
    });
}

fn finder_class_body<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    cls_body: &ast::class::Body<L, L>,
) {
    let loc = cls_body.loc.dupe();
    this.enter_scope(ScopeKind::Var, &loc, |inner| {
        let Ok(()) = ast_visitor::class_body_default(inner, cls_body);
    });
}

fn finder_class_expression<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    cls: &ast::class::Class<L, L>,
) {
    let is_polymorphic = cls.tparams.is_some();
    this.enter_possibly_polymorphic_scope(is_polymorphic, ScopeKind::Lex, loc, |inner| {
        let Ok(()) = ast_visitor::class_expression_default(inner, loc, cls);
    });
}

fn finder_class<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    cls: &ast::class::Class<L, L>,
) {
    let is_polymorphic = cls.tparams.is_some();
    this.enter_possibly_polymorphic_scope(is_polymorphic, ScopeKind::Lex, loc, |inner| {
        let Ok(()) = ast_visitor::class_default(inner, loc, cls);
    });
}

fn finder_arrow_function<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    func: &ast::function::Function<L, L>,
) {
    let is_polymorphic = func.tparams.is_some();
    this.enter_possibly_polymorphic_scope(is_polymorphic, ScopeKind::Var, loc, |inner| {
        let Ok(()) = ast_visitor::arrow_function_default(inner, loc, func);
    });
}

fn finder_function_expression_or_method<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    func: &ast::function::Function<L, L>,
) {
    let is_polymorphic = func.tparams.is_some();
    this.enter_possibly_polymorphic_scope(is_polymorphic, ScopeKind::Var, loc, |inner| {
        let Ok(()) = ast_visitor::function_expression_or_method_default(inner, loc, func);
    });
}

fn finder_function_declaration<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    func: &ast::function::Function<L, L>,
) {
    if let Some(id) = &func.id {
        let Ok(()) = this.function_identifier(id);
    }
    let is_polymorphic = func.tparams.is_some();
    this.enter_possibly_polymorphic_scope(is_polymorphic, ScopeKind::Var, loc, |inner| {
        let Ok(()) = inner.function_params(&func.params);
        let Ok(()) = ast_visitor::function_return_annotation_default(inner, &func.return_);
        let Ok(()) = ast_visitor::function_body_any_default(inner, &func.body);
        if let Some(pred) = &func.predicate {
            let Ok(()) = ast_visitor::predicate_default(inner, pred);
        }
        if let Some(tparams) = &func.tparams {
            let Ok(()) = ast_visitor::type_params_default(
                inner,
                &ast_visitor::TypeParamsContext::Function,
                tparams,
            );
        }
    });
}

fn finder_component_declaration<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    comp: &ast::statement::ComponentDeclaration<L, L>,
) {
    let Ok(()) = this.component_identifier(&comp.id);
    let is_polymorphic = comp.tparams.is_some();
    this.enter_possibly_polymorphic_scope(is_polymorphic, ScopeKind::Var, loc, |inner| {
        if let Some(tparams) = &comp.tparams {
            let Ok(()) = ast_visitor::type_params_default(
                inner,
                &ast_visitor::TypeParamsContext::ComponentDeclaration,
                tparams,
            );
        }
        let Ok(()) = ast_visitor::component_params_default(inner, &comp.params);
        if let Some(body) = &comp.body {
            let Ok(()) = ast_visitor::component_body_default(inner, body);
        }
        let Ok(()) = ast_visitor::component_renders_annotation_default(inner, &comp.renders);
    });
}

fn finder_declare_module<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    module: &ast::statement::DeclareModule<L, L>,
) {
    this.enter_scope(ScopeKind::Var, loc, |inner| {
        let Ok(()) = ast_visitor::declare_module_default(inner, loc, module);
    });
}

fn finder_if_statement<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    stmt: &ast::statement::If<L, L>,
) {
    let Ok(()) = this.expression(&stmt.test);
    let (env0, cx) = this.acc();
    let env0 = env0.dupe();
    let cx = *cx;
    let Ok(()) = this.if_consequent_statement(stmt.alternate.is_some(), &stmt.consequent);
    let (env1, _) = this.acc();
    let env1 = env1.dupe();
    let env2 = this.accumulate_branch_env(
        env0.dupe(),
        cx,
        |inner| {
            if let Some(alternate) = &stmt.alternate {
                let Ok(()) = inner.if_alternate_statement(alternate);
            }
        },
        env1,
    );
    this.set_acc(env2, cx);
}

fn finder_switch<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    switch: &ast::statement::Switch<L, L>,
) {
    let Ok(()) = this.expression(&switch.discriminant);
    this.enter_scope(ScopeKind::Lex, loc, |inner| {
        let (env0, cx) = inner.acc();
        let env0 = env0.dupe();
        let cx = *cx;
        let mut acc_env = env0.dupe();
        for case in switch.cases.iter() {
            acc_env = inner.accumulate_branch_env(
                env0.dupe(),
                cx,
                |inner2| {
                    let Ok(()) = ast_visitor::switch_case_default(inner2, case);
                },
                acc_env,
            );
        }
        inner.set_acc(acc_env, cx);
    });
}

fn finder_try_catch<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    try_stmt: &ast::statement::Try<L, L>,
) {
    let (env0, cx) = this.acc();
    let env0 = env0.dupe();
    let cx = *cx;
    let Ok(()) = this.block(&try_stmt.block.0, &try_stmt.block.1);
    let (env1, _) = this.acc();
    let env1 = env1.dupe();
    let env2 = if let Some(h) = &try_stmt.handler {
        this.accumulate_branch_env(
            env0.dupe(),
            cx,
            |inner| {
                let Ok(()) = inner.catch_clause(h);
            },
            env1,
        )
    } else {
        env1
    };
    let env3 = if let Some((finalizer_loc, f)) = &try_stmt.finalizer {
        this.accumulate_branch_env(
            env0,
            cx,
            |inner| {
                let Ok(()) = inner.block(finalizer_loc, f);
            },
            env2,
        )
    } else {
        env2
    };
    this.set_acc(env3, cx);
}

fn finder_update_expression<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    expr: &ast::expression::Update<L, L>,
) {
    match expr.argument.deref() {
        ExpressionInner::Identifier { inner, .. } => {
            let Ok(()) = this.identifier(inner);
            let Ok(()) = this.pattern_identifier(None, inner);
        }
        _ => {
            let Ok(()) = this.expression(&expr.argument);
        }
    }
}

/// Unified helper for match expressions and statements
fn finder_match<L: LocSig, T: FinderBehavior<L> + for<'a> AstVisitor<'a, L>, B>(
    this: &mut T,
    loc: &L,
    arg: &ast::expression::Expression<L, L>,
    match_keyword_loc: L,
    cases: &[ast::match_::Case<L, L, B>],
    mut on_case_body: impl FnMut(&mut T, &B),
) {
    let Ok(()) = this.expression(arg);
    this.enter_scope(ScopeKind::Lex, loc, |inner| {
        let id = ast_utils::match_root_ident::<L, L>(match_keyword_loc);
        let Ok(()) = inner.pattern_identifier(Some(ast::VariableKind::Const), &id);
        match cases {
            [] => {}
            [first_case, rest_cases @ ..] => {
                let (env0, cx) = inner.acc();
                let env0 = env0.dupe();
                let cx = *cx;
                let Ok(()) = finder_match_case(inner, first_case, &mut |visitor, body| {
                    on_case_body(visitor, body);
                    Ok(())
                });
                let (env1, _) = inner.acc();
                let env2 = rest_cases.iter().fold(env1.dupe(), |env_acc, case| {
                    inner.accumulate_branch_env(
                        env0.dupe(),
                        cx,
                        |i| {
                            let Ok(()) = finder_match_case(i, case, &mut |visitor, body| {
                                on_case_body(visitor, body);
                                Ok(())
                            });
                        },
                        env_acc,
                    )
                });
                inner.set_acc(env2, cx);
            }
        }
    });
}

fn finder_match_case<'ast, L: LocSig, T: FinderBehavior<L>, B>(
    this: &mut T,
    case: &'ast ast::match_::Case<L, L, B>,
    on_case_body: &mut impl FnMut(&mut T, &'ast B) -> Result<(), !>,
) -> Result<(), !> {
    this.enter_scope(ScopeKind::Lex, &case.loc, |inner| {
        let Ok(()) = ast_visitor::match_case_default(inner, case, on_case_body);
    });
    Ok(())
}

fn finder_record_declaration<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    loc: &L,
    record: &ast::statement::RecordDeclaration<L, L>,
) {
    let is_polymorphic = record.tparams.is_some();
    this.enter_possibly_polymorphic_scope(is_polymorphic, ScopeKind::Lex, loc, |inner| {
        let Ok(()) = ast_visitor::record_declaration_default(inner, loc, record);
    });
}

fn finder_record_body<L: LocSig, T: FinderBehavior<L>>(
    this: &mut T,
    body: &ast::statement::record_declaration::Body<L, L>,
) {
    this.enter_scope(ScopeKind::Var, &body.loc, |inner| {
        let Ok(()) = ast_visitor::record_declaration_body_default(inner, body);
    });
}

pub fn empty_env<L: LocSig>() -> Env<L> {
    Env(FlowVector::unit(Scope::new(ScopeKind::Var)))
}

fn enter_new_lex_child<L: LocSig>(kind: ScopeKind, _loc: L, env: Env<L>) -> Env<L> {
    let mut inner = env.0;
    inner.push_front(Scope::new(kind));
    Env(inner)
}

struct FindDeclarations<L: LocSig> {
    env: Env<L>,
    cx: FindDeclarationsContext,
}

impl<L: LocSig> FindDeclarations<L> {
    fn new(env: Env<L>) -> Self {
        Self {
            env,
            cx: FindDeclarationsContext::default(),
        }
    }

    fn new_entry(
        &mut self,
        var: FlowSmolStr,
        binding_kind: BindingKind,
        kind: ast::VariableKind,
        loc: L,
    ) {
        let write_state = self.cx.init_state;
        if let Some((entries, entries_idx)) = find_entries_for_new_variable(kind, &self.env) {
            let entry = get_entry(&var, binding_kind, entries);
            let declare_locs: FlowOrdSet<L> = entry.declare_locs.dupe().update(loc.dupe()).into();

            let (state, provider_locs): (IntermediateState, FlowOrdMap<L, WriteState>) =
                match (binding_kind, entry.binding_kind) {
                    (BindingKind::DeclaredFunction, BindingKind::DeclaredFunction) => (
                        IntermediateState::Annotated { contextual: false },
                        entry
                            .provider_locs
                            .dupe()
                            .update(loc, WriteState::Annotation { contextual: false })
                            .into(),
                    ),
                    (_, BindingKind::DeclaredFunction) => (entry.state, entry.provider_locs.dupe()),
                    _ => {
                        if let Some(new_state) = extended_state_opt(&write_state, &entry.state) {
                            (
                                new_state,
                                entry.provider_locs.dupe().update(loc, write_state).into(),
                            )
                        } else {
                            (entry.state, entry.provider_locs.dupe())
                        }
                    }
                };

            let new_entry = BaseEntry::new(BaseEntryInner {
                entry_id: entry.entry_id,
                name: entry.name.dupe(),
                state,
                declare_locs,
                def_locs: entry.def_locs.dupe(),
                provider_locs,
                possible_generic_escape_locs: entry.possible_generic_escape_locs.dupe(),
                binding_kind: entry.binding_kind,
            });

            let scope = &mut self.env.0[entries_idx];
            scope.entries.insert(var, new_entry);
        }
    }
}

/// Context for the find_declarations visitor.
#[derive(Debug, Clone, Copy)]
struct FindDeclarationsContext {
    init_state: WriteState,
}

impl Default for FindDeclarationsContext {
    fn default() -> Self {
        Self {
            init_state: WriteState::Value(0),
        }
    }
}

impl<L: LocSig> FinderBehavior<L> for FindDeclarations<L> {
    type Cx = FindDeclarationsContext;

    fn acc(&self) -> (&Env<L>, &Self::Cx) {
        (&self.env, &self.cx)
    }

    fn set_acc(&mut self, env: Env<L>, cx: Self::Cx) {
        self.env = env;
        self.cx = cx;
    }

    fn enter_lex_child(&self, kind: ScopeKind, _loc: L, env: Env<L>) -> Env<L> {
        enter_new_lex_child(kind, _loc, env)
    }
}

impl<'ast, L: LocSig> AstVisitor<'ast, L> for FindDeclarations<L> {
    fn normalize_loc(loc: &'ast L) -> &'ast L {
        loc
    }

    fn normalize_type(type_: &'ast L) -> &'ast L {
        type_
    }

    fn block(&mut self, loc: &L, stmt: &ast::statement::Block<L, L>) -> Result<(), !> {
        finder_block(self, loc, stmt);
        Ok(())
    }

    fn catch_clause(&mut self, clause: &ast::statement::try_::CatchClause<L, L>) -> Result<(), !> {
        finder_catch_clause(self, clause);
        Ok(())
    }

    fn do_while(&mut self, loc: &L, stmt: &ast::statement::DoWhile<L, L>) -> Result<(), !> {
        finder_do_while(self, loc, stmt);
        Ok(())
    }

    fn for_statement(&mut self, loc: &L, stmt: &ast::statement::For<L, L>) -> Result<(), !> {
        finder_for_statement(self, loc, stmt);
        Ok(())
    }

    fn for_in_statement(&mut self, loc: &L, stmt: &ast::statement::ForIn<L, L>) -> Result<(), !> {
        finder_for_in_statement(self, loc, stmt);
        Ok(())
    }

    fn for_of_statement(&mut self, loc: &L, stmt: &ast::statement::ForOf<L, L>) -> Result<(), !> {
        finder_for_of_statement(self, loc, stmt);
        Ok(())
    }

    fn while_(&mut self, loc: &L, stmt: &ast::statement::While<L, L>) -> Result<(), !> {
        finder_while(self, loc, stmt);
        Ok(())
    }

    fn with_(&mut self, loc: &L, stmt: &ast::statement::With<L, L>) -> Result<(), !> {
        finder_with(self, loc, stmt);
        Ok(())
    }

    fn class_body(&mut self, cls_body: &ast::class::Body<L, L>) -> Result<(), !> {
        finder_class_body(self, cls_body);
        Ok(())
    }

    fn class_expression(&mut self, loc: &L, cls: &ast::class::Class<L, L>) -> Result<(), !> {
        finder_class_expression(self, loc, cls);
        Ok(())
    }

    fn class_(&mut self, loc: &L, cls: &ast::class::Class<L, L>) -> Result<(), !> {
        finder_class(self, loc, cls);
        Ok(())
    }

    fn arrow_function(&mut self, loc: &L, func: &ast::function::Function<L, L>) -> Result<(), !> {
        finder_arrow_function(self, loc, func);
        Ok(())
    }

    fn function_expression_or_method(
        &mut self,
        loc: &L,
        func: &ast::function::Function<L, L>,
    ) -> Result<(), !> {
        finder_function_expression_or_method(self, loc, func);
        Ok(())
    }

    fn function_(&mut self, _loc: &L, func: &ast::function::Function<L, L>) -> Result<(), !> {
        let init_state = match &func.return_ {
            ast::function::ReturnAnnot::Available(_) => {
                WriteState::Annotation { contextual: false }
            }
            _ => WriteState::Value(0),
        };
        if let Some(id) = &func.id {
            self.in_context(
                Some(&|_cx| FindDeclarationsContext { init_state }),
                |inner| {
                    let Ok(()) = inner.function_identifier(id);
                },
            );
        }
        let Ok(()) = self.function_params(&func.params);
        let Ok(()) = ast_visitor::function_return_annotation_default(self, &func.return_);
        let Ok(()) = ast_visitor::function_body_any_default(self, &func.body);
        if let Some(pred) = &func.predicate {
            let Ok(()) = ast_visitor::predicate_default(self, pred);
        }
        if let Some(tparams) = &func.tparams {
            let Ok(()) = ast_visitor::type_params_default(
                self,
                &ast_visitor::TypeParamsContext::Function,
                tparams,
            );
        }
        Ok(())
    }

    fn function_declaration(
        &mut self,
        loc: &L,
        func: &ast::function::Function<L, L>,
    ) -> Result<(), !> {
        let init_state = match &func.return_ {
            ast::function::ReturnAnnot::Available(_) => {
                WriteState::Annotation { contextual: false }
            }
            _ => WriteState::Value(0),
        };
        if let Some(id) = &func.id {
            self.in_context(
                Some(&|_cx| FindDeclarationsContext { init_state }),
                |inner| {
                    let Ok(()) = inner.function_identifier(id);
                },
            );
        }
        let is_polymorphic = func.tparams.is_some();
        self.enter_possibly_polymorphic_scope(is_polymorphic, ScopeKind::Var, loc, |inner| {
            let Ok(()) = inner.function_params(&func.params);
            let Ok(()) = ast_visitor::function_return_annotation_default(inner, &func.return_);
            let Ok(()) = ast_visitor::function_body_any_default(inner, &func.body);
            if let Some(pred) = &func.predicate {
                let Ok(()) = ast_visitor::predicate_default(inner, pred);
            }
            if let Some(tparams) = &func.tparams {
                let Ok(()) = ast_visitor::type_params_default(
                    inner,
                    &ast_visitor::TypeParamsContext::Function,
                    tparams,
                );
            }
        });
        Ok(())
    }

    fn component_declaration(
        &mut self,
        loc: &L,
        comp: &ast::statement::ComponentDeclaration<L, L>,
    ) -> Result<(), !> {
        let init_state = WriteState::Annotation { contextual: false };
        self.in_context(
            Some(&|_cx| FindDeclarationsContext { init_state }),
            |inner| {
                let Ok(()) = inner.component_identifier(&comp.id);
            },
        );
        let is_polymorphic = comp.tparams.is_some();
        self.enter_possibly_polymorphic_scope(is_polymorphic, ScopeKind::Var, loc, |inner| {
            if let Some(tparams) = &comp.tparams {
                let Ok(()) = ast_visitor::type_params_default(
                    inner,
                    &ast_visitor::TypeParamsContext::ComponentDeclaration,
                    tparams,
                );
            }
            let Ok(()) = ast_visitor::component_params_default(inner, &comp.params);
            if let Some(body) = &comp.body {
                let Ok(()) = ast_visitor::component_body_default(inner, body);
            }
            let Ok(()) = ast_visitor::component_renders_annotation_default(inner, &comp.renders);
        });
        Ok(())
    }

    fn declare_module(
        &mut self,
        loc: &L,
        module: &ast::statement::DeclareModule<L, L>,
    ) -> Result<(), !> {
        finder_declare_module(self, loc, module);
        Ok(())
    }

    fn if_statement(&mut self, _loc: &L, stmt: &ast::statement::If<L, L>) -> Result<(), !> {
        finder_if_statement(self, stmt);
        Ok(())
    }

    fn switch(&mut self, loc: &L, switch: &ast::statement::Switch<L, L>) -> Result<(), !> {
        finder_switch(self, loc, switch);
        Ok(())
    }

    fn try_catch(&mut self, _loc: &L, try_stmt: &ast::statement::Try<L, L>) -> Result<(), !> {
        finder_try_catch(self, try_stmt);
        Ok(())
    }

    fn update_expression(
        &mut self,
        _loc: &L,
        expr: &ast::expression::Update<L, L>,
    ) -> Result<(), !> {
        finder_update_expression(self, expr);
        Ok(())
    }

    fn record_declaration(
        &mut self,
        loc: &L,
        record: &ast::statement::RecordDeclaration<L, L>,
    ) -> Result<(), !> {
        finder_record_declaration(self, loc, record);
        Ok(())
    }

    fn record_declaration_body(
        &mut self,
        body: &ast::statement::record_declaration::Body<L, L>,
    ) -> Result<(), !> {
        finder_record_body(self, body);
        Ok(())
    }

    fn pattern_object_property_identifier_key(
        &mut self,
        _kind: Option<ast::VariableKind>,
        _key: &ast::Identifier<L, L>,
    ) -> Result<(), !> {
        // Don't call pattern_identifier on property keys--either it will have been called twice, or incorrectly.
        // The example to consider is
        //   { a1: a } = ...
        // pattern_object_property_identifier_key will be called on a1, but a1 is not a variable in scope, its a property on the
        // RHS object. On the other hand, a is a variable in scope, which will be visited by pattern_object_property_pattern.
        // If we didn't rename a1, we'd visit a1 with both pattern_object_property_identifier_key and
        // pattern_object_property_pattern, so it's safe to only visit it with the latter.
        Ok(())
    }

    fn match_expression(
        &mut self,
        loc: &L,
        m: &ast::expression::MatchExpression<L, L>,
    ) -> Result<(), !> {
        finder_match(
            self,
            loc,
            &m.arg,
            m.match_keyword_loc.dupe(),
            &m.cases,
            |visitor, body| {
                let Ok(()) = visitor.expression(body);
            },
        );
        Ok(())
    }

    fn match_statement(
        &mut self,
        loc: &L,
        m: &ast::statement::MatchStatement<L, L>,
    ) -> Result<(), !> {
        finder_match(
            self,
            loc,
            &m.arg,
            m.match_keyword_loc.dupe(),
            &m.cases,
            |visitor, body| {
                let Ok(()) = visitor.statement(body);
            },
        );
        Ok(())
    }

    fn match_case<B>(
        &mut self,
        case: &'ast ast::match_::Case<L, L, B>,
        on_case_body: &mut impl FnMut(&mut Self, &'ast B) -> Result<(), !>,
    ) -> Result<(), !> {
        finder_match_case(self, case, on_case_body)
    }

    fn match_object_pattern_property_key(
        &mut self,
        _key: &ast::match_pattern::object_pattern::Key<L, L>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn match_member_pattern_property(
        &mut self,
        _prop: &ast::match_pattern::member_pattern::Property<L, L>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn pattern_identifier(
        &mut self,
        kind: Option<ast::VariableKind>,
        ident: &ast::Identifier<L, L>,
    ) -> Result<(), !> {
        if let Some(var_kind) = kind {
            let binding_kind = match var_kind {
                ast::VariableKind::Var => BindingKind::Var,
                ast::VariableKind::Let => BindingKind::Let,
                ast::VariableKind::Const => BindingKind::Const,
            };
            self.new_entry(ident.name.dupe(), binding_kind, var_kind, ident.loc.dupe());
        }
        ast_visitor::identifier_default(self, ident)
    }

    fn function_this_param(
        &mut self,
        this_param: &ast::function::ThisParam<L, L>,
    ) -> Result<(), !> {
        self.new_entry(
            FlowSmolStr::new_inline("this"),
            BindingKind::Const,
            ast::VariableKind::Const,
            this_param.loc.dupe(),
        );
        ast_visitor::function_this_param_default(self, this_param)
    }

    fn function_identifier(&mut self, ident: &ast::Identifier<L, L>) -> Result<(), !> {
        self.new_entry(
            ident.name.dupe(),
            BindingKind::Function,
            ast::VariableKind::Let,
            ident.loc.dupe(),
        );
        ast_visitor::identifier_default(self, ident)
    }

    fn component_identifier(&mut self, ident: &ast::Identifier<L, L>) -> Result<(), !> {
        self.new_entry(
            ident.name.dupe(),
            BindingKind::Component,
            ast::VariableKind::Let,
            ident.loc.dupe(),
        );
        ast_visitor::identifier_default(self, ident)
    }

    fn declare_function(
        &mut self,
        loc: &L,
        stmt: &ast::statement::DeclareFunction<L, L>,
    ) -> Result<(), !> {
        if let Some(id) = &stmt.id {
            let id_loc = id.loc.dupe();
            self.new_entry(
                id.name.dupe(),
                BindingKind::DeclaredFunction,
                ast::VariableKind::Let,
                id_loc,
            );
        }
        ast_visitor::declare_function_default(self, loc, stmt)
    }

    fn declare_namespace(
        &mut self,
        loc: &L,
        decl: &ast::statement::DeclareNamespace<L, L>,
    ) -> Result<(), !> {
        // The default visitor routes the namespace id through pattern_identifier
        // with kind=Const, which our pattern_identifier override registers as a
        // `BindingKind::Const` provider. For type-only namespaces (e.g.
        // `declare namespace f { type T = ... }`) the namespace contributes no
        // value-side binding — its types are folded into a sibling function/class
        // via `wrap_with_namespace_types`. If we let the default fire, the
        // namespace's id loc ends up in the sibling function's provider list,
        // which makes mk_env emit an AssigningWrite at the namespace id with no
        // corresponding def in name_def, producing a `ReadOfUnreachedTvar`
        // InternalError when the sibling is read. Skip the id for type-only
        // bodies; value-bodied namespaces fall through to the default, which
        // still tracks the Const-style binding correctly.
        let stmt =
            ast::statement::Statement::new(ast::statement::StatementInner::DeclareNamespace {
                loc: loc.dupe(),
                inner: std::sync::Arc::new(decl.clone()),
            });
        if flow_parser::ast_utils::is_type_only_declaration_statement(&stmt) {
            let Ok(()) = self.block(&decl.body.0, &decl.body.1);
            Ok(())
        } else {
            if let ast::statement::declare_namespace::Id::Local(id) = &decl.id {
                let Ok(_) = self.pattern_identifier(Some(ast::VariableKind::Const), id);
            }
            let Ok(()) = self.block(&decl.body.0, &decl.body.1);
            Ok(())
        }
    }

    fn declare_variable(
        &mut self,
        _loc: &L,
        decl: &ast::statement::DeclareVariable<L, L>,
    ) -> Result<(), !> {
        let kind = decl.kind;
        for declarator in decl.declarations.iter() {
            if let ast::pattern::Pattern::Identifier { inner, .. } = &declarator.id {
                if let ast::types::AnnotationOrHint::Available(annot) = &inner.annot {
                    let Ok(()) = self.type_annotation(annot);
                }
                if let Some(init) = &declarator.init {
                    let Ok(()) = self.expression(init);
                }
                self.in_context(
                    Some(&|_cx| FindDeclarationsContext {
                        init_state: WriteState::Annotation { contextual: false },
                    }),
                    |inner_self| {
                        let Ok(()) = inner_self.pattern_identifier(Some(kind), &inner.name);
                    },
                );
            }
        }
        Ok(())
    }

    fn variable_declarator(
        &mut self,
        kind: ast::VariableKind,
        declarator: &ast::statement::variable::Declarator<L, L>,
    ) -> Result<(), !> {
        let annot = match &declarator.id {
            ast::pattern::Pattern::Array { inner, .. } => Some(&inner.annot),
            ast::pattern::Pattern::Object { inner, .. } => Some(&inner.annot),
            ast::pattern::Pattern::Identifier { inner, .. } => Some(&inner.annot),
            _ => None,
        };
        let init_state = match (&declarator.init, annot) {
            (_, Some(types::AnnotationOrHint::Available(_))) => {
                WriteState::Annotation { contextual: false }
            }
            (None, _) => WriteState::Nothing,
            (Some(init_expr), _) if matches!(&**init_expr, ExpressionInner::Array { inner, .. } if inner.elements.is_empty()) => {
                WriteState::EmptyArr
            }
            (Some(init_expr), _) if matches!(&**init_expr, ExpressionInner::NullLiteral { .. }) => {
                WriteState::Null(0)
            }
            _ => WriteState::Value(0),
        };
        self.in_context(
            Some(&|_cx| FindDeclarationsContext { init_state }),
            |inner| {
                let Ok(()) = inner.variable_declarator_pattern(kind, &declarator.id);
            },
        );
        if let Some(init) = &declarator.init {
            self.expression(init)?;
        }
        Ok(())
    }

    fn for_in_left_declaration(
        &mut self,
        left: &(L, ast::statement::VariableDeclaration<L, L>),
    ) -> Result<(), !> {
        let (_, decl) = left;
        if let Some(declarator) = decl.declarations.first() {
            let id = &declarator.id;
            let init_state = match id {
                ast::pattern::Pattern::Array { inner, .. }
                    if matches!(inner.annot, types::AnnotationOrHint::Available(_)) =>
                {
                    WriteState::Annotation { contextual: false }
                }
                ast::pattern::Pattern::Object { inner, .. }
                    if matches!(inner.annot, types::AnnotationOrHint::Available(_)) =>
                {
                    WriteState::Annotation { contextual: false }
                }
                ast::pattern::Pattern::Identifier { inner, .. }
                    if matches!(inner.annot, types::AnnotationOrHint::Available(_)) =>
                {
                    WriteState::Annotation { contextual: false }
                }
                _ => WriteState::Value(0),
            };
            self.in_context(
                Some(&|_cx| FindDeclarationsContext { init_state }),
                |inner| {
                    let Ok(()) = inner.variable_declarator_pattern(decl.kind, id);
                },
            );
        }
        Ok(())
    }

    fn for_of_left_declaration(
        &mut self,
        left: &(L, ast::statement::VariableDeclaration<L, L>),
    ) -> Result<(), !> {
        let (_, decl) = left;
        if let Some(declarator) = decl.declarations.first() {
            let id = &declarator.id;
            let init_state = match id {
                ast::pattern::Pattern::Array { inner, .. }
                    if matches!(inner.annot, types::AnnotationOrHint::Available(_)) =>
                {
                    WriteState::Annotation { contextual: false }
                }
                ast::pattern::Pattern::Object { inner, .. }
                    if matches!(inner.annot, types::AnnotationOrHint::Available(_)) =>
                {
                    WriteState::Annotation { contextual: false }
                }
                ast::pattern::Pattern::Identifier { inner, .. }
                    if matches!(inner.annot, types::AnnotationOrHint::Available(_)) =>
                {
                    WriteState::Annotation { contextual: false }
                }
                _ => WriteState::Value(0),
            };
            self.in_context(
                Some(&|_cx| FindDeclarationsContext { init_state }),
                |inner| {
                    let Ok(()) = inner.variable_declarator_pattern(decl.kind, id);
                },
            );
        }
        Ok(())
    }

    fn function_param_pattern(&mut self, pattern: &ast::pattern::Pattern<L, L>) -> Result<(), !> {
        let contextual = match pattern {
            ast::pattern::Pattern::Array { inner, .. } => {
                !matches!(inner.annot, types::AnnotationOrHint::Available(_))
            }
            ast::pattern::Pattern::Object { inner, .. } => {
                !matches!(inner.annot, types::AnnotationOrHint::Available(_))
            }
            ast::pattern::Pattern::Identifier { inner, .. } => {
                !matches!(inner.annot, types::AnnotationOrHint::Available(_))
            }
            _ => true,
        };
        let init_state = WriteState::Annotation { contextual };
        self.in_context(
            Some(&|_cx| FindDeclarationsContext { init_state }),
            |inner| {
                let Ok(()) = ast_visitor::function_param_pattern_default(inner, pattern);
            },
        );
        Ok(())
    }

    fn component_param_pattern(&mut self, pattern: &ast::pattern::Pattern<L, L>) -> Result<(), !> {
        let init_state = WriteState::Annotation { contextual: false };
        self.in_context(
            Some(&|_cx| FindDeclarationsContext { init_state }),
            |inner| {
                let Ok(()) = ast_visitor::component_param_pattern_default(inner, pattern);
            },
        );
        Ok(())
    }

    // Skip destructuring patterns in function type params (e.g. in
    // `declare function f({a}: T): R`), as they are not runtime bindings.
    fn function_param_type_pattern(
        &mut self,
        _patt: &'ast ast::pattern::Pattern<L, L>,
    ) -> Result<(), !> {
        Ok(())
    }
}

/// Context for the find_providers visitor.
#[derive(Debug, Clone, Copy)]
struct FindProvidersContext {
    mk_state: fn(i32) -> WriteState,
}

impl Default for FindProvidersContext {
    fn default() -> Self {
        Self {
            mk_state: |n| WriteState::Value(n),
        }
    }
}

struct FindProviders<L: LocSig> {
    env: Env<L>,
    cx: FindProvidersContext,
}

impl<L: LocSig> FindProviders<L> {
    fn new(env: Env<L>) -> Self {
        Self {
            env,
            cx: FindProvidersContext::default(),
        }
    }

    fn add_provider(&mut self, var: &FlowSmolStr, loc: L) {
        let mk_state = self.cx.mk_state;
        if let Some((entry, var_scopes_off, def_scopes_stack, entry_idx)) =
            find_entry_for_existing_variable(var, &self.env)
        {
            // Helper to find the index of a polymorphic scope
            fn find_polymorphic_scope_index<L: LocSig>(
                env: &FlowVector<Scope<L>>,
                start_idx: usize,
            ) -> Option<usize> {
                env.iter()
                    .enumerate()
                    .skip(start_idx)
                    .find(|(_, scope)| scope.kind == ScopeKind::Polymorphic)
                    .map(|(idx, _)| idx)
            }

            let possible_generic_escape = match def_scopes_stack {
                None => false,
                Some(def_scopes_start_idx) => {
                    let assign_polymorphic = find_polymorphic_scope_index(&self.env.0, 0);
                    let declare_polymorphic =
                        find_polymorphic_scope_index(&self.env.0, def_scopes_start_idx);
                    match (assign_polymorphic, declare_polymorphic) {
                        (Some(_), None) => true,
                        (Some(assign_idx), Some(declare_idx)) => assign_idx != declare_idx,
                        _ => false,
                    }
                }
            };

            let write_state = mk_state(var_scopes_off);
            let extended_state = if possible_generic_escape {
                Some(entry.state)
            } else {
                extended_state_opt(&write_state, &entry.state)
            };

            let def_locs: FlowOrdSet<L> = entry.def_locs.dupe().update(loc.dupe()).into();

            let (state, provider_locs): (IntermediateState, FlowOrdMap<L, WriteState>) =
                if let Some(new_state) = extended_state {
                    let provider_locs: FlowOrdMap<L, WriteState> = if !possible_generic_escape {
                        entry
                            .provider_locs
                            .dupe()
                            .update(loc.dupe(), write_state)
                            .into()
                    } else {
                        entry.provider_locs.dupe()
                    };
                    (new_state, provider_locs)
                } else {
                    (entry.state, entry.provider_locs.dupe())
                };

            let possible_generic_escape_locs: FlowOrdSet<L> = if possible_generic_escape {
                entry
                    .possible_generic_escape_locs
                    .dupe()
                    .update(loc.dupe())
                    .into()
            } else {
                entry.possible_generic_escape_locs.dupe()
            };

            let new_entry = BaseEntry::new(BaseEntryInner {
                entry_id: entry.entry_id,
                name: entry.name.dupe(),
                state,
                declare_locs: entry.declare_locs.dupe(),
                def_locs,
                provider_locs,
                possible_generic_escape_locs,
                binding_kind: entry.binding_kind,
            });

            let scope = &mut self.env.0[entry_idx];
            scope.entries.insert(var.dupe(), new_entry);
        }
    }
}

impl<L: LocSig> FinderBehavior<L> for FindProviders<L> {
    type Cx = FindProvidersContext;

    fn acc(&self) -> (&Env<L>, &Self::Cx) {
        (&self.env, &self.cx)
    }

    fn set_acc(&mut self, env: Env<L>, cx: Self::Cx) {
        self.env = env;
        self.cx = cx;
    }

    fn enter_lex_child(&self, kind: ScopeKind, loc: L, env: Env<L>) -> Env<L> {
        enter_existing_lex_child(kind, &loc, &env).unwrap_or(env)
    }
}

impl<'ast, L: LocSig> AstVisitor<'ast, L> for FindProviders<L> {
    fn normalize_loc(loc: &'ast L) -> &'ast L {
        loc
    }

    fn normalize_type(type_: &'ast L) -> &'ast L {
        type_
    }

    fn block(&mut self, loc: &L, stmt: &ast::statement::Block<L, L>) -> Result<(), !> {
        finder_block(self, loc, stmt);
        Ok(())
    }

    fn catch_clause(&mut self, clause: &ast::statement::try_::CatchClause<L, L>) -> Result<(), !> {
        finder_catch_clause(self, clause);
        Ok(())
    }

    fn do_while(&mut self, loc: &L, stmt: &ast::statement::DoWhile<L, L>) -> Result<(), !> {
        finder_do_while(self, loc, stmt);
        Ok(())
    }

    fn for_statement(&mut self, loc: &L, stmt: &ast::statement::For<L, L>) -> Result<(), !> {
        finder_for_statement(self, loc, stmt);
        Ok(())
    }

    fn for_in_statement(&mut self, loc: &L, stmt: &ast::statement::ForIn<L, L>) -> Result<(), !> {
        finder_for_in_statement(self, loc, stmt);
        Ok(())
    }

    fn for_of_statement(&mut self, loc: &L, stmt: &ast::statement::ForOf<L, L>) -> Result<(), !> {
        finder_for_of_statement(self, loc, stmt);
        Ok(())
    }

    fn while_(&mut self, loc: &L, stmt: &ast::statement::While<L, L>) -> Result<(), !> {
        finder_while(self, loc, stmt);
        Ok(())
    }

    fn with_(&mut self, loc: &L, stmt: &ast::statement::With<L, L>) -> Result<(), !> {
        finder_with(self, loc, stmt);
        Ok(())
    }

    fn class_body(&mut self, cls_body: &ast::class::Body<L, L>) -> Result<(), !> {
        finder_class_body(self, cls_body);
        Ok(())
    }

    fn class_expression(&mut self, loc: &L, cls: &ast::class::Class<L, L>) -> Result<(), !> {
        finder_class_expression(self, loc, cls);
        Ok(())
    }

    fn class_(&mut self, loc: &L, cls: &ast::class::Class<L, L>) -> Result<(), !> {
        finder_class(self, loc, cls);
        Ok(())
    }

    fn arrow_function(&mut self, loc: &L, func: &ast::function::Function<L, L>) -> Result<(), !> {
        finder_arrow_function(self, loc, func);
        Ok(())
    }

    fn function_expression_or_method(
        &mut self,
        loc: &L,
        func: &ast::function::Function<L, L>,
    ) -> Result<(), !> {
        finder_function_expression_or_method(self, loc, func);
        Ok(())
    }

    fn function_declaration(
        &mut self,
        loc: &L,
        func: &ast::function::Function<L, L>,
    ) -> Result<(), !> {
        finder_function_declaration(self, loc, func);
        Ok(())
    }

    fn component_declaration(
        &mut self,
        loc: &L,
        comp: &ast::statement::ComponentDeclaration<L, L>,
    ) -> Result<(), !> {
        finder_component_declaration(self, loc, comp);
        Ok(())
    }

    fn declare_module(
        &mut self,
        loc: &L,
        module: &ast::statement::DeclareModule<L, L>,
    ) -> Result<(), !> {
        finder_declare_module(self, loc, module);
        Ok(())
    }

    fn if_statement(&mut self, _loc: &L, stmt: &ast::statement::If<L, L>) -> Result<(), !> {
        finder_if_statement(self, stmt);
        Ok(())
    }

    fn switch(&mut self, loc: &L, switch: &ast::statement::Switch<L, L>) -> Result<(), !> {
        finder_switch(self, loc, switch);
        Ok(())
    }

    fn try_catch(&mut self, _loc: &L, try_stmt: &ast::statement::Try<L, L>) -> Result<(), !> {
        finder_try_catch(self, try_stmt);
        Ok(())
    }

    fn update_expression(
        &mut self,
        _loc: &L,
        expr: &ast::expression::Update<L, L>,
    ) -> Result<(), !> {
        finder_update_expression(self, expr);
        Ok(())
    }

    fn record_declaration(
        &mut self,
        loc: &L,
        record: &ast::statement::RecordDeclaration<L, L>,
    ) -> Result<(), !> {
        finder_record_declaration(self, loc, record);
        Ok(())
    }

    fn record_declaration_body(
        &mut self,
        body: &ast::statement::record_declaration::Body<L, L>,
    ) -> Result<(), !> {
        finder_record_body(self, body);
        Ok(())
    }

    fn pattern_object_property_identifier_key(
        &mut self,
        _kind: Option<ast::VariableKind>,
        _key: &ast::Identifier<L, L>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn match_object_pattern_property_key(
        &mut self,
        _key: &ast::match_pattern::object_pattern::Key<L, L>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn match_member_pattern_property(
        &mut self,
        _prop: &ast::match_pattern::member_pattern::Property<L, L>,
    ) -> Result<(), !> {
        Ok(())
    }

    fn pattern_identifier(
        &mut self,
        kind: Option<ast::VariableKind>,
        ident: &ast::Identifier<L, L>,
    ) -> Result<(), !> {
        if kind.is_none() {
            self.add_provider(&ident.name, ident.loc.dupe());
        }
        ast_visitor::identifier_default(self, ident)
    }

    fn assignment(&mut self, _loc: &L, expr: &ast::expression::Assignment<L, L>) -> Result<(), !> {
        let mk_state: fn(i32) -> WriteState = match expr.right.deref() {
            ExpressionInner::NullLiteral { .. } => |n| WriteState::Null(n),
            ExpressionInner::Array { inner, .. } if !inner.elements.is_empty() => {
                |n| WriteState::ArrayValue(n)
            }
            _ => |n| WriteState::Value(n),
        };

        let (left, _) = ast_utils::unwrap_nonnull_lhs(&expr.left);
        self.in_context(Some(&|_cx| FindProvidersContext { mk_state }), |inner| {
            let Ok(()) = inner.assignment_pattern(left.as_ref());
        });

        self.expression(&expr.right)
    }

    fn match_expression(
        &mut self,
        loc: &L,
        m: &ast::expression::MatchExpression<L, L>,
    ) -> Result<(), !> {
        finder_match(
            self,
            loc,
            &m.arg,
            m.match_keyword_loc.dupe(),
            &m.cases,
            |visitor, body| {
                let Ok(()) = visitor.expression(body);
            },
        );
        Ok(())
    }

    fn match_statement(
        &mut self,
        loc: &L,
        m: &ast::statement::MatchStatement<L, L>,
    ) -> Result<(), !> {
        finder_match(
            self,
            loc,
            &m.arg,
            m.match_keyword_loc.dupe(),
            &m.cases,
            |visitor, body| {
                let Ok(()) = visitor.statement(body);
            },
        );
        Ok(())
    }

    fn match_case<B>(
        &mut self,
        case: &'ast ast::match_::Case<L, L, B>,
        on_case_body: &mut impl FnMut(&mut Self, &'ast B) -> Result<(), !>,
    ) -> Result<(), !> {
        finder_match_case(self, case, on_case_body)
    }

    fn unary_expression(&mut self, loc: &L, expr: &ast::expression::Unary<L, L>) -> Result<(), !> {
        if expr.operator == ast::expression::UnaryOperator::Delete {
            if let ExpressionInner::Identifier { inner, .. } = &*expr.argument {
                self.add_provider(&inner.name, inner.loc.dupe());
                return Ok(());
            }
        }
        ast_visitor::unary_expression_default(self, loc, expr)
    }

    fn expression(&mut self, expr: &ast::expression::Expression<L, L>) -> Result<(), !> {
        match &**expr {
            ExpressionInner::Call { inner, .. } => {
                if let ExpressionInner::Member { inner: member, .. } = &*inner.callee {
                    if let ExpressionInner::Identifier {
                        inner: obj_ident, ..
                    } = member.object.deref()
                    {
                        if let ast::expression::member::Property::PropertyIdentifier(prop_ident) =
                            &member.property
                        {
                            if prop_ident.name.as_str() == "push"
                                && inner.targs.is_none()
                                && inner.arguments.arguments.len() == 1
                            {
                                if let Some(ast::expression::ExpressionOrSpread::Expression(
                                    arg_expr,
                                )) = inner.arguments.arguments.first()
                                {
                                    let name = &obj_ident.name;
                                    let arg_loc = arg_expr.loc().dupe();
                                    let state = state_of_var(name, &self.env);
                                    if matches!(
                                        state,
                                        Some(
                                            IntermediateState::EmptyArrInitialized
                                                | IntermediateState::ArrInitialized(_)
                                        )
                                    ) {
                                        self.in_context(
                                            Some(&|_cx| FindProvidersContext {
                                                mk_state: |n| WriteState::ArrWrite(n),
                                            }),
                                            |inner| {
                                                inner.add_provider(name, arg_loc);
                                            },
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
            ExpressionInner::Assignment { inner, .. } => {
                if inner.operator.is_none() {
                    if let ast::pattern::Pattern::Expression {
                        inner: left_expr, ..
                    } = &inner.left
                    {
                        if let ExpressionInner::Member {
                            inner: member_inner,
                            ..
                        } = &***left_expr
                        {
                            if let ExpressionInner::Identifier {
                                inner: obj_ident, ..
                            } = &*member_inner.object
                            {
                                if matches!(
                                    &member_inner.property,
                                    ast::expression::member::Property::PropertyExpression(_)
                                ) {
                                    let name = &obj_ident.name;
                                    let arg_loc = inner.right.loc().dupe();
                                    let state = state_of_var(name, &self.env);
                                    if matches!(
                                        state,
                                        Some(
                                            IntermediateState::EmptyArrInitialized
                                                | IntermediateState::ArrInitialized(_)
                                        )
                                    ) {
                                        self.in_context(
                                            Some(&|_cx| FindProvidersContext {
                                                mk_state: |n| WriteState::ArrWrite(n),
                                            }),
                                            |inner| {
                                                inner.add_provider(name, arg_loc);
                                            },
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        ast_visitor::expression_default(self, expr)
    }

    // Skip destructuring patterns in function type params (e.g. in
    // `declare function f({a}: T): R`), as they are not runtime bindings.
    fn function_param_type_pattern(
        &mut self,
        _patt: &'ast ast::pattern::Pattern<L, L>,
    ) -> Result<(), !> {
        Ok(())
    }
}

fn find_declaration_statements<L: LocSig>(stmts: &[ast::statement::Statement<L, L>]) -> Env<L> {
    let mut finder = FindDeclarations::new(empty_env());
    for stmt in stmts {
        let Ok(()) = finder.statement(stmt);
    }
    finder.env
}

fn enter_existing_lex_child<L: LocSig>(
    _kind: ScopeKind,
    loc: &L,
    env: &Env<L>,
) -> Result<Env<L>, ImpossibleStateError> {
    let current_scope = env
        .0
        .front()
        .expect("enter_existing_lex_child: env should be non-empty");
    if let Some(child) = current_scope.children.get(loc) {
        let mut new_env = env.0.dupe();
        new_env.push_front(child.dupe());
        Ok(Env(new_env))
    } else {
        Err(ImpossibleStateError(
            "Missing lexical child at expected position".to_string(),
        ))
    }
}

fn find_provider_statements<L: LocSig>(
    env: Env<L>,
    stmts: &[ast::statement::Statement<L, L>],
) -> Env<L> {
    let mut finder = FindProviders::new(env);
    for stmt in stmts {
        let Ok(()) = finder.statement(stmt);
    }
    finder.env
}

fn simplify_providers<L: LocSig>(entry: &IntermediateEntry<L>) -> Entry<L> {
    let mut providers = Providers::new();

    for (loc, write_state) in &entry.provider_locs {
        match (write_state, &entry.state) {
            (WriteState::Annotation { .. }, IntermediateState::Annotated { .. }) => {
                providers.writes.insert(loc.dupe(), WriteKind::Ordinary);
            }
            (WriteState::Annotation { .. }, _) => {}
            (
                WriteState::Null(n),
                IntermediateState::NullInitialized(m) | IntermediateState::Initialized(_, Some(m)),
            )
            | (
                WriteState::ArrayValue(n) | WriteState::Value(n),
                IntermediateState::Initialized(m, _),
            ) => {
                if *n == *m {
                    providers.writes.insert(loc.dupe(), WriteKind::Ordinary);
                }
            }
            (WriteState::ArrayValue(n), IntermediateState::ArrInitialized(m)) => {
                if *n == *m {
                    providers.writes.insert(loc.dupe(), WriteKind::Ordinary);
                }
            }
            (WriteState::ArrWrite(n), IntermediateState::ArrInitialized(m)) => {
                if *n == *m {
                    providers.array_writes.insert(loc.dupe());
                }
            }
            (
                WriteState::EmptyArr,
                IntermediateState::EmptyArrInitialized | IntermediateState::ArrInitialized(_),
            ) => {
                providers.writes.insert(loc.dupe(), WriteKind::EmptyArray);
            }
            _ => {}
        }
    }

    let final_state = match &entry.state {
        IntermediateState::Annotated { contextual } => State::AnnotatedVar {
            contextual: *contextual,
        },
        IntermediateState::ArrInitialized(_) => State::ArrayInitializedVar,
        IntermediateState::EmptyArrInitialized => State::EmptyArrayInitializedVar,
        IntermediateState::Initialized(_, _) => State::InitializedVar,
        IntermediateState::NullInitialized(_) => State::NullInitializedVar,
        IntermediateState::Uninitialized => State::UninitializedVar,
    };

    BaseEntry::new(BaseEntryInner {
        entry_id: entry.entry_id,
        name: entry.name.dupe(),
        state: final_state,
        declare_locs: entry.declare_locs.dupe(),
        def_locs: entry.def_locs.dupe(),
        provider_locs: providers,
        possible_generic_escape_locs: entry.possible_generic_escape_locs.dupe(),
        binding_kind: entry.binding_kind,
    })
}

pub fn compute_provider_env<L: LocSig>(stmts: &[ast::statement::Statement<L, L>]) -> Env<L> {
    let env = find_declaration_statements(stmts);
    find_provider_statements(env, stmts)
}

pub fn all_entries<L: LocSig>(env: &Env<L>) -> BTreeSet<Entry<L>>
where
    Entry<L>: Ord,
{
    fn all_entries_in_scope<L: LocSig>(scope: &Scope<L>) -> Vec<Entry<L>> {
        let mut result = Vec::new();

        for child in scope.children.values() {
            result.extend(all_entries_in_scope(child));
        }

        for entry in scope.entries.values() {
            result.push(simplify_providers(entry));
        }

        result
    }

    all_entries_in_scope(env.0.front().expect("Env should not be empty"))
        .into_iter()
        .collect()
}

pub fn get_providers_for_toplevel_var<L: LocSig>(
    var: &str,
    env: &Env<L>,
) -> Option<FlowOrdMap<L, WriteKind>> {
    let scope = env.0.front().expect("Env should not be empty");
    scope
        .entries
        .get(var)
        .map(|entry| simplify_providers(entry).provider_locs.writes.dupe())
}
