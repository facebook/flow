/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The dynamic scope of a `flow_js` solve.
//!
//! Speculation, type-application expansion and implicit instantiation are
//! nested scopes of the constraint solver, not properties of the file being
//! checked, so they are carried in a [`FlowJsEnv`] threaded through `flow_js`
//! rather than in `Context`. The value is immutable: a nested scope derives a
//! new env, so a scope cannot outlive the call that created it and there is
//! nothing to restore on the way out.
//!
//! The scope stops at the edge of the solver. A checker entry point, a lazily
//! forced tvar, a hint evaluation and a module forcing each start their own
//! solve at [`FlowJsEnv::entry`], so code above `flow_js` sees the default env
//! and calls the plain `foo` rather than the `foo_with_env` it wraps. The one
//! way a live scope reaches that code is a closure the solver calls back
//! *inside* the scope — a speculation case and a `flow_js` helper-trait method
//! — which is why those take an env of their own.

use std::cell::RefCell;
use std::collections::BTreeSet;
use std::rc::Rc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_type::type_::SpecState;
use flow_typing_type::type_::SpeculationHintState;
use flow_typing_type::type_::Type;

#[derive(Debug, Clone)]
pub enum InformationForSynthesisLogging {
    CallInformationForSynthesisLogging {
        lhs_t: Type,
        call_callee_hint_ref: Rc<RefCell<SpeculationHintState>>,
    },
    NoInformationForSynthesisLogging,
}

// Next, a model for "cases." A case serves as the context for a speculative
// match. In other words, while we're trying to execute a flow in speculation
// mode, we use this data structure to record stuff.
//
// A case carries a (local) index that identifies which type we're currently
// considering among the members of a union or intersection type. This is used
// only for error reporting.
#[derive(Debug, Clone)]
pub struct Case {
    pub case_id: i32,
    pub errors: Rc<RefCell<Vec<ErrorMessage<ALoc>>>>,
    pub information_for_synthesis_logging: InformationForSynthesisLogging,
}

/// A branch is a wrapper around a case, that also carries the speculation id of
/// the spec currently being processed.
#[derive(Debug, Clone)]
pub struct Branch {
    pub speculation_id: i32,
    pub case: Case,
}

pub mod type_app_expansion {
    use super::*;

    // Array types function like type applications but are not implemented as such. Unless
    // we decide to unify their implementation with regular typeapps, they need special
    // handling here
    #[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Root {
        Type(Type),
        Array(flow_common::reason::Reason),
        ROArray(flow_common::reason::Reason),
        Tuple(flow_common::reason::Reason, usize),
    }

    pub type RootSet = BTreeSet<Root>;

    #[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq)]
    pub enum Bound {
        Lower,
        Upper,
    }

    #[derive(Debug, Clone)]
    pub struct Entry(pub Type, pub Vec<RootSet>, pub Bound);
}

use type_app_expansion::Bound;
use type_app_expansion::Entry;
use type_app_expansion::RootSet;

/// One entered speculation branch, holding the branches it is nested in.
#[derive(Debug)]
struct SpeculationFrame {
    branch: Branch,
    enclosing: Option<Rc<SpeculationFrame>>,
}

/// One type application being expanded, holding the expansions it is nested in.
#[derive(Debug)]
struct TypeAppFrame {
    entry: Entry,
    enclosing: Option<Rc<TypeAppFrame>>,
}

/// The environment for `flow_js` related functions. Callers outside the
/// `flow_js` system use [`FlowJsEnv::entry`]; any non-default env out there
/// must be carefully audited (see the module docs for the two ways one can
/// legitimately get there).
///
/// Deriving an env is on the hot path of every entry into the solver, so the
/// scope stacks are shared with the env derived from rather than copied: only
/// entering a scope allocates, and only the single frame it pushes.
#[derive(Debug, Clone, Dupe, Default)]
pub struct FlowJsEnv {
    /// The innermost speculation branch being tried, or [`None`] when we are
    /// not speculating, in which case errors are reported instead of being
    /// deferred to a branch.
    speculation: Option<Rc<SpeculationFrame>>,
    /// The innermost type application being expanded, used to cut off expansion
    /// that would otherwise diverge.
    instantiation_stack: Option<Rc<TypeAppFrame>>,
    /// Whether we are inside the implicit instantiation solver, where
    /// instantiable tvars must not be resolved the way they normally are.
    in_implicit_instantiation: bool,
}

impl FlowJsEnv {
    /// The env at a checker entry point: not speculating, nothing expanding,
    /// not solving type arguments.
    pub fn entry() -> Self {
        Self::default()
    }

    // ---- speculation ----

    pub fn speculating(&self) -> bool {
        self.speculation.is_some()
    }

    /// The enclosing branches, innermost first.
    fn branches(&self) -> impl Iterator<Item = &Branch> + '_ {
        std::iter::successors(self.speculation.as_deref(), |frame| {
            frame.enclosing.as_deref()
        })
        .map(|frame| &frame.branch)
    }

    /// The innermost branch, identified by its speculation and case ids.
    pub fn speculation_id(&self) -> Option<SpecState> {
        self.speculation.as_ref().map(|frame| SpecState {
            speculation_id: frame.branch.speculation_id,
            case_id: frame.branch.case.case_id,
        })
    }

    /// The speculation ids of the enclosing branches, outermost first.
    pub fn speculation_id_path(&self) -> impl Iterator<Item = i32> + '_ {
        let mut path: Vec<i32> = self
            .branches()
            .map(|branch| branch.speculation_id)
            .collect();
        path.reverse();
        path.into_iter()
    }

    /// The (speculation id, case id) of every enclosing branch, innermost
    /// first. An edge recorded under any of these is already on the path to the
    /// current branch, so callers ask whether one is there rather than where.
    pub fn speculation_path(&self) -> impl Iterator<Item = (i32, i32)> + '_ {
        self.branches()
            .map(|branch| (branch.speculation_id, branch.case.case_id))
    }

    /// Record an error against the innermost branch, to be replayed if that
    /// branch turns out to be the one that wins. A no-op when not speculating.
    pub fn defer_error(&self, msg: ErrorMessage<ALoc>) {
        if let Some(frame) = &self.speculation {
            frame.branch.case.errors.borrow_mut().push(msg);
        }
    }

    /// Enter `branch`. What the branch expands is its own business and is
    /// discarded along with the branch, so expansion starts over.
    pub fn with_branch(&self, branch: Branch) -> Self {
        Self {
            speculation: Some(Rc::new(SpeculationFrame {
                branch,
                enclosing: self.speculation.dupe(),
            })),
            instantiation_stack: self.instantiation_stack.dupe(),
            in_implicit_instantiation: self.in_implicit_instantiation,
        }
    }

    // ---- implicit instantiation ----

    pub fn in_implicit_instantiation(&self) -> bool {
        self.in_implicit_instantiation
    }

    pub fn solving_implicit_instantiation(&self) -> Self {
        Self {
            in_implicit_instantiation: true,
            ..self.dupe()
        }
    }

    // ---- type application expansion ----

    /// Detect whether expanding `c<ts>` would loop, given the roots `tss` of
    /// `ts`. Returns the env to expand under, or [`None`] if `limit`
    /// possibly-expanding occurrences of `c` are already on the stack.
    pub fn push_typeapp_unless_loop(
        &self,
        limit: i32,
        side: Bound,
        c: &Type,
        tss: Vec<RootSet>,
    ) -> Option<Self> {
        let expanding = std::iter::successors(self.instantiation_stack.as_deref(), |frame| {
            frame.enclosing.as_deref()
        });
        let mut count = 0;
        for frame in expanding {
            let Entry(prev_c, prev_tss, prev_side) = &frame.entry;
            if c == prev_c && possibly_expanding_targs(prev_tss, &tss) && side == *prev_side {
                count += 1;
                if count >= limit {
                    return None;
                }
            }
        }
        Some(Self {
            speculation: self.speculation.dupe(),
            instantiation_stack: Some(Rc::new(TypeAppFrame {
                entry: Entry(c.dupe(), tss, side),
                enclosing: self.instantiation_stack.dupe(),
            })),
            in_implicit_instantiation: self.in_implicit_instantiation,
        })
    }

    /// Force a signature tvar's thunk: neither the enclosing speculation nor
    /// the enclosing expansion applies to it.
    pub fn signature_tvar_env(&self) -> Self {
        Self {
            speculation: None,
            instantiation_stack: None,
            in_implicit_instantiation: self.in_implicit_instantiation,
        }
    }
}

// Say that targs are possibly expanding when, given previous targs and
// current targs, each previously non-empty targ is contained in the
// corresponding current targ.
fn possibly_expanding_targs(prev_tss: &[RootSet], tss: &[RootSet]) -> bool {
    let mut seen_nonempty_prev_ts = false;
    let mut prev_iter = prev_tss.iter();
    let mut curr_iter = tss.iter();

    loop {
        match (prev_iter.next(), curr_iter.next()) {
            (Some(prev_ts), Some(ts)) => {
                // if prev_ts is not a subset of ts, we have found a counterexample
                // and we can bail out
                if !prev_ts.is_subset(ts) {
                    return false;
                }
                // otherwise, we recurse on the remaining targs, updating the bit
                seen_nonempty_prev_ts = seen_nonempty_prev_ts || !prev_ts.is_empty();
            }
            // we have found no counterexamples, so it comes down to whether we've
            // seen any non-empty prev_ts
            (None, None) => return seen_nonempty_prev_ts,
            // something's wrong around arities, but that's not our problem, so bail out
            _ => return false,
        }
    }
}
