/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::hash::Hash;

use dupe::Dupe;
use flow_analysis::scope_api::Def;
use flow_analysis::scope_api::Scope;
use flow_analysis::scope_api::ScopeInfo;
use flow_analysis::ssa_api::Values;
use flow_analysis::ssa_api::WriteLoc;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;

use crate::provider_api::Info as ProviderInfo;
use crate::provider_api::is_provider_state_fully_initialized;

// We consider a binding to be const-like if all reads point to the same
// write, modulo initialization.
pub fn is_const_like<L>(info: &ScopeInfo<L>, values: &Values<L>, loc: &L) -> bool
where
    L: Dupe + Clone + Eq + Ord + Hash,
{
    let Some(def) = info.def_of_use_opt(loc) else {
        return false;
    };
    let def_locs = &def.locs;
    let uses = info.uses_of_use(loc, false);

    // We consider a binding to be const-like if all reads point to the same
    // write, modulo initialization.
    let mut writes: BTreeSet<L> = BTreeSet::new();
    for use_loc in &uses {
        match values.get(use_loc) {
            None => {
                // use is a write
            }
            Some(write_locs) => {
                // use is a read
                // collect writes pointed to by the read, modulo initialization
                for write_loc in write_locs {
                    match write_loc {
                        WriteLoc::Uninitialized => {}
                        WriteLoc::Write(reason) => {
                            writes.insert(reason.loc().dupe());
                        }
                    }
                }
            }
        }
    }

    match writes.iter().next() {
        None => true,
        Some(write_loc) => writes.len() <= 1 && def_locs.iter().any(|def_loc| def_loc == write_loc),
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum InitializationValid<L: Dupe + Clone + Eq + Ord> {
    #[default]
    Valid,
    NotWritten {
        possible_generic_escape_locs: FlowOrdSet<L>,
    },
    NullWritten {
        null_provider_loc: L,
        possible_generic_escape_locs: FlowOrdSet<L>,
    },
}

pub fn declaration_validity<L>(
    info: &ScopeInfo<L>,
    values: &Values<L>,
    providers: &ProviderInfo<L>,
    loc: &L,
) -> InitializationValid<L>
where
    L: Dupe + Clone + Eq + Ord + Hash + flow_parser::loc_sig::LocSig,
{
    let Some(def_providers) = providers.providers_of_def(loc) else {
        return InitializationValid::Valid;
    };
    if !def_providers.providers.is_empty()
        && is_provider_state_fully_initialized(&def_providers.state)
    {
        return InitializationValid::Valid;
    }

    let uses = info.uses_of_use(loc, false);

    // Since this variable is not fully initialized, if there are any providers then
    // they must be null providers
    let null_providers: BTreeSet<L> = def_providers
        .providers
        .iter()
        .map(|p| p.reason.loc().dupe())
        .collect();

    let reads_exist = uses.iter().any(|use_loc| values.contains_key(use_loc));

    let possible_generic_escape_locs = def_providers.possible_generic_escape_locs.clone();

    match (reads_exist, !null_providers.is_empty()) {
        (false, _) => InitializationValid::Valid,
        (true, false) => InitializationValid::NotWritten {
            possible_generic_escape_locs,
        },
        (true, true) => {
            let null_provider_loc = null_providers
                .iter()
                .next()
                .expect("null_providers is not empty")
                .dupe();
            InitializationValid::NullWritten {
                null_provider_loc,
                possible_generic_escape_locs,
            }
        }
    }
}

pub fn is_not_captured_by_closure<L>(info: &ScopeInfo<L>, use_loc: &L) -> bool
where
    L: Dupe + Clone + Eq + Ord + Hash,
{
    fn lookup<L>(
        in_current_var_scope: bool,
        info: &ScopeInfo<L>,
        scope: &Scope<L>,
        def: &Def<L>,
    ) -> bool
    where
        L: Dupe + Clone + Eq + Ord + Hash,
    {
        if scope.defs.values().any(|def_prime| def == def_prime) {
            return in_current_var_scope;
        }

        match scope.parent {
            None => true,
            Some(scope_id) => {
                let scope_prime = info.scope(scope_id);
                lookup(
                    in_current_var_scope && scope.lexical,
                    info,
                    scope_prime,
                    def,
                )
            }
        }
    }

    match info.scope_of_use(use_loc) {
        Some((_scope_id, scope)) => match scope.locals.get(use_loc) {
            Some(def) => lookup(true, info, scope, def),
            None => true,
        },
        None => true,
    }
}

pub fn written_by_closure<L>(info: &ScopeInfo<L>, values: &Values<L>, loc: &L) -> BTreeSet<L>
where
    L: Dupe + Clone + Eq + Ord + Hash,
{
    let uses = info.uses_of_use(loc, false);
    let mut result = BTreeSet::new();

    for use_loc in uses {
        match values.get(&use_loc) {
            None => {
                // use is a write
                if !is_not_captured_by_closure(info, &use_loc) {
                    result.insert(use_loc);
                }
            }
            Some(_write_locs) => {
                // use is a read
                // collect writes pointed to by the read, modulo initialization
            }
        }
    }

    result
}

// Some variables are unhavocable by making a function call but still can be havoced in other
// situations. `via_call` indicates whether the havocing operation is a call or something
// else (resetting an activation scope, yielding, entering a new scope)

pub struct InvalidationCaches<L: Clone + Eq + Ord> {
    const_like_cache: BTreeMap<L, bool>,
    written_by_closure_cache: BTreeMap<L, bool>,
}

impl<L: Clone + Eq + Ord> InvalidationCaches<L> {
    pub fn new() -> Self {
        InvalidationCaches {
            const_like_cache: BTreeMap::new(),
            written_by_closure_cache: BTreeMap::new(),
        }
    }
}

impl<L: Clone + Eq + Ord> Default for InvalidationCaches<L> {
    fn default() -> Self {
        Self::new()
    }
}

pub fn should_invalidate<L>(
    all: bool,
    caches: &mut InvalidationCaches<L>,
    info: &ScopeInfo<L>,
    values: &Values<L>,
    loc: L,
) -> bool
where
    L: Dupe + Clone + Eq + Ord + Hash,
{
    let const_like = caches
        .const_like_cache
        .get(&loc)
        .copied()
        .unwrap_or_else(|| {
            let b = is_const_like(info, values, &loc);
            caches.const_like_cache.insert(loc.dupe(), b);
            b
        });

    if const_like {
        return false;
    }

    if all {
        return true;
    }

    let not_written_by_closure = caches
        .written_by_closure_cache
        .get(&loc)
        .copied()
        .unwrap_or_else(|| {
            let b = written_by_closure(info, values, &loc).is_empty();
            caches.written_by_closure_cache.insert(loc.dupe(), b);
            b
        });

    !not_written_by_closure
}
