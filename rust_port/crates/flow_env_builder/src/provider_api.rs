/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_common::reason::Name;
use flow_common::reason::VirtualReason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::mk_reason;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_parser::loc_sig::LocSig;

use crate::find_providers::Entry;
use crate::find_providers::Env;
use crate::find_providers::State;
use crate::find_providers::WriteKind;
use crate::find_providers::all_entries;
use crate::find_providers::compute_provider_env;
use crate::find_providers::empty_env;
use crate::find_providers::get_providers_for_toplevel_var as fp_get_providers_for_toplevel_var;

#[derive(Debug, Clone, Dupe, PartialEq, Eq)]
pub struct Provider<L: LocSig> {
    pub reason: VirtualReason<L>,
    pub empty_array_writes: Option<FlowOrdSet<L>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefProviders<L: LocSig> {
    pub state: State,
    pub providers: Vec<Provider<L>>,
    pub array_providers: FlowOrdSet<L>,
    pub possible_generic_escape_locs: FlowOrdSet<L>,
}

#[derive(Debug, Clone)]
pub struct Info<L: LocSig> {
    pub all_exact_providers: FlowOrdSet<L>,
    pub all_array_providers: FlowOrdSet<L>,
    pub all_annotated_providers: FlowOrdSet<L>,
    pub all_providers_of_writes: FlowOrdMap<L, DefProviders<L>>,
    pub raw_env: Env<L>,
}

impl<L: LocSig> Default for Info<L> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<L: LocSig> Info<L> {
    pub fn empty() -> Self {
        Info {
            all_exact_providers: FlowOrdSet::new(),
            all_array_providers: FlowOrdSet::new(),
            all_annotated_providers: FlowOrdSet::new(),
            all_providers_of_writes: FlowOrdMap::new(),
            raw_env: empty_env(),
        }
    }

    pub fn is_provider(&self, loc: &L) -> bool {
        self.all_exact_providers.contains(loc)
    }

    pub fn is_array_provider(&self, loc: &L) -> bool {
        self.all_array_providers.contains(loc)
    }

    pub fn is_provider_of_annotated(&self, loc: &L) -> bool {
        self.all_annotated_providers.contains(loc)
    }

    pub fn providers_of_def(&self, loc: &L) -> Option<&DefProviders<L>> {
        self.all_providers_of_writes.get(loc)
    }

    pub fn get_providers_for_toplevel_var(&self, var: &str) -> Option<FlowOrdMap<L, WriteKind>> {
        fp_get_providers_for_toplevel_var(var, &self.raw_env)
    }
}

pub fn is_provider_state_fully_initialized(state: &State) -> bool {
    match state {
        State::InitializedVar
        | State::AnnotatedVar { .. }
        | State::ArrayInitializedVar
        | State::EmptyArrayInitializedVar => true,
        State::UninitializedVar | State::NullInitializedVar => false,
    }
}

fn all_exact_providers<L: LocSig>(entries: &std::collections::BTreeSet<Entry<L>>) -> FlowOrdSet<L> {
    let mut result = FlowOrdSet::new();
    for entry in entries {
        for loc in entry.provider_locs.writes.keys() {
            result.insert(loc.dupe());
        }
    }
    result
}

fn all_array_providers<L: LocSig>(entries: &std::collections::BTreeSet<Entry<L>>) -> FlowOrdSet<L> {
    let mut result = FlowOrdSet::new();
    for entry in entries {
        result = result.union(entry.provider_locs.array_writes.dupe());
    }
    result
}

fn all_annotated_providers<L: LocSig>(
    entries: &std::collections::BTreeSet<Entry<L>>,
) -> FlowOrdSet<L> {
    let mut result = FlowOrdSet::new();
    for entry in entries {
        if matches!(entry.state, State::AnnotatedVar { .. }) {
            for loc in entry.provider_locs.writes.keys() {
                result.insert(loc.dupe());
            }
        }
    }
    result
}

fn all_providers_of_writes<L: LocSig>(
    entries: &std::collections::BTreeSet<Entry<L>>,
) -> FlowOrdMap<L, DefProviders<L>> {
    let mut result = FlowOrdMap::new();

    for entry in entries {
        let mut providers: Vec<Provider<L>> = entry
            .provider_locs
            .writes
            .iter()
            .map(|(loc, write_kind)| {
                let reason = mk_reason(
                    VirtualReasonDesc::RIdentifier(Name::new(entry.name.dupe())),
                    loc.dupe(),
                );
                let empty_array_writes = match write_kind {
                    WriteKind::Ordinary => None,
                    WriteKind::EmptyArray => Some(entry.provider_locs.array_writes.dupe()),
                };
                Provider {
                    reason,
                    empty_array_writes,
                }
            })
            .collect();

        providers.sort_by(|a, b| a.reason.loc().cmp(b.reason.loc()));

        let def_providers = DefProviders {
            state: entry.state,
            providers,
            array_providers: entry.provider_locs.array_writes.dupe(),
            possible_generic_escape_locs: entry.possible_generic_escape_locs.dupe(),
        };

        for loc in &entry.declare_locs {
            result = result.update(loc.dupe(), def_providers.clone()).into();
        }
        for loc in &entry.def_locs {
            result = result.update(loc.dupe(), def_providers.clone()).into();
        }
    }

    result
}

pub fn find_providers<L: LocSig>(program: &flow_parser::ast::Program<L, L>) -> Info<L> {
    let env = compute_provider_env(&program.statements);
    let entries = all_entries(&env);

    Info {
        all_exact_providers: all_exact_providers(&entries),
        all_array_providers: all_array_providers(&entries),
        all_annotated_providers: all_annotated_providers(&entries),
        all_providers_of_writes: all_providers_of_writes(&entries),
        raw_env: env,
    }
}
