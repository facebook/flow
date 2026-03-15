/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::hash::Hash;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use vec1::Vec1;

use crate::bindings::Kind;

#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeId(pub(super) u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Def<Loc> {
    pub locs: Vec1<Loc>,
    pub name: ScopeId,
    pub actual_name: FlowSmolStr,
    pub kind: Kind,
}

impl<Loc: PartialOrd> PartialOrd for Def<Loc> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        for (t1, t2) in self.locs.iter().zip(other.locs.iter()) {
            let i = t1.partial_cmp(t2)?;
            if i != std::cmp::Ordering::Equal {
                return Some(i);
            }
        }
        Some(self.locs.len().cmp(&other.locs.len()))
    }
}

impl<Loc: Ord> Ord for Def<Loc> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // OCaml: | ([], []) -> 0 | ([], _) -> -1 | (_, []) -> 1
        for (t1, t2) in self.locs.iter().zip(other.locs.iter()) {
            let i = t1.cmp(t2);
            if i != std::cmp::Ordering::Equal {
                return i;
            }
        }
        self.locs.len().cmp(&other.locs.len())
    }
}

impl<Loc: PartialEq> Def<Loc> {
    pub fn is(&self, x: &Loc) -> bool {
        self.locs.iter().any(|loc| loc == x)
    }
}

#[derive(Debug, Clone)]
pub struct Scope<Loc> {
    pub lexical: bool,
    pub parent: Option<ScopeId>,
    pub defs: BTreeMap<FlowSmolStr, Def<Loc>>,
    pub locals: BTreeMap<Loc, Def<Loc>>,
    pub globals: Vec<FlowSmolStr>,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct ScopeInfo<Loc> {
    pub(super) max_distinct: u32,
    pub scopes: BTreeMap<ScopeId, Scope<Loc>>,
}

impl<Loc: Dupe + PartialEq + Eq + Ord + Hash> ScopeInfo<Loc> {
    pub fn empty() -> Self {
        Self {
            max_distinct: 0,
            scopes: BTreeMap::new(),
        }
    }

    pub fn all_uses(&self) -> BTreeSet<&Loc> {
        let mut uses = BTreeSet::new();
        for scope in self.scopes.values() {
            for use_loc in scope.locals.keys() {
                uses.insert(use_loc);
            }
        }
        uses
    }

    pub fn defs_of_all_uses(&self) -> BTreeMap<Loc, Def<Loc>> {
        let mut result = BTreeMap::new();
        for scope in self.scopes.values() {
            for (use_loc, def) in &scope.locals {
                result.insert(use_loc.dupe(), def.clone());
            }
        }
        result
    }

    pub fn uses_of_all_defs(&self) -> BTreeMap<Def<Loc>, BTreeSet<Loc>> {
        let use_def_map = self.defs_of_all_uses();
        let mut def_uses_map: BTreeMap<Def<Loc>, BTreeSet<Loc>> = BTreeMap::new();
        for (use_loc, def) in use_def_map {
            def_uses_map
                .entry(def)
                .or_insert_with(BTreeSet::new)
                .insert(use_loc);
        }
        def_uses_map
    }

    pub fn def_of_use_opt(&self, use_loc: &Loc) -> Option<&Def<Loc>> {
        for scope in self.scopes.values() {
            if let Some(def) = scope.locals.get(use_loc) {
                return Some(def);
            }
        }
        None
    }

    pub fn use_is_def(&self, use_loc: &Loc) -> bool {
        let def = self.def_of_use_opt(use_loc).unwrap();
        def.is(use_loc)
    }

    pub fn uses_of_def(&self, def: &Def<Loc>, exclude_def: bool) -> BTreeSet<Loc> {
        let mut uses = BTreeSet::new();
        for scope in self.scopes.values() {
            for (use_loc, def_prime) in &scope.locals {
                if exclude_def && def_prime.is(use_loc) {
                    continue;
                }
                if def == def_prime {
                    uses.insert(use_loc.dupe());
                }
            }
        }
        uses
    }

    pub fn scopes_of_uses_of_def(&self, def: &Def<Loc>) -> BTreeSet<ScopeId> {
        let mut scopes = BTreeSet::new();
        for (scope_id, scope) in &self.scopes {
            for (use_loc, def_prime) in &scope.locals {
                if def_prime.is(use_loc) {
                    continue;
                }
                if def == def_prime {
                    scopes.insert(*scope_id);
                }
            }
        }
        scopes
    }

    pub fn uses_of_use(&self, use_loc: &Loc, exclude_def: bool) -> BTreeSet<Loc> {
        let Some(def) = self.def_of_use_opt(use_loc) else {
            return BTreeSet::new();
        };
        self.uses_of_def(def, exclude_def)
    }

    pub fn def_is_unused(&self, def: &Def<Loc>) -> bool {
        self.uses_of_def(def, true).is_empty()
    }

    pub fn scope(&self, scope_id: ScopeId) -> &Scope<Loc> {
        self.scopes
            .get(&scope_id)
            .unwrap_or_else(|| panic!("Scope {} not found", scope_id.0))
    }

    pub fn scope_within<'a>(&'a self, scope_id: ScopeId, mut s: &'a Scope<Loc>) -> bool {
        loop {
            match s.parent {
                None => return false,
                Some(p) => {
                    if p == scope_id {
                        return true;
                    } else {
                        s = self.scope(p);
                    }
                }
            }
        }
    }

    pub fn scope_of_loc(&self, scope_loc: &Loc) -> Vec<ScopeId> {
        let mut result = Vec::new();
        for (scope_id, scope) in &self.scopes {
            if &scope.loc == scope_loc {
                result.push(*scope_id);
            }
        }
        result
    }

    pub fn closest_enclosing_scope(
        &self,
        loc: &Loc,
        in_range: impl Fn(&Loc, &Loc) -> bool,
    ) -> ScopeId {
        let mut scope_id = ScopeId(0);
        let mut prev_scope = self.scope(ScopeId(0));
        for (this_scope_id, this_scope) in &self.scopes {
            if in_range(loc, &this_scope.loc) && in_range(&this_scope.loc, &prev_scope.loc) {
                scope_id = *this_scope_id;
                prev_scope = this_scope;
            }
        }
        scope_id
    }

    pub fn is_local_use(&self, use_loc: &Loc) -> bool {
        self.scopes
            .values()
            .any(|scope| scope.locals.contains_key(use_loc))
    }

    pub fn fold_scope_chain<A>(
        &self,
        mut scope_id: ScopeId,
        mut acc: A,
        mut f: impl FnMut(ScopeId, &Scope<Loc>, A) -> A,
    ) -> A {
        loop {
            let s = self.scope(scope_id);
            acc = f(scope_id, s, acc);
            match s.parent {
                Some(parent_id) => {
                    scope_id = parent_id;
                }
                None => return acc,
            }
        }
    }
}
