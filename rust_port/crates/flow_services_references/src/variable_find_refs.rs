/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use flow_analysis::scope_api;
use flow_parser::loc::Loc;

use crate::find_refs_types::RefKind;
use crate::find_refs_types::SingleRef;

pub fn local_find_refs(
    scope_info: &scope_api::ScopeInfo<Loc>,
    locs: &[Loc],
) -> Option<Vec<SingleRef>> {
    let all_uses = scope_info.all_uses();
    let matching_uses: BTreeSet<&Loc> = all_uses
        .into_iter()
        .filter(|use_loc| locs.iter().any(|loc| use_loc.contains(loc)))
        .collect();
    let num_matching_uses = matching_uses.len();
    if num_matching_uses == 0 {
        None
    } else {
        let mut sorted_locs: BTreeSet<Loc> = BTreeSet::new();
        for use_loc in &matching_uses {
            let def = scope_info
                .def_of_use_opt(use_loc)
                .expect("use should have a def");
            let uses = scope_info.uses_of_def(def, false);
            for u in uses.iter() {
                sorted_locs.insert(u.clone());
            }
        }
        let sorted_locs: Vec<Loc> = sorted_locs.into_iter().collect();
        let sorted_locs: Vec<SingleRef> = sorted_locs
            .into_iter()
            .map(|loc| (RefKind::Local, loc))
            .collect();
        Some(sorted_locs)
    }
}
