/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::ops::Deref;

use dupe::Dupe;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::subst_name::SubstName;
use crate::subst_name::SubstNameInner;

pub fn string(name: &FlowSmolStr, used_names: &BTreeSet<FlowSmolStr>) -> FlowSmolStr {
    let mut i = 0;
    loop {
        let candidate = if i == 0 {
            FlowSmolStr::new(format!("{}_", name.as_str()))
        } else {
            FlowSmolStr::new(format!("{}_{}", name.as_str(), i))
        };
        if !used_names.contains(&candidate) {
            return candidate;
        }
        i += 1;
    }
}

pub fn subst_name(name: &SubstName, used_names: &FlowOrdSet<SubstName>) -> SubstName {
    let (ct, n): (i32, FlowSmolStr) = match name.deref() {
        SubstNameInner::Synthetic { name, .. } => {
            panic!("Cannot rename synthetic name {}", name)
        }
        SubstNameInner::Name(n) => (0, n.dupe()),
        SubstNameInner::Id(ct, n) => (*ct, n.dupe()),
    };

    let mut ct = ct + 1;
    loop {
        let name = SubstName::id(ct, n.dupe());
        if !used_names.contains(&name) {
            return name;
        }
        ct += 1;
    }
}

pub fn avoid_capture<N, M, UsedNames>(
    name: N,
    map: M,
    in_free_vars: impl FnOnce(&N) -> bool,
    used_names: impl FnOnce() -> UsedNames,
    fresh_name: impl FnOnce(&N, &UsedNames) -> N,
    remove: impl FnOnce(M, &N) -> M,
    add_alpha: impl FnOnce(M, N, N) -> M,
) -> (N, M)
where
    N: Clone,
{
    if in_free_vars(&name) {
        let new_name = fresh_name(&name, &used_names());
        let map = add_alpha(map, name, new_name.clone());
        (new_name, map)
    } else {
        let map = remove(map, &name);
        (name, map)
    }
}
