/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::fmt;

use dupe::Dupe;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;

#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash)]
pub struct ILoc(pub Loc, pub u32);

impl fmt::Display for ILoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?}, {})", self.0, self.1)
    }
}
impl Ord for ILoc {
    fn cmp(&self, other: &Self) -> Ordering {
        let ILoc(a_loc, a_id) = self;
        let ILoc(b_loc, b_id) = other;
        let i = a_loc.cmp(b_loc);
        if i == Ordering::Equal {
            a_id.cmp(b_id)
        } else {
            i
        }
    }
}

impl PartialOrd for ILoc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl LocSig for ILoc {
    fn none() -> Self {
        ILoc(Loc::none(), 0)
    }

    fn compare(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }

    fn equal(&self, other: &Self) -> bool {
        self == other
    }

    fn debug_to_string(&self, include_source: bool) -> String {
        let ILoc(loc, id) = self;
        format!("({}, {})", loc.debug_to_string(include_source), id)
    }
}
