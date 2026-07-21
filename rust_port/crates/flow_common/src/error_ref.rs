/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::reason::VirtualReasonDesc;

/// The location and description from a reason that an error actually renders.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub struct ErrorReference<L: Dupe> {
    pub loc: L,
    pub desc: VirtualReasonDesc<L>,
}

impl<L: Dupe> ErrorReference<L> {
    /// Creates a reference with an explicitly selected location and description.
    pub fn new(loc: L, desc: VirtualReasonDesc<L>) -> Self {
        Self { loc, desc }
    }

    /// Maps the reference location and any locations embedded in its description.
    pub fn map_locs<M: Dupe, F>(&self, f: &F) -> ErrorReference<M>
    where
        F: Fn(&L) -> M,
    {
        ErrorReference::new(f(&self.loc), self.desc.map_locs(f))
    }
}
