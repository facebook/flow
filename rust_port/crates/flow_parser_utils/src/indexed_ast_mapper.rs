/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::loc::Loc;
use flow_parser::polymorphic_ast_mapper::LocMapper;

use crate::iloc::ILoc;

pub struct IndexMapper {
    counter: u32,
}

impl IndexMapper {
    pub fn new() -> Self {
        Self { counter: 0 }
    }
}

impl Default for IndexMapper {
    fn default() -> Self {
        Self::new()
    }
}

impl LocMapper<Loc, Loc, ILoc, ILoc> for IndexMapper {
    fn on_loc_annot(&mut self, loc: &Loc) -> Result<ILoc, !> {
        let c = self.counter;
        self.counter += 1;
        Ok(ILoc(loc.dupe(), c))
    }

    fn on_type_annot(&mut self, loc: &Loc) -> Result<ILoc, !> {
        let c = self.counter;
        self.counter += 1;
        Ok(ILoc(loc.dupe(), c))
    }
}

pub struct UnindexMapper;

impl LocMapper<ILoc, ILoc, Loc, Loc> for UnindexMapper {
    fn on_loc_annot(&mut self, iloc: &ILoc) -> Result<Loc, !> {
        let ILoc(loc, _id) = iloc;
        Ok(loc.dupe())
    }

    fn on_type_annot(&mut self, iloc: &ILoc) -> Result<Loc, !> {
        let ILoc(loc, _id) = iloc;
        Ok(loc.dupe())
    }
}
