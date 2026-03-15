/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_parser::loc::Loc;
use flow_parser::polymorphic_ast_mapper::LocMapper;

pub struct LocToALocMapper;

impl LocMapper<Loc, Loc, ALoc, ALoc> for LocToALocMapper {
    fn on_loc_annot(&mut self, loc: &Loc) -> Result<ALoc, !> {
        Ok(ALoc::of_loc(loc.dupe()))
    }

    fn on_type_annot(&mut self, annot: &Loc) -> Result<ALoc, !> {
        Ok(ALoc::of_loc(annot.dupe()))
    }
}
