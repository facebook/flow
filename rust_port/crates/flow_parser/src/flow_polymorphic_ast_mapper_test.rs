/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::loc::Loc;
use crate::parse_program_without_file;
use crate::polymorphic_ast_mapper;
use crate::polymorphic_ast_mapper::LocMapper;

struct Mapper;

impl LocMapper<Loc, Loc, Loc, Loc> for Mapper {
    fn on_loc_annot(&mut self, x: &Loc) -> Result<Loc, !> {
        Ok(x.dupe())
    }

    fn on_type_annot(&mut self, x: &Loc) -> Result<Loc, !> {
        Ok(x.dupe())
    }
}

// these tests don't do much other than check that the mapper doesn't raise exceptions
fn run_mapper(source: &str) {
    let (ast, _errors) = parse_program_without_file(true, None, None, Ok(source));
    let mut mapper = Mapper;
    let Ok(_mapped) = polymorphic_ast_mapper::program(&mut mapper, &ast);
}

#[test]
fn simple() {
    let source = "function foo() { (5 * 3); 4; (6 + 4); }";
    run_mapper(source);
}
