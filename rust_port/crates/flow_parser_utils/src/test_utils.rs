/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_aloc::ALoc;
use flow_parser::ParseOptions;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_parser::parse_program_without_file;
use flow_parser::polymorphic_ast_mapper;

use crate::ast_loc_utils::LocToALocMapper;

pub fn parse(contents: &str) -> ast::Program<Loc, Loc> {
    let parse_options = Some(ParseOptions {
        enums: true,
        ..ParseOptions::default()
    });
    let (ast, _errors) = parse_program_without_file(true, None, parse_options, Ok(contents));
    ast
}

pub fn mk_loc((start_line, start_column): (i32, i32), (end_line, end_column): (i32, i32)) -> Loc {
    Loc::mk(None, start_line, start_column, end_line, end_column)
}

pub fn mk_aloc(start: (i32, i32), finish: (i32, i32)) -> ALoc {
    ALoc::of_loc(mk_loc(start, finish))
}

pub fn print_list<T>(printer: impl Fn(&T) -> String, list: &[T]) -> String {
    list.iter().map(printer).collect::<Vec<_>>().join(", ")
}

pub fn eq<T>(printer: impl Fn(&T) -> String, v1: &T, v2: &T) -> bool {
    printer(v1) == printer(v2)
}

pub fn parse_with_alocs(contents: &str) -> ast::Program<ALoc, ALoc> {
    let loc_ast = parse(contents);
    let mut loc_to_aloc_mapper = LocToALocMapper;
    let Ok(aloc_ast) = polymorphic_ast_mapper::program(&mut loc_to_aloc_mapper, &loc_ast);
    aloc_ast
}
