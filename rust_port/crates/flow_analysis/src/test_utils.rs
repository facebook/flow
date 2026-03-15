/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_aloc::ALoc;
use flow_aloc::LocToALocMapper;
use flow_parser::ParseOptions;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::parse_program_without_file;
use flow_parser::polymorphic_ast_mapper;

pub fn parse(contents: &str) -> ast::Program<Loc, Loc> {
    let parse_options = Some(ParseOptions {
        enums: true,
        components: true,
        pattern_matching: true,
        records: true,
        ..Default::default()
    });
    let (ast, _errors) = parse_program_without_file(false, None, parse_options, Ok(contents));
    ast
}

pub fn mk_loc(start: (i32, i32), finish: (i32, i32)) -> Loc {
    Loc {
        source: None,
        start: Position {
            line: start.0,
            column: start.1,
        },
        end: Position {
            line: finish.0,
            column: finish.1,
        },
    }
}

pub fn mk_aloc(start: (i32, i32), finish: (i32, i32)) -> ALoc {
    ALoc::of_loc(mk_loc(start, finish))
}

pub fn print_list<T, F>(printer: F, list: &[T]) -> String
where
    F: Fn(&T) -> String,
{
    list.iter().map(printer).collect::<Vec<_>>().join(", ")
}

// let eq printer v1 v2 = printer v1 = printer v2
pub fn eq<T, F>(printer: F, v1: &T, v2: &T) -> bool
where
    F: Fn(&T) -> String,
{
    printer(v1) == printer(v2)
}

pub fn parse_with_alocs(contents: &str) -> ast::Program<ALoc, ALoc> {
    let loc_ast = parse(contents);
    let mut mapper = LocToALocMapper;
    let Ok(result) = polymorphic_ast_mapper::program(&mut mapper, &loc_ast);
    result
}
