/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! OCaml: flow/src/analysis/env_builder/__tests__/find_provider_test.ml
//!
//! Tests for the find_providers module.

use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_parser::ParseOptions;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::loc_sig::LocSig;
use flow_parser::parse_program_without_file;

use crate::find_providers::*;

/// Dedent and trim a string, removing empty lines and common indentation.
fn dedent_trim(s: &str) -> String {
    let lines: Vec<&str> = s.lines().filter(|line| !line.trim().is_empty()).collect();

    if lines.is_empty() {
        return String::new();
    }

    // Find minimum indentation
    let min_indent = lines
        .iter()
        .map(|line| line.chars().take_while(|c| *c == ' ').count())
        .min()
        .unwrap_or(0);

    // Remove common indentation
    lines
        .iter()
        .map(|line| {
            if line.len() >= min_indent {
                &line[min_indent..]
            } else {
                line
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Parse JavaScript source code with pattern matching and enums enabled.
fn parse(contents: &str) -> flow_parser::ast::Program<Loc, Loc> {
    let source = dedent_trim(contents);
    let parse_options = ParseOptions {
        enums: true,
        pattern_matching: true,
        records: true,
        ..Default::default()
    };

    let (ast, _errors) = parse_program_without_file(false, None, Some(parse_options), Ok(&source));
    ast
}

/// Format provider locations for test assertions.
fn print_providers(prov: Option<&FlowOrdMap<Loc, WriteKind>>) -> String {
    match prov {
        None => "[]".to_string(),
        Some(provider_locs) => {
            let locs: Vec<String> = provider_locs
                .keys()
                .map(|loc| loc.debug_to_string(false))
                .collect();
            if locs.is_empty() {
                "[]".to_string()
            } else {
                format!("[{}]", locs.join("], ["))
            }
        }
    }
}

/// Format providers for a definition (includes array providers).
fn print_providers_of_def(prov: Option<&Entry<Loc>>) -> String {
    match prov {
        None => "[]".to_string(),
        Some(entry) => {
            let normal_locs: Vec<String> = entry
                .provider_locs
                .writes
                .keys()
                .map(|loc| loc.debug_to_string(false))
                .collect();

            let mut result = if normal_locs.is_empty() {
                "[]".to_string()
            } else {
                format!("[{}]", normal_locs.join("], ["))
            };

            if !entry.provider_locs.array_writes.is_empty() {
                let array_locs: Vec<String> = entry
                    .provider_locs
                    .array_writes
                    .iter()
                    .map(|loc| loc.debug_to_string(false))
                    .collect();
                result.push_str(&format!(" array providers: [{}]", array_locs.join("], [")));
            }

            result
        }
    }
}

/// Create a test case that checks provider locations for a given variable.
fn mk_provider_test(var: &str, contents: &str, expected_msg: &str) {
    let ast = parse(contents);
    let env = compute_provider_env(&ast.statements);
    let msg = get_providers_for_toplevel_var(var, &env);
    let actual = print_providers(msg.as_ref());
    assert_eq!(
        actual, expected_msg,
        "Results don't match for variable '{}'\nExpected: {}\nActual: {}",
        var, expected_msg, actual
    );
}

/// Create a location from line and column (1-indexed line, 0-indexed column).
fn mk_loc(start_line: i32, start_col: i32, end_line: i32, end_col: i32) -> Loc {
    Loc {
        source: None,
        start: Position {
            line: start_line,
            column: start_col,
        },
        end: Position {
            line: end_line,
            column: end_col,
        },
    }
}

/// Create a test case that checks providers for a specific declaration location.
fn mk_provider_loc_test(
    start_line: i32,
    start_col: i32,
    end_line: i32,
    end_col: i32,
    contents: &str,
    expected_msg: &str,
) {
    let ast = parse(contents);
    let env = compute_provider_env(&ast.statements);

    // Find the entry at the given location (search both declare_locs and def_locs)
    let target_loc = mk_loc(start_line, start_col, end_line, end_col);
    let all = all_entries(&env);
    let entry = all
        .iter()
        .find(|e| e.declare_locs.contains(&target_loc) || e.def_locs.contains(&target_loc));

    let actual = print_providers_of_def(entry);
    assert_eq!(
        actual, expected_msg,
        "Results don't match for location ({}, {}) to ({}, {})\nExpected: {}\nActual: {}",
        start_line, start_col, end_line, end_col, expected_msg, actual
    );
}

#[test]
fn test_empty_to_null_to_init() {
    mk_provider_test(
        "x",
        "
var x;
x = null;
x = 42;
",
        "[(2, 0) to (2, 1)], [(3, 0) to (3, 1)]",
    );
}

#[test]
fn test_null_to_init2() {
    mk_provider_test(
        "x",
        "
let x = null;
x = 42;
x = 100;
",
        "[(1, 4) to (1, 5)], [(2, 0) to (2, 1)]",
    );
}

#[test]
fn test_empty_to_null_to_init_loc() {
    mk_provider_loc_test(
        4,
        0,
        4,
        1,
        "
var x;
x = null;
x = 42;
x = 10;
",
        "[(2, 0) to (2, 1)], [(3, 0) to (3, 1)]",
    );
}

#[test]
fn test_write_before_declare() {
    mk_provider_test(
        "x",
        "
x = 42;
var x = true;
",
        "[(2, 4) to (2, 5)]",
    );
}

#[test]
fn test_write_before_declare_2() {
    mk_provider_test(
        "x",
        "
x = 42;
var x = null;
",
        "[(1, 0) to (1, 1)], [(2, 4) to (2, 5)]",
    );
}

#[test]
fn test_shadow_let() {
    mk_provider_test(
        "x",
        "
let x;
{
  let x;
  x = 42;
}
x = null;
x = 42;
",
        "[(6, 0) to (6, 1)], [(7, 0) to (7, 1)]",
    );
}

#[test]
fn test_shadow_let_2() {
    mk_provider_test(
        "x",
        "
let x;
{
  x = 42;
  let x;
  x = 42;
}
x = null;
x = 42;
",
        "[(7, 0) to (7, 1)], [(8, 0) to (8, 1)]",
    );
}

#[test]
fn test_shadow_var() {
    mk_provider_test(
        "x",
        "
var x;
{
  var x;
  x = 42;
}
x = null;
x = 42;
",
        "[(4, 2) to (4, 3)]",
    );
}

#[test]
fn test_inner_scope() {
    mk_provider_test(
        "x",
        "
let x;
{
  x = 42;
}
",
        "[(3, 2) to (3, 3)]",
    );
}

#[test]
fn test_annotated() {
    mk_provider_test(
        "x",
        "
var x: ?number;
x = null;
x = 42;
",
        "[(1, 4) to (1, 5)]",
    );
}

#[test]
fn test_function() {
    mk_provider_test(
        "f",
        "
function f() {

}
",
        "[(1, 9) to (1, 10)]",
    );
}

#[test]
fn test_function_parameters() {
    mk_provider_test(
        "x",
        "
function f(x) {

}
",
        "[]",
    );
}

#[test]
fn test_if() {
    mk_provider_test(
        "x",
        "
var x = null;
var condition;
if (condition) {
  x = 42;
} else {
  x = 100;
}
x = true;
",
        "[(1, 4) to (1, 5)], [(4, 2) to (4, 3)], [(6, 2) to (6, 3)]",
    );
}

#[test]
fn test_if_onearmed() {
    mk_provider_test(
        "x",
        "
var x = null;
var condition;
if (condition) {
  x = 42;
}
x = true;
",
        "[(1, 4) to (1, 5)], [(4, 2) to (4, 3)]",
    );
}

#[test]
fn test_switch() {
    mk_provider_test(
        "x",
        "
var x = null;
var condition;
switch (condition) {
  case 'a':
    x = 1;
    break;
  case 'b':
    x = true;
    break;
  case 'c':
    x = 'hi';
    break;
  default:
    x = (x => x);
}
x = 100;
",
        "[(1, 4) to (1, 5)], [(5, 4) to (5, 5)], [(8, 4) to (8, 5)], [(11, 4) to (11, 5)], [(14, 4) to (14, 5)]",
    );
}

#[test]
fn test_match_expression_null() {
    mk_provider_test(
        "x",
        "
let x = null;
declare const arg: 1 | 2;
(match (arg) {
  1 => x = 42,
  2 => x = 100,
});
x = true;
",
        "[(1, 4) to (1, 5)], [(4, 7) to (4, 8)], [(5, 7) to (5, 8)]",
    );
}

#[test]
fn test_match_expression_array() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
let x = [];
declare const arg: 1 | 2;
(match (arg) {
  1 => x.push(42),
  2 => x.push(100),
});
x = true;
",
        "[(1, 4) to (1, 5)] array providers: [(4, 14) to (4, 16)], [(5, 14) to (5, 17)]",
    );
}

#[test]
fn test_match_statement_null() {
    mk_provider_test(
        "x",
        "
let x = null;
declare const arg: 1 | 2;
match (arg) {
  1 => {
    x = 42;
  }
  2 => {
    x = 100;
  }
}
x = false;
",
        "[(1, 4) to (1, 5)], [(5, 4) to (5, 5)], [(8, 4) to (8, 5)]",
    );
}

#[test]
fn test_match_statement_array() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
let x = [];
declare const arg: 1 | 2;
match (arg) {
  1 => {
    x.push(true);
  }
  2 => {
    x.push(false);
  }
}
x = false;
",
        "[(1, 4) to (1, 5)] array providers: [(5, 11) to (5, 15)], [(8, 11) to (8, 16)]",
    );
}

#[test]
fn test_try() {
    mk_provider_test(
        "x",
        "
var x = null;
try {
  x = 100;
} catch (e) {
  x = true
} finally {
  x = 'hi';
}
x = 42;
",
        "[(1, 4) to (1, 5)], [(3, 2) to (3, 3)], [(5, 2) to (5, 3)], [(7, 2) to (7, 3)]",
    );
}

#[test]
fn test_try_inscope() {
    mk_provider_test(
        "e",
        "
try {
} catch (e) {
}
",
        "[]",
    );
}

#[test]
fn test_fun_var1() {
    mk_provider_test(
        "ac",
        "
var ac = 42;
function ac() {}
",
        "[(1, 4) to (1, 6)]",
    );
}

#[test]
fn test_fun_var2() {
    mk_provider_test(
        "ac",
        "
function ac() {}
var ac = 42;
",
        "[(1, 9) to (1, 11)]",
    );
}

#[test]
fn test_undeclared() {
    mk_provider_test(
        "x",
        "
x = 10
",
        "[(1, 0) to (1, 1)]",
    );
}

#[test]
fn test_inc() {
    mk_provider_test(
        "x",
        "
var x;
x++;
",
        "[(2, 0) to (2, 1)]",
    );
}

#[test]
fn test_pluseq() {
    mk_provider_test(
        "x",
        "
var x;
x += 42;
",
        "[(2, 0) to (2, 1)]",
    );
}

#[test]
fn test_destruct1() {
    mk_provider_test(
        "x",
        "
var { a: x } = 10;
",
        "[(1, 9) to (1, 10)]",
    );
}

#[test]
fn test_destruct2() {
    mk_provider_test(
        "a",
        "
var { a: x } = 10;
",
        "[]",
    );
}

#[test]
fn test_loop1() {
    mk_provider_loc_test(
        1,
        9,
        1,
        10,
        "
for (var x of [1,2,3]) { };
",
        "[(1, 9) to (1, 10)]",
    );
}

#[test]
fn test_loop1a() {
    mk_provider_loc_test(
        1,
        9,
        1,
        10,
        "
for (var x of [1,2,3]) { x=3; };
",
        "[(1, 9) to (1, 10)]",
    );
}

#[test]
fn test_loop2() {
    mk_provider_test(
        "x",
        "
var x = null;
for (x of [1,2,3]) { };
",
        "[(1, 4) to (1, 5)], [(2, 5) to (2, 6)]",
    );
}

#[test]
fn test_loop3() {
    mk_provider_loc_test(
        1,
        9,
        1,
        10,
        "
for (var x in {a: 'a'}) { };
",
        "[(1, 9) to (1, 10)]",
    );
}

#[test]
fn test_loop3a() {
    mk_provider_loc_test(
        1,
        9,
        1,
        10,
        "
for (var x in {a: 'a'}) { x=3; };
",
        "[(1, 9) to (1, 10)]",
    );
}

#[test]
fn test_loop4() {
    mk_provider_test(
        "x",
        "
var x = null;
for (x in {a: 'a'}) { };
",
        "[(1, 4) to (1, 5)], [(2, 5) to (2, 6)]",
    );
}

#[test]
fn test_class_expr1_base() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
let w;

w = class w { m() { w = 42 }};
",
        "[(2, 0) to (2, 1)]",
    );
}

#[test]
fn test_class_expr2_base() {
    mk_provider_loc_test(
        2,
        20,
        2,
        21,
        "
let w;

w = class w { m() { w = 42 }};

",
        "[(2, 10) to (2, 11)]",
    );
}

#[test]
fn test_destructuring_1() {
    mk_provider_loc_test(
        2,
        1,
        2,
        2,
        "
var [a,b]: ?number = [];
[a] = null;
",
        "[(1, 5) to (1, 6)]",
    );
}

#[test]
fn test_destructuring_2() {
    mk_provider_loc_test(
        2,
        2,
        2,
        3,
        "
var [a,b]: ?number = [];
{[a] = null};
",
        "[(1, 5) to (1, 6)]",
    );
}

#[test]
fn test_same_generic_scope() {
    mk_provider_loc_test(
        2,
        10,
        2,
        11,
        "
function f() {
      var x;
      x = 42; // provider
}
",
        "[(3, 6) to (3, 7)]",
    );
}

#[test]
fn test_nested1() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x;
function f() {
      x = 42; // provider
}
",
        "[(3, 6) to (3, 7)]",
    );
}

#[test]
fn test_nested1_generic() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x;
function f<T>() {
      x = 42; // provider
}
",
        "[]",
    );
}

#[test]
fn test_nested2() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = null; // provider
function f() {
      x = 42; // provider
}
",
        "[(1, 4) to (1, 5)], [(3, 6) to (3, 7)]",
    );
}

#[test]
fn test_nested2_generic() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = null; // provider
function f<T>() {
      x = 42; // provider
}
",
        "[(1, 4) to (1, 5)]",
    );
}

#[test]
fn test_nested3() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x;
function f() {
      x = 42; // provider
}
x = null // provider
",
        "[(3, 6) to (3, 7)], [(5, 0) to (5, 1)]",
    );
}

#[test]
fn test_nested4() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x;
function f() {
      x = 42;
}
x = 42; // provider
",
        "[(5, 0) to (5, 1)]",
    );
}

#[test]
fn test_nested5() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x;
function f() {
      x = null;
}
x = 42 // provider
",
        "[(5, 0) to (5, 1)]",
    );
}

#[test]
fn test_nested6() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x;
if (condition) {
      function f() {
            x = 42; // provider
      }
} else {
      x = null // provider
}
",
        "[(4, 12) to (4, 13)], [(7, 6) to (7, 7)]",
    );
}

#[test]
fn test_nested6a() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x;
if (condition) {
      function f() {
            x = 42;
      }
} else {
      x = 42 // provider
}
",
        "[(7, 6) to (7, 7)]",
    );
}

#[test]
fn test_nested7() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x;
if (condition) {
      x = null // provider
      function f() {
            x = 42;
      }
} else {
      x = 42 // provider
}
",
        "[(3, 6) to (3, 7)], [(8, 6) to (8, 7)]",
    );
}

#[test]
fn test_nested8() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x;
if (condition) {
      function f() {
            x = 42; // provider
      }
} else {
      function f() {
            function g() {
                  x = 42;
            }
      }
}
",
        "[(4, 12) to (4, 13)]",
    );
}

#[test]
fn test_nested9() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x;
if (condition) {
      function f() {
            x = null; // provider
      }
} else {
      function f() {
            function g() {
                  x = 42; // provider
            }
      }
}
",
        "[(4, 12) to (4, 13)], [(9, 18) to (9, 19)]",
    );
}

#[test]
fn test_class_expr1() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
let w;
function f() {
  w = class w { m() { w = 42 }};
}
",
        "[(3, 2) to (3, 3)]",
    );
}

#[test]
fn test_class_expr2() {
    mk_provider_loc_test(
        3,
        22,
        3,
        23,
        "
let w;
function f() {
  w = class w { m() { w = 42 }};
}
",
        "[(3, 12) to (3, 13)]",
    );
}

#[test]
fn test_generic_class_1() {
    mk_provider_loc_test(
        3,
        12,
        3,
        13,
        "
let w;
class C<T> {
  meth() { w = 1; }
}
",
        "[]",
    );
}

#[test]
fn test_generic_class_2() {
    mk_provider_loc_test(
        4,
        4,
        4,
        5,
        "
class C<T> {
  meth() {
    let w;
    w = 1;
  }
}
",
        "[(4, 4) to (4, 5)]",
    );
}

#[test]
fn test_extend_state_opt1() {
    mk_provider_test(
        "x", "
var x;
", "[]",
    );
}

#[test]
fn test_extend_state_opt2() {
    mk_provider_test(
        "x",
        "
var x: string; // p
var x: number;
",
        "[(1, 4) to (1, 5)]",
    );
}

#[test]
fn test_extend_state_opt3() {
    mk_provider_test(
        "x",
        "
var x = 42;
var x: string; // p
",
        "[(2, 4) to (2, 5)]",
    );
}

#[test]
fn test_extend_state_opt4() {
    mk_provider_test(
        "x",
        "
var x;
x = null; // p
",
        "[(2, 0) to (2, 1)]",
    );
}

#[test]
fn test_extend_state_opt5() {
    mk_provider_test(
        "x",
        "
var x;
x = 42; // p
",
        "[(2, 0) to (2, 1)]",
    );
}

#[test]
fn test_extend_state_opt6() {
    mk_provider_test(
        "x",
        "
var x = null; // p
x = 42; // p
",
        "[(1, 4) to (1, 5)], [(2, 0) to (2, 1)]",
    );
}

#[test]
fn test_extend_state_opt7() {
    mk_provider_test(
        "x",
        "
var x;
function f() { x = null; }
x = 42; // p
",
        "[(3, 0) to (3, 1)]",
    );
}

#[test]
fn test_extend_state_opt8() {
    mk_provider_test(
        "x",
        "
var x = null; // p
x = null;
",
        "[(1, 4) to (1, 5)]",
    );
}

#[test]
fn test_extend_state_opt9() {
    mk_provider_test(
        "x",
        "
var x;
function f() { x = null; }
x = null; // p
",
        "[(3, 0) to (3, 1)]",
    );
}

#[test]
fn test_extend_state_opt10() {
    mk_provider_test(
        "x",
        "
var x = null; // p
x = 42; // p
x = null
",
        "[(1, 4) to (1, 5)], [(2, 0) to (2, 1)]",
    );
}

#[test]
fn test_extend_state_opt11() {
    mk_provider_test(
        "x",
        "
var x;
function f() {
      x = null;
      x = 42; // p
}
x = null; // p
",
        "[(4, 6) to (4, 7)], [(6, 0) to (6, 1)]",
    );
}

#[test]
fn test_extend_state_opt12() {
    mk_provider_test(
        "x",
        "
var x = 42; // p
x = null
",
        "[(1, 4) to (1, 5)]",
    );
}

#[test]
fn test_extend_state_opt13() {
    mk_provider_test(
        "x",
        "
var x;
function f() {
      x = 42 // p
}
x = null; // p
",
        "[(3, 6) to (3, 7)], [(5, 0) to (5, 1)]",
    );
}

#[test]
fn test_extend_state_opt14() {
    mk_provider_test(
        "x",
        "
var x = 42; // p
x = 'a';
",
        "[(1, 4) to (1, 5)]",
    );
}

#[test]
fn test_extend_state_opt15() {
    mk_provider_test(
        "x",
        "
var x = null; // p
function f() {
      x = 42
}
x = 'a'; // p
",
        "[(1, 4) to (1, 5)], [(5, 0) to (5, 1)]",
    );
}

#[test]
fn test_extend_state_opt16() {
    mk_provider_test(
        "x",
        "
var x;
function f() {
      x = null;
      x = 42;
}
x = 'a'; // p
",
        "[(6, 0) to (6, 1)]",
    );
}

#[test]
fn test_extend_state_opt17() {
    mk_provider_test(
        "x",
        "
var x;
function f() {
      x = 42;
}
x = 'a'; // p
",
        "[(5, 0) to (5, 1)]",
    );
}

#[test]
fn test_annot_no_init() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
let w: number; //provider
function f() {
  w = 10;
}
",
        "[(1, 4) to (1, 5)]",
    );
}

#[test]
fn test_function_same_name() {
    mk_provider_loc_test(
        3,
        15,
        3,
        16,
        "
function f() { }
if (condition) {
      function f() { }
}
",
        "[(3, 15) to (3, 16)]",
    );
}

#[test]
fn test_switch1() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
let w;
switch (bar) {
  case 'foo':
        let w;
        w = 10;
}
w = 42; // provider for external
",
        "[(7, 0) to (7, 1)]",
    );
}

#[test]
fn test_switch2() {
    mk_provider_loc_test(
        4,
        12,
        4,
        13,
        "
let w;
switch (bar) {
  case 'foo':
        let w;
        w = 10; // provider for internal
}
w = 42;
",
        "[(5, 8) to (5, 9)]",
    );
}

#[test]
fn test_declared_function() {
    mk_provider_loc_test(
        2,
        17,
        2,
        18,
        "
declare function f(): number;
declare function f(x: string): string;
function f(x: any): any { return null }
",
        "[(1, 17) to (1, 18)], [(2, 17) to (2, 18)]",
    );
}

#[test]
fn test_arr1() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = [];
x.push(42);
x.push(100);
",
        "[(1, 4) to (1, 5)] array providers: [(2, 7) to (2, 9)]",
    );
}

#[test]
fn test_arr2() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = [];
x.push(42);
x = [100]
",
        "[(1, 4) to (1, 5)] array providers: [(2, 7) to (2, 9)]",
    );
}

#[test]
fn test_arr3() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = [];
x = [100]
x.push(42);
",
        "[(1, 4) to (1, 5)], [(2, 0) to (2, 1)]",
    );
}

#[test]
fn test_arr4() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = [];
function f() {
      x.push(1)
}
x.push(2)
",
        "[(1, 4) to (1, 5)] array providers: [(5, 7) to (5, 8)]",
    );
}

#[test]
fn test_arr5() {
    mk_provider_loc_test(
        3,
        4,
        3,
        11,
        "
declare var noop : <T>(arr: Array<Array<T>>) => void;
declare var arr : Array<Array<?string>>;
let new_arr = [];
arr.forEach(x => { new_arr.push(x) });
new_arr = new_arr.filter(Boolean);
noop<string>(new_arr);
",
        "[(3, 4) to (3, 11)] array providers: [(4, 32) to (4, 33)]",
    );
}

#[test]
fn test_arr6() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = [];
function f() {
      x = [10]
}
x[0] = 2
",
        "[(1, 4) to (1, 5)] array providers: [(5, 7) to (5, 8)]",
    );
}

#[test]
fn test_arr7() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = [];
x = [10];
",
        "[(1, 4) to (1, 5)], [(2, 0) to (2, 1)]",
    );
}

#[test]
fn test_arr8() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = [];
x.push(42);
x = [10];
",
        "[(1, 4) to (1, 5)] array providers: [(2, 7) to (2, 9)]",
    );
}

#[test]
fn test_arr9() {
    mk_provider_loc_test(
        2,
        6,
        2,
        7,
        "
function foo() {
  var x = [];
  x.push(42);
}
",
        "[(2, 6) to (2, 7)] array providers: [(3, 9) to (3, 11)]",
    );
}

#[test]
fn test_arr10() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = [];
function foo() {
  x.push(42);
}
",
        "[(1, 4) to (1, 5)] array providers: [(3, 9) to (3, 11)]",
    );
}

#[test]
fn test_arr11() {
    mk_provider_loc_test(
        1,
        4,
        1,
        5,
        "
var x = [];
function foo<T>() {
  x.push(42);
}
",
        "[(1, 4) to (1, 5)]",
    );
}
