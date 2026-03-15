/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use flow_common::reason::Name;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::mk_reason;
use flow_parser::ParseOptions;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::parse_program_without_file;
use pretty_assertions::assert_eq;

use crate::ssa_api::Values;
use crate::ssa_api::WriteLoc;
use crate::ssa_api::print_values;
use crate::ssa_builder::program;

fn mk_loc(start: (i32, i32), end: (i32, i32)) -> Loc {
    Loc {
        source: None,
        start: Position {
            line: start.0,
            column: start.1,
        },
        end: Position {
            line: end.0,
            column: end.1,
        },
    }
}

fn parse(contents: &str) -> ast::Program<Loc, Loc> {
    let options = ParseOptions {
        enums: true,
        components: true,
        ..Default::default()
    };
    let (ast, _errors) = parse_program_without_file(true, None, Some(options), Ok(contents));
    ast
}

fn mk_write(start: (i32, i32), end: (i32, i32), name: &str) -> WriteLoc<Loc> {
    let loc = mk_loc(start, end);
    let reason = mk_reason(VirtualReasonDesc::RIdentifier(Name::new(name)), loc);
    WriteLoc::Write(reason)
}

fn mk_ssa_builder_test(enable_enums: bool, contents: &str, expected_values: Values<Loc>) {
    let values = program(enable_enums, &parse(contents));
    assert_eq!(
        print_values(&expected_values),
        print_values(&values),
        "SSA values don't match!"
    );
}

fn values_from_entries(entries: Vec<(Loc, Vec<WriteLoc<Loc>>)>) -> Values<Loc> {
    let mut map = BTreeMap::new();
    for (loc, writes) in entries {
        map.insert(loc, writes);
    }
    Values(map)
}

#[test]
fn test_var() {
    mk_ssa_builder_test(
        false,
        "function foo(x) {
       var y;
       if (x) y = 123;
       return y;
     }",
        values_from_entries(vec![
            (
                mk_loc((3, 11), (3, 12)),
                vec![mk_write((1, 13), (1, 14), "x")],
            ),
            (
                mk_loc((4, 14), (4, 15)),
                vec![WriteLoc::Uninitialized, mk_write((3, 14), (3, 15), "y")],
            ),
        ]),
    );
}

#[test]
fn test_var_hoist() {
    mk_ssa_builder_test(
        false,
        "function foo(x) {
       y = x;
       var y;
       return y;
     }",
        values_from_entries(vec![
            (
                mk_loc((2, 11), (2, 12)),
                vec![mk_write((1, 13), (1, 14), "x")],
            ),
            (
                mk_loc((4, 14), (4, 15)),
                vec![mk_write((2, 7), (2, 8), "y")],
            ),
        ]),
    );
}

#[test]
fn test_let() {
    mk_ssa_builder_test(
        false,
        "function foo() { let x = 0; return x; }",
        values_from_entries(vec![(
            mk_loc((1, 35), (1, 36)),
            vec![mk_write((1, 21), (1, 22), "x")],
        )]),
    );
}

#[test]
fn test_let_update() {
    mk_ssa_builder_test(
        false,
        "function foo() { let x = 0; x++; return x; }",
        values_from_entries(vec![
            (
                mk_loc((1, 28), (1, 29)),
                vec![mk_write((1, 21), (1, 22), "x")],
            ),
            (
                mk_loc((1, 40), (1, 41)),
                vec![mk_write((1, 28), (1, 29), "x")],
            ),
        ]),
    );
}

#[test]
fn test_if() {
    mk_ssa_builder_test(
        false,
        "(function() { var xxx = 0; let yyy = 1; if (yyy) { yyy = 2; } else { } xxx = yyy; })",
        values_from_entries(vec![
            (
                mk_loc((1, 44), (1, 47)),
                vec![mk_write((1, 31), (1, 34), "yyy")],
            ),
            (
                mk_loc((1, 77), (1, 80)),
                vec![
                    mk_write((1, 31), (1, 34), "yyy"),
                    mk_write((1, 51), (1, 54), "yyy"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_if_let() {
    mk_ssa_builder_test(
        false,
        "(function() { var xxx = 0; let yyy = 1; if (yyy) { let yyy = 2; } else { } xxx = yyy; })",
        values_from_entries(vec![
            (
                mk_loc((1, 44), (1, 47)),
                vec![mk_write((1, 31), (1, 34), "yyy")],
            ),
            (
                mk_loc((1, 81), (1, 84)),
                vec![mk_write((1, 31), (1, 34), "yyy")],
            ),
        ]),
    );
}

#[test]
fn test_while() {
    mk_ssa_builder_test(
        false,
        "(function() { var xxx = 0; let yyy = 1; while (yyy) { yyy = 2; } xxx = yyy; })",
        values_from_entries(vec![
            (
                mk_loc((1, 47), (1, 50)),
                vec![
                    mk_write((1, 31), (1, 34), "yyy"),
                    mk_write((1, 54), (1, 57), "yyy"),
                ],
            ),
            (
                mk_loc((1, 71), (1, 74)),
                vec![
                    mk_write((1, 31), (1, 34), "yyy"),
                    mk_write((1, 54), (1, 57), "yyy"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_while_let() {
    mk_ssa_builder_test(
        false,
        "(function() { var xxx = 0; let yyy = 1; while (yyy) { let yyy = 2; } xxx = yyy; })",
        values_from_entries(vec![
            (
                mk_loc((1, 47), (1, 50)),
                vec![mk_write((1, 31), (1, 34), "yyy")],
            ),
            (
                mk_loc((1, 75), (1, 78)),
                vec![mk_write((1, 31), (1, 34), "yyy")],
            ),
        ]),
    );
}

#[test]
fn test_for() {
    mk_ssa_builder_test(
        false,
        "(function() { var xxx = 0; let yyy = 1; for (var zzz = 0; zzz < 3; zzz = zzz + 1) { yyy = 2; } xxx = yyy; })",
        values_from_entries(vec![
            (
                mk_loc((1, 58), (1, 61)),
                vec![
                    mk_write((1, 49), (1, 52), "zzz"),
                    mk_write((1, 67), (1, 70), "zzz"),
                ],
            ),
            (
                mk_loc((1, 73), (1, 76)),
                vec![
                    mk_write((1, 49), (1, 52), "zzz"),
                    mk_write((1, 67), (1, 70), "zzz"),
                ],
            ),
            (
                mk_loc((1, 101), (1, 104)),
                vec![
                    mk_write((1, 31), (1, 34), "yyy"),
                    mk_write((1, 84), (1, 87), "yyy"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_for_let() {
    mk_ssa_builder_test(
        false,
        "(function() { var xxx = 0; let yyy = 1; for (let zzz = 0; zzz < 3; zzz = zzz + 1) { yyy = 2; } xxx = yyy; })",
        values_from_entries(vec![
            (
                mk_loc((1, 58), (1, 61)),
                vec![
                    mk_write((1, 49), (1, 52), "zzz"),
                    mk_write((1, 67), (1, 70), "zzz"),
                ],
            ),
            (
                mk_loc((1, 73), (1, 76)),
                vec![
                    mk_write((1, 49), (1, 52), "zzz"),
                    mk_write((1, 67), (1, 70), "zzz"),
                ],
            ),
            (
                mk_loc((1, 101), (1, 104)),
                vec![
                    mk_write((1, 31), (1, 34), "yyy"),
                    mk_write((1, 84), (1, 87), "yyy"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_switch() {
    mk_ssa_builder_test(
        false,
        "(function() { var a = 0; switch (a + 1) { case a: a = a + 1; case a + 1: a = a + 1; default: a = a + 1; } return a; })",
        values_from_entries(vec![
            (
                mk_loc((1, 33), (1, 34)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 47), (1, 48)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 54), (1, 55)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 66), (1, 67)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 77), (1, 78)),
                vec![
                    mk_write((1, 18), (1, 19), "a"),
                    mk_write((1, 50), (1, 51), "a"),
                ],
            ),
            (
                mk_loc((1, 97), (1, 98)),
                vec![
                    mk_write((1, 18), (1, 19), "a"),
                    mk_write((1, 73), (1, 74), "a"),
                ],
            ),
            (
                mk_loc((1, 113), (1, 114)),
                vec![
                    mk_write((1, 18), (1, 19), "a"),
                    mk_write((1, 93), (1, 94), "a"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_try() {
    mk_ssa_builder_test(
        false,
        "(function() { var xxx = 0; let yyy = 1; try { yyy = 2; } catch (e) { xxx = yyy; } return xxx; })",
        values_from_entries(vec![
            (
                mk_loc((1, 75), (1, 78)),
                vec![
                    mk_write((1, 31), (1, 34), "yyy"),
                    mk_write((1, 46), (1, 49), "yyy"),
                ],
            ),
            (
                mk_loc((1, 89), (1, 92)),
                vec![
                    mk_write((1, 18), (1, 21), "xxx"),
                    mk_write((1, 69), (1, 72), "xxx"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_closure() {
    mk_ssa_builder_test(
        false,
        "(function() { var xxx = 0; let yyy = 1; function foo() { xxx = yyy; } yyy = 2; foo(); return xxx; })",
        values_from_entries(vec![
            (
                mk_loc((1, 63), (1, 66)),
                vec![
                    WriteLoc::Uninitialized,
                    mk_write((1, 31), (1, 34), "yyy"),
                    mk_write((1, 70), (1, 73), "yyy"),
                ],
            ),
            (
                mk_loc((1, 79), (1, 82)),
                vec![mk_write((1, 49), (1, 52), "foo")],
            ),
            (
                mk_loc((1, 93), (1, 96)),
                vec![
                    mk_write((1, 18), (1, 21), "xxx"),
                    mk_write((1, 57), (1, 60), "xxx"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_break_while() {
    mk_ssa_builder_test(
        false,
        "(function() { var x = 0; while (x) { x = 1; break; x; x = 2; } return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 32), (1, 33)),
                vec![mk_write((1, 18), (1, 19), "x")],
            ),
            (
                mk_loc((1, 70), (1, 71)),
                vec![
                    mk_write((1, 18), (1, 19), "x"),
                    mk_write((1, 37), (1, 38), "x"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_continue_while() {
    mk_ssa_builder_test(
        false,
        "(function() { var x = 0; while (x) { x = 1; continue; x; x = 2; } return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 32), (1, 33)),
                vec![
                    mk_write((1, 18), (1, 19), "x"),
                    mk_write((1, 37), (1, 38), "x"),
                ],
            ),
            (
                mk_loc((1, 73), (1, 74)),
                vec![
                    mk_write((1, 18), (1, 19), "x"),
                    mk_write((1, 37), (1, 38), "x"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_break_for() {
    mk_ssa_builder_test(
        false,
        "(function() { for (var x = 0; x < 10; x++) { x = 1; break; x; x = 2; } return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 30), (1, 31)),
                vec![mk_write((1, 23), (1, 24), "x")],
            ),
            (
                mk_loc((1, 78), (1, 79)),
                vec![
                    mk_write((1, 23), (1, 24), "x"),
                    mk_write((1, 45), (1, 46), "x"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_continue_for() {
    mk_ssa_builder_test(
        false,
        "(function() { for (var x = 0; x < 10; x++) { x = 1; continue; x; x = 2; } return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 30), (1, 31)),
                vec![
                    mk_write((1, 23), (1, 24), "x"),
                    mk_write((1, 38), (1, 39), "x"),
                ],
            ),
            (
                mk_loc((1, 38), (1, 39)),
                vec![mk_write((1, 45), (1, 46), "x")],
            ),
            (
                mk_loc((1, 81), (1, 82)),
                vec![
                    mk_write((1, 23), (1, 24), "x"),
                    mk_write((1, 38), (1, 39), "x"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_break_switch() {
    mk_ssa_builder_test(
        false,
        "(function() { var a = 0; switch (a + 1) { case a: a = a + 1; break; case a + 1: a = a + 1; default: a = a + 1; } return a; })",
        values_from_entries(vec![
            (
                mk_loc((1, 33), (1, 34)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 47), (1, 48)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 54), (1, 55)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 73), (1, 74)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 84), (1, 85)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 104), (1, 105)),
                vec![
                    mk_write((1, 18), (1, 19), "a"),
                    mk_write((1, 80), (1, 81), "a"),
                ],
            ),
            (
                mk_loc((1, 120), (1, 121)),
                vec![
                    mk_write((1, 18), (1, 19), "a"),
                    mk_write((1, 50), (1, 51), "a"),
                    mk_write((1, 100), (1, 101), "a"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_break_labeled() {
    mk_ssa_builder_test(
        false,
        "(function() { var a = 0; L: { a = a + 1; break L; a = a + 1; } return a; })",
        values_from_entries(vec![
            (
                mk_loc((1, 34), (1, 35)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 70), (1, 71)),
                vec![mk_write((1, 30), (1, 31), "a")],
            ),
        ]),
    );
}

#[test]
fn test_break_labeled_while() {
    mk_ssa_builder_test(
        false,
        "(function() { var x = 0; L: while (x) { x = 1; break L; x; x = 2; } return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 35), (1, 36)),
                vec![mk_write((1, 18), (1, 19), "x")],
            ),
            (
                mk_loc((1, 75), (1, 76)),
                vec![
                    mk_write((1, 18), (1, 19), "x"),
                    mk_write((1, 40), (1, 41), "x"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_break_if() {
    mk_ssa_builder_test(
        false,
        "(function() { var a = 0; L: { a = a + 1; if (a) break L; else break L; a = a + 1; } return a; })",
        values_from_entries(vec![
            (
                mk_loc((1, 34), (1, 35)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 45), (1, 46)),
                vec![mk_write((1, 30), (1, 31), "a")],
            ),
            (
                mk_loc((1, 91), (1, 92)),
                vec![mk_write((1, 30), (1, 31), "a")],
            ),
        ]),
    );
}

#[test]
fn test_break_if_partial() {
    mk_ssa_builder_test(
        false,
        "(function() { var a = 0; L: { a = a + 1; if (a) break L; a = a + 1; } return a; })",
        values_from_entries(vec![
            (
                mk_loc((1, 34), (1, 35)),
                vec![mk_write((1, 18), (1, 19), "a")],
            ),
            (
                mk_loc((1, 45), (1, 46)),
                vec![mk_write((1, 30), (1, 31), "a")],
            ),
            (
                mk_loc((1, 61), (1, 62)),
                vec![mk_write((1, 30), (1, 31), "a")],
            ),
            (
                mk_loc((1, 77), (1, 78)),
                vec![
                    mk_write((1, 30), (1, 31), "a"),
                    mk_write((1, 57), (1, 58), "a"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_continue_if_partial() {
    mk_ssa_builder_test(
        false,
        "(function() { var a = 0; while (a) { a = a + 1; if (a) continue; a = a + 1; } return a; })",
        values_from_entries(vec![
            (
                mk_loc((1, 32), (1, 33)),
                vec![
                    mk_write((1, 18), (1, 19), "a"),
                    mk_write((1, 37), (1, 38), "a"),
                    mk_write((1, 65), (1, 66), "a"),
                ],
            ),
            (
                mk_loc((1, 41), (1, 42)),
                vec![
                    mk_write((1, 18), (1, 19), "a"),
                    mk_write((1, 37), (1, 38), "a"),
                    mk_write((1, 65), (1, 66), "a"),
                ],
            ),
            (
                mk_loc((1, 52), (1, 53)),
                vec![mk_write((1, 37), (1, 38), "a")],
            ),
            (
                mk_loc((1, 69), (1, 70)),
                vec![mk_write((1, 37), (1, 38), "a")],
            ),
            (
                mk_loc((1, 85), (1, 86)),
                vec![
                    mk_write((1, 18), (1, 19), "a"),
                    mk_write((1, 37), (1, 38), "a"),
                    mk_write((1, 65), (1, 66), "a"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_continue_labeled_while() {
    mk_ssa_builder_test(
        false,
        "(function() { var x = 0; L: while (x) { x = 1; continue L; x; x = 2; } return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 35), (1, 36)),
                vec![
                    mk_write((1, 18), (1, 19), "x"),
                    mk_write((1, 40), (1, 41), "x"),
                ],
            ),
            (
                mk_loc((1, 78), (1, 79)),
                vec![
                    mk_write((1, 18), (1, 19), "x"),
                    mk_write((1, 40), (1, 41), "x"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_continue_labeled_do_while() {
    mk_ssa_builder_test(
        false,
        "(function() { var x = 0; L: do { x = 1; continue L; x; x = 2; } while (x) return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 71), (1, 72)),
                vec![mk_write((1, 33), (1, 34), "x")],
            ),
            (
                mk_loc((1, 81), (1, 82)),
                vec![mk_write((1, 33), (1, 34), "x")],
            ),
        ]),
    );
}

#[test]
fn test_labeled_break_do_while() {
    mk_ssa_builder_test(
        false,
        "(function() { var x = 0; L: { do { x = 1; break L; x = 2; } while (true); x = 3; } return x; })",
        values_from_entries(vec![(
            mk_loc((1, 90), (1, 91)),
            vec![mk_write((1, 35), (1, 36), "x")],
        )]),
    );
}

#[test]
fn test_labeled_break_try_catch() {
    mk_ssa_builder_test(
        false,
        "(function() { var x = 0; L: try { x = x + 1; } catch (e) { x = e + 1; break L; } finally { x = x + 1; } return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 38), (1, 39)),
                vec![mk_write((1, 18), (1, 19), "x")],
            ),
            (
                mk_loc((1, 63), (1, 64)),
                vec![mk_write((1, 54), (1, 55), "e")],
            ),
            (
                mk_loc((1, 95), (1, 96)),
                vec![
                    mk_write((1, 18), (1, 19), "x"),
                    mk_write((1, 34), (1, 35), "x"),
                    mk_write((1, 59), (1, 60), "x"),
                    mk_write((1, 91), (1, 92), "x"),
                ],
            ),
            (
                mk_loc((1, 111), (1, 112)),
                vec![mk_write((1, 91), (1, 92), "x")],
            ),
        ]),
    );
}

#[test]
fn test_nested_labeled_break_try_catch() {
    mk_ssa_builder_test(
        false,
        "(function() { var x = 0; M: { L: { try { x = x + 1; } catch (e) { x = e + 1; break L; } finally { x = x + 1; break M; } } x = x + 1; } return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 45), (1, 46)),
                vec![mk_write((1, 18), (1, 19), "x")],
            ),
            (
                mk_loc((1, 70), (1, 71)),
                vec![mk_write((1, 61), (1, 62), "e")],
            ),
            (
                mk_loc((1, 102), (1, 103)),
                vec![
                    mk_write((1, 18), (1, 19), "x"),
                    mk_write((1, 41), (1, 42), "x"),
                    mk_write((1, 66), (1, 67), "x"),
                    mk_write((1, 98), (1, 99), "x"),
                ],
            ),
            (
                mk_loc((1, 142), (1, 143)),
                vec![mk_write((1, 98), (1, 99), "x")],
            ),
        ]),
    );
}

#[test]
fn test_throw() {
    mk_ssa_builder_test(
        false,
        "(function() { var x = 0; if (x) { x = 1; throw x; x = 2; } return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 29), (1, 30)),
                vec![mk_write((1, 18), (1, 19), "x")],
            ),
            (
                mk_loc((1, 47), (1, 48)),
                vec![mk_write((1, 34), (1, 35), "x")],
            ),
            (
                mk_loc((1, 66), (1, 67)),
                vec![mk_write((1, 18), (1, 19), "x")],
            ),
        ]),
    );
}

#[test]
fn test_nested_while() {
    mk_ssa_builder_test(
        false,
        "(function() { var x = 0; while (x) { x = 1; if (x) { break; } x = 2; while (x) { break; } x = x + 1 } return x; })",
        values_from_entries(vec![
            (
                mk_loc((1, 32), (1, 33)),
                vec![
                    mk_write((1, 18), (1, 19), "x"),
                    mk_write((1, 90), (1, 91), "x"),
                ],
            ),
            (
                mk_loc((1, 48), (1, 49)),
                vec![mk_write((1, 37), (1, 38), "x")],
            ),
            (
                mk_loc((1, 76), (1, 77)),
                vec![mk_write((1, 62), (1, 63), "x")],
            ),
            (
                mk_loc((1, 94), (1, 95)),
                vec![mk_write((1, 62), (1, 63), "x")],
            ),
            (
                mk_loc((1, 109), (1, 110)),
                vec![
                    mk_write((1, 18), (1, 19), "x"),
                    mk_write((1, 37), (1, 38), "x"),
                    mk_write((1, 90), (1, 91), "x"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_jsx() {
    mk_ssa_builder_test(
        false,
        "class Foo {}; <Foo></Foo>; <Foo/>",
        values_from_entries(vec![
            (
                mk_loc((1, 15), (1, 18)),
                vec![mk_write((1, 6), (1, 9), "Foo")],
            ),
            (
                mk_loc((1, 21), (1, 24)),
                vec![mk_write((1, 6), (1, 9), "Foo")],
            ),
            (
                mk_loc((1, 28), (1, 31)),
                vec![mk_write((1, 6), (1, 9), "Foo")],
            ),
        ]),
    );
}

#[test]
fn test_new() {
    mk_ssa_builder_test(
        false,
        "(function() { var x; new Y(x = 1); return x; })",
        values_from_entries(vec![(
            mk_loc((1, 42), (1, 43)),
            vec![mk_write((1, 27), (1, 28), "x")],
        )]),
    );
}

#[test]
fn test_new_closure() {
    mk_ssa_builder_test(
        false,
        "(function() { var x; new Y(function() { x = 1; }); return x; })",
        values_from_entries(vec![(
            mk_loc((1, 58), (1, 59)),
            vec![WriteLoc::Uninitialized, mk_write((1, 40), (1, 41), "x")],
        )]),
    );
}

#[test]
fn test_arrow() {
    mk_ssa_builder_test(
        false,
        "(x) => { let y = x; return y }",
        values_from_entries(vec![
            (
                mk_loc((1, 17), (1, 18)),
                vec![mk_write((1, 1), (1, 2), "x")],
            ),
            (
                mk_loc((1, 27), (1, 28)),
                vec![mk_write((1, 13), (1, 14), "y")],
            ),
        ]),
    );
}

#[test]
fn test_destructuring() {
    mk_ssa_builder_test(
        false,
        "let {a, b} = {a : 3, b : 4}; let c = a; let d = b;",
        values_from_entries(vec![
            (
                mk_loc((1, 37), (1, 38)),
                vec![mk_write((1, 5), (1, 6), "a")],
            ),
            (
                mk_loc((1, 48), (1, 49)),
                vec![mk_write((1, 8), (1, 9), "b")],
            ),
        ]),
    );
}

#[test]
fn test_rest_param() {
    mk_ssa_builder_test(
        false,
        "(...y) => { return y }",
        values_from_entries(vec![(
            mk_loc((1, 19), (1, 20)),
            vec![mk_write((1, 4), (1, 5), "y")],
        )]),
    );
}

#[test]
fn test_spread() {
    mk_ssa_builder_test(
        false,
        "let y = {}; let x = {...y}; ",
        values_from_entries(vec![(
            mk_loc((1, 24), (1, 25)),
            vec![mk_write((1, 4), (1, 5), "y")],
        )]),
    );
}

#[test]
fn test_switch_const() {
    mk_ssa_builder_test(
        false,
        "switch ('') { case '': const foo = ''; foo; };",
        values_from_entries(vec![(
            mk_loc((1, 39), (1, 42)),
            vec![mk_write((1, 29), (1, 32), "foo")],
        )]),
    );
}

#[test]
fn test_switch_weird() {
    mk_ssa_builder_test(
        false,
        "switch ('') { case l: 0; break; case '': let l };",
        values_from_entries(vec![(
            mk_loc((1, 19), (1, 20)),
            vec![WriteLoc::Uninitialized],
        )]),
    );
}

#[test]
fn test_param_defaults() {
    mk_ssa_builder_test(
        false,
        "function foo({x, y} = {x : 3, y : 3}) { return x + y; }",
        values_from_entries(vec![
            (
                mk_loc((1, 47), (1, 48)),
                vec![mk_write((1, 14), (1, 15), "x")],
            ),
            (
                mk_loc((1, 51), (1, 52)),
                vec![mk_write((1, 17), (1, 18), "y")],
            ),
        ]),
    );
}

#[test]
fn test_param_defaults_shadowed() {
    mk_ssa_builder_test(
        false,
        "let a = 0; function foo(x = a, y = x) { let a = 1; }",
        values_from_entries(vec![
            (
                mk_loc((1, 28), (1, 29)),
                vec![WriteLoc::Uninitialized, mk_write((1, 4), (1, 5), "a")],
            ),
            (
                mk_loc((1, 35), (1, 36)),
                vec![mk_write((1, 24), (1, 25), "x")],
            ),
        ]),
    );
}

#[test]
fn test_enums() {
    mk_ssa_builder_test(
        true,
        "enum E {A, B}; let a = E.A;",
        values_from_entries(vec![(
            mk_loc((1, 23), (1, 24)),
            vec![mk_write((1, 5), (1, 6), "E")],
        )]),
    );
}

#[test]
fn test_enums_off() {
    mk_ssa_builder_test(
        false,
        "enum E {A, B}; let a = E.A;",
        values_from_entries(vec![]),
    );
}

#[test]
fn test_instance_of() {
    mk_ssa_builder_test(
        false,
        "class A {}; let x = undefined; (x instanceof A)",
        values_from_entries(vec![
            (
                mk_loc((1, 32), (1, 33)),
                vec![mk_write((1, 16), (1, 17), "x")],
            ),
            (
                mk_loc((1, 45), (1, 46)),
                vec![mk_write((1, 6), (1, 7), "A")],
            ),
        ]),
    );
}

#[test]
fn test_class() {
    mk_ssa_builder_test(
        false,
        "class b { m() { b; } }",
        values_from_entries(vec![(
            mk_loc((1, 16), (1, 17)),
            vec![WriteLoc::Uninitialized, mk_write((1, 6), (1, 7), "b")],
        )]),
    );
}

#[test]
fn test_class_expr() {
    mk_ssa_builder_test(
        false,
        "let a = class b { m() { b; } }",
        values_from_entries(vec![(
            mk_loc((1, 24), (1, 25)),
            vec![WriteLoc::Uninitialized, mk_write((1, 14), (1, 15), "b")],
        )]),
    );
}

#[test]
fn test_fun_rec() {
    mk_ssa_builder_test(
        false,
        "function b() { b; }",
        values_from_entries(vec![(
            mk_loc((1, 15), (1, 16)),
            vec![WriteLoc::Uninitialized, mk_write((1, 9), (1, 10), "b")],
        )]),
    );
}

#[test]
fn test_fun_expr() {
    mk_ssa_builder_test(
        false,
        "let a = function b() { b; }",
        values_from_entries(vec![(
            mk_loc((1, 23), (1, 24)),
            vec![WriteLoc::Uninitialized, mk_write((1, 17), (1, 18), "b")],
        )]),
    );
}

#[test]
fn test_for_issue() {
    mk_ssa_builder_test(
        false,
        "for (var x in []) { x; x = 42; } x;",
        values_from_entries(vec![
            (
                mk_loc((1, 20), (1, 21)),
                vec![mk_write((1, 9), (1, 10), "x")],
            ),
            (
                mk_loc((1, 33), (1, 34)),
                vec![WriteLoc::Uninitialized, mk_write((1, 23), (1, 24), "x")],
            ),
        ]),
    );
}

#[test]
fn test_for_no_issue() {
    mk_ssa_builder_test(
        false,
        "for (let x = 0;;) { x; x = 42; }",
        values_from_entries(vec![(
            mk_loc((1, 20), (1, 21)),
            vec![
                mk_write((1, 9), (1, 10), "x"),
                mk_write((1, 23), (1, 24), "x"),
            ],
        )]),
    );
}

#[test]
fn test_for_in_post() {
    mk_ssa_builder_test(
        false,
        "var x = 42;
              for (var x in y) {
                x;
                if (condition) {
                  x = 42;
                  x;
                }
                x;
              }
              x;",
        values_from_entries(vec![
            (
                mk_loc((3, 16), (3, 17)),
                vec![mk_write((2, 23), (2, 24), "x")],
            ),
            (
                mk_loc((6, 18), (6, 19)),
                vec![mk_write((5, 18), (5, 19), "x")],
            ),
            (
                mk_loc((8, 16), (8, 17)),
                vec![
                    mk_write((2, 23), (2, 24), "x"),
                    mk_write((5, 18), (5, 19), "x"),
                ],
            ),
            (
                mk_loc((10, 14), (10, 15)),
                vec![
                    mk_write((1, 4), (1, 5), "x"),
                    mk_write((2, 23), (2, 24), "x"),
                    mk_write((5, 18), (5, 19), "x"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_declare_module() {
    mk_ssa_builder_test(
        false,
        "var x = 3;
         declare module 'foo' {
           declare var x: number;
           declare var y: typeof x;
         }
         x;",
        values_from_entries(vec![
            (
                mk_loc((4, 33), (4, 34)),
                vec![mk_write((3, 23), (3, 24), "x")],
            ),
            (mk_loc((6, 9), (6, 10)), vec![mk_write((1, 4), (1, 5), "x")]),
        ]),
    );
}

#[test]
fn test_try_catch_initialization() {
    mk_ssa_builder_test(
        false,
        "function test(a, b) {
  var c;
  try {
    c = b();
  } catch {};
  return c;
}",
        values_from_entries(vec![
            (
                mk_loc((4, 8), (4, 9)),
                vec![mk_write((1, 17), (1, 18), "b")],
            ),
            (
                mk_loc((6, 9), (6, 10)),
                vec![WriteLoc::Uninitialized, mk_write((4, 4), (4, 5), "c")],
            ),
        ]),
    );
}

#[test]
fn test_try_throw_catch() {
    mk_ssa_builder_test(
        false,
        "var x = 42;
try {
  x = 10;
  throw new Error();
  x = 100;
} catch (e) {
  x;
}",
        values_from_entries(vec![(
            mk_loc((7, 2), (7, 3)),
            vec![mk_write((1, 4), (1, 5), "x"), mk_write((3, 2), (3, 3), "x")],
        )]),
    );
}

#[test]
fn test_try_finally_initialization() {
    mk_ssa_builder_test(
        false,
        "function test(a, b) {
  var c;
  try {
    c = b();
  } finally {};
  return c;
}",
        values_from_entries(vec![
            (
                mk_loc((4, 8), (4, 9)),
                vec![mk_write((1, 17), (1, 18), "b")],
            ),
            (
                mk_loc((6, 9), (6, 10)),
                vec![WriteLoc::Uninitialized, mk_write((4, 4), (4, 5), "c")],
            ),
        ]),
    );
}

#[test]
fn test_try_catch_finally_initialization() {
    mk_ssa_builder_test(
        false,
        "function test(a, b) {
  var c;
  try {
    c = b();
  } catch {
    c = b()
  } finally {};
  return c;
}",
        values_from_entries(vec![
            (
                mk_loc((4, 8), (4, 9)),
                vec![mk_write((1, 17), (1, 18), "b")],
            ),
            (
                mk_loc((6, 8), (6, 9)),
                vec![mk_write((1, 17), (1, 18), "b")],
            ),
            (
                mk_loc((8, 9), (8, 10)),
                vec![
                    WriteLoc::Uninitialized,
                    mk_write((4, 4), (4, 5), "c"),
                    mk_write((6, 4), (6, 5), "c"),
                ],
            ),
        ]),
    );
}

#[test]
fn test_component_abrupt_completion() {
    mk_ssa_builder_test(
        false,
        "component Foo() { return }
Foo;
",
        values_from_entries(vec![(
            mk_loc((2, 0), (2, 3)),
            vec![mk_write((1, 10), (1, 13), "Foo")],
        )]),
    );
}

#[test]
fn test_function_abrupt_completion() {
    mk_ssa_builder_test(
        false,
        "function Foo() { return }
Foo;
",
        values_from_entries(vec![(
            mk_loc((2, 0), (2, 3)),
            vec![mk_write((1, 9), (1, 12), "Foo")],
        )]),
    );
}
