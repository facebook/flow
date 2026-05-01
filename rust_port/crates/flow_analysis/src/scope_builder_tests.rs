/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashSet;

use dupe::Dupe;
use flow_parser::ParseOptions;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::loc_sig::LocSig;
use flow_parser::parse_program_without_file;
use pretty_assertions::assert_eq;

use crate::bindings::Kind;
use crate::scope_api::Def;
use crate::scope_api::ScopeId;
use crate::scope_builder::program;

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

fn loc_debug_string(loc: &Loc) -> String {
    loc.debug_to_string(false)
}

fn loc_list_to_string(locs: &[Loc]) -> String {
    locs.iter()
        .map(loc_debug_string)
        .collect::<Vec<_>>()
        .join(", ")
}

fn mk_scope_builder_all_uses_test(enable_enums: bool, contents: &str, expected_all_uses: Vec<Loc>) {
    let info = program(enable_enums, true, &parse(contents));
    let mut all_uses: Vec<Loc> = info.all_uses().into_iter().cloned().collect();
    all_uses.sort_by(|a, b| a.compare_ignore_source(b));

    assert_eq!(
        loc_list_to_string(&expected_all_uses),
        loc_list_to_string(&all_uses),
        "All uses don't match!"
    );
}

fn mk_scope_builder_locs_of_defs_of_all_uses_test(
    enable_enums: bool,
    contents: &str,
    expected_locs_of_defs: Vec<Vec<Loc>>,
) {
    let info = program(enable_enums, true, &parse(contents));
    let mut all_uses: Vec<&Loc> = info.all_uses().into_iter().collect();
    all_uses.sort_by(|a, b| a.compare_ignore_source(b));

    let defs: Vec<&Def<Loc>> = all_uses
        .iter()
        .map(|use_loc| info.def_of_use_opt(use_loc).unwrap())
        .collect();

    let locs_of_defs: Vec<Vec<Loc>> = defs
        .iter()
        .map(|def| def.locs.iter().cloned().collect())
        .collect();

    let printer = |list: &Vec<Vec<Loc>>| {
        list.iter()
            .map(|inner| format!("[{}]", loc_list_to_string(inner)))
            .collect::<Vec<_>>()
            .join(", ")
    };

    assert_eq!(
        printer(&expected_locs_of_defs),
        printer(&locs_of_defs),
        "Defs of all uses don't match!"
    );
}

fn mk_scope_builder_uses_of_all_uses_test(
    enable_enums: bool,
    contents: &str,
    expected_uses: Vec<Vec<Loc>>,
) {
    let info = program(enable_enums, true, &parse(contents));
    let mut all_uses: Vec<&Loc> = info.all_uses().into_iter().collect();
    all_uses.sort_by(|a, b| a.compare_ignore_source(b));

    let uses: Vec<Vec<Loc>> = all_uses
        .iter()
        .map(|use_loc| {
            let mut use_locs: Vec<Loc> = info.uses_of_use(use_loc, true).iter().cloned().collect();
            use_locs.sort_by(|a, b| a.compare_ignore_source(b));
            use_locs
        })
        .collect();

    let printer = |list: &Vec<Vec<Loc>>| {
        list.iter()
            .map(|inner| format!("[{}]", loc_list_to_string(inner)))
            .collect::<Vec<_>>()
            .join(", ")
    };

    assert_eq!(
        printer(&expected_uses),
        printer(&uses),
        "Uses of all uses don't match!"
    );
}

// Asserts that scopes classified as toplevel indeed contains all toplevel defs
fn mk_scope_builder_toplevel_scopes_test(
    enable_enums: bool,
    contents: &str,
    expected_defs_in_toplevel: Vec<&str>,
) {
    let info = program(enable_enums, false, &parse(contents));

    let toplevel_scopes = vec![ScopeId(0)];
    let mut actual_defs_in_toplevel = HashSet::new();

    for scope_id in toplevel_scopes {
        let scope = info.scope(scope_id);
        for def_name in scope.defs.keys() {
            actual_defs_in_toplevel.insert(def_name.as_str());
        }
    }

    let mut actual_defs: Vec<&str> = actual_defs_in_toplevel.into_iter().collect();
    actual_defs.sort();

    let mut expected: Vec<&str> = expected_defs_in_toplevel;
    expected.sort();

    assert_eq!(
        expected.join(", "),
        actual_defs.join(", "),
        "Toplevel defs don't match!"
    );
}

fn mk_autocomplete_identifiers_test(
    enable_enums: bool,
    contents: &str,
    ac_loc: Loc,
    expected_names: Vec<&str>,
) {
    let scope_info = program(enable_enums, false, &parse(contents));

    // get the innermost scope enclosing the requested location
    let ac_scope_id =
        scope_info.closest_enclosing_scope(&ac_loc, |a, b| Loc::span_compare(b, a) == 0);

    // gather all in-scope variables
    let mut actual_names = HashSet::new();
    scope_info.fold_scope_chain(ac_scope_id, (), |_scope_id, scope, _| {
        for (name, def) in &scope.defs {
            let allows_forward_ref = matches!(
                def.kind,
                Kind::Function
                    | Kind::DeclaredFunction
                    | Kind::Class
                    | Kind::DeclaredClass
                    | Kind::Type { .. }
                    | Kind::Enum
            );
            if allows_forward_ref || def.locs.first().cmp(&ac_loc) == std::cmp::Ordering::Less {
                actual_names.insert(name.clone());
            }
        }
    });

    let mut actual: Vec<String> = actual_names
        .into_iter()
        .map(|s| s.as_str().to_owned())
        .collect();
    actual.sort();

    let mut expected: Vec<&str> = expected_names;
    expected.sort();

    assert_eq!(expected.join(", "), actual.join(", "));
}

fn mk_scope_builder_scope_loc_test(
    enable_enums: bool,
    contents: &str,
    expected_scope_locs: Vec<(u32, Loc)>,
) {
    let info = program(enable_enums, true, &parse(contents));

    let mut scope_locs: Vec<(u32, Loc)> = info
        .scopes
        .iter()
        .map(|(id, scope)| (id.0, scope.loc.dupe()))
        .collect();
    scope_locs.sort_by_key(|(id, _)| *id);

    let printer = |list: &Vec<(u32, Loc)>| {
        list.iter()
            .map(|(id, loc)| format!("{}: {}", id, loc_debug_string(loc)))
            .collect::<Vec<_>>()
            .join(", ")
    };

    assert_eq!(
        printer(&expected_scope_locs),
        printer(&scope_locs),
        "Scope locs don't match!"
    );
}

#[test]
fn let_all_uses() {
    mk_scope_builder_all_uses_test(
        false,
        "function foo(x, ...y) {\n  let z = 0;\n  x, y;\n  return z;\n}",
        vec![
            mk_loc((1, 9), (1, 12)),  // foo
            mk_loc((1, 13), (1, 14)), // x def
            mk_loc((1, 19), (1, 20)), // y def
            mk_loc((2, 6), (2, 7)),   // z def
            mk_loc((3, 2), (3, 3)),   // x use
            mk_loc((3, 5), (3, 6)),   // y use
            mk_loc((4, 9), (4, 10)),  // z use
        ],
    );
}

#[test]
fn let_locs_of_defs_of_all_uses() {
    mk_scope_builder_locs_of_defs_of_all_uses_test(
        false,
        "function foo() { let x = 0; return x; }",
        vec![
            vec![mk_loc((1, 9), (1, 12))],
            vec![mk_loc((1, 21), (1, 22))],
            vec![mk_loc((1, 21), (1, 22))],
        ],
    );
}

#[test]
fn let_uses_of_all_uses() {
    mk_scope_builder_uses_of_all_uses_test(
        false,
        "function foo() { let x = 0; return x; }",
        vec![
            vec![],
            vec![mk_loc((1, 35), (1, 36))],
            vec![mk_loc((1, 35), (1, 36))],
        ],
    );
}

#[test]
fn var_locs_of_defs_of_all_uses() {
    mk_scope_builder_locs_of_defs_of_all_uses_test(
        false,
        "function foo({y}) { var {x} = y; return x; }",
        vec![
            vec![mk_loc((1, 9), (1, 12))],
            vec![mk_loc((1, 14), (1, 15))],
            vec![mk_loc((1, 25), (1, 26))],
            vec![mk_loc((1, 14), (1, 15))],
            vec![mk_loc((1, 25), (1, 26))],
        ],
    );
}

#[test]
fn var_uses_of_all_uses() {
    mk_scope_builder_uses_of_all_uses_test(
        false,
        "function foo({y}) { var {x} = y; return x; }",
        vec![
            vec![],
            vec![mk_loc((1, 30), (1, 31))],
            vec![mk_loc((1, 40), (1, 41))],
            vec![mk_loc((1, 30), (1, 31))],
            vec![mk_loc((1, 40), (1, 41))],
        ],
    );
}

#[test]
fn var_locs_of_defs_of_all_uses2() {
    mk_scope_builder_locs_of_defs_of_all_uses_test(
        false,
        "function foo() { var { x, y } = { x: 0, y: 0 }; var { x: _x, y: _y } = { x, y }; return ({ x: _x, y: _y }); }",
        vec![
            vec![mk_loc((1, 9), (1, 12))],
            vec![mk_loc((1, 23), (1, 24))],
            vec![mk_loc((1, 26), (1, 27))],
            vec![mk_loc((1, 57), (1, 59))],
            vec![mk_loc((1, 64), (1, 66))],
            vec![mk_loc((1, 23), (1, 24))],
            vec![mk_loc((1, 26), (1, 27))],
            vec![mk_loc((1, 57), (1, 59))],
            vec![mk_loc((1, 64), (1, 66))],
        ],
    );
}

#[test]
fn let_uses_of_all_uses2() {
    mk_scope_builder_uses_of_all_uses_test(
        false,
        "function foo() { let { x, y } = { x: 0, y: 0 }; let { x: _x, y: _y } = { x, y }; return ({ x: _x, y: _y }); }",
        vec![
            vec![],
            vec![mk_loc((1, 73), (1, 74))],
            vec![mk_loc((1, 76), (1, 77))],
            vec![mk_loc((1, 94), (1, 96))],
            vec![mk_loc((1, 101), (1, 103))],
            vec![mk_loc((1, 73), (1, 74))],
            vec![mk_loc((1, 76), (1, 77))],
            vec![mk_loc((1, 94), (1, 96))],
            vec![mk_loc((1, 101), (1, 103))],
        ],
    );
}

#[test]
fn jsx_uses_of_all_uses() {
    mk_scope_builder_all_uses_test(
        false,
        "class Foo {}; <Foo></Foo>; <Foo/>",
        vec![
            mk_loc((1, 6), (1, 9)),
            mk_loc((1, 15), (1, 18)),
            mk_loc((1, 21), (1, 24)),
            mk_loc((1, 28), (1, 31)),
        ],
    );
}

#[test]
fn declare_var() {
    mk_scope_builder_all_uses_test(
        false,
        "declare var foo: number; foo",
        vec![mk_loc((1, 12), (1, 15)), mk_loc((1, 25), (1, 28))],
    );
}

#[test]
fn declare_let() {
    mk_scope_builder_all_uses_test(
        false,
        "declare let foo: number; foo",
        vec![mk_loc((1, 12), (1, 15)), mk_loc((1, 25), (1, 28))],
    );
}

#[test]
fn declare_const() {
    mk_scope_builder_all_uses_test(
        false,
        "declare const foo: number; foo",
        vec![mk_loc((1, 14), (1, 17)), mk_loc((1, 27), (1, 30))],
    );
}

#[test]
fn declare_var_let_const_block_scope() {
    mk_scope_builder_toplevel_scopes_test(
        false,
        "{ declare var x: 1; declare let y: 2; declare const z: 3 }",
        vec!["x"],
    );
}

#[test]
fn declare_export_var() {
    mk_scope_builder_all_uses_test(
        false,
        "declare export var bar: number; bar",
        vec![mk_loc((1, 19), (1, 22)), mk_loc((1, 32), (1, 35))],
    );
}

#[test]
fn declare_class() {
    mk_scope_builder_all_uses_test(
        false,
        "declare class Foo {}; new Foo()",
        vec![mk_loc((1, 14), (1, 17)), mk_loc((1, 26), (1, 29))],
    );
}

#[test]
fn declare_function() {
    mk_scope_builder_all_uses_test(
        false,
        "declare function foo(): void; foo()",
        vec![mk_loc((1, 17), (1, 20)), mk_loc((1, 30), (1, 33))],
    );
}

#[test]
fn import_named() {
    mk_scope_builder_all_uses_test(
        false,
        "import {A} from 'A'; A()",
        vec![mk_loc((1, 8), (1, 9)), mk_loc((1, 21), (1, 22))],
    );
}

#[test]
fn import_named_as() {
    mk_scope_builder_all_uses_test(
        false,
        "const B = 1; import {B as A} from 'A'; A()",
        vec![
            mk_loc((1, 6), (1, 7)),
            mk_loc((1, 26), (1, 27)),
            mk_loc((1, 39), (1, 40)),
        ],
    );
}

#[test]
fn export_named_function() {
    mk_scope_builder_all_uses_test(
        false,
        "export function foo() {}; foo()",
        vec![mk_loc((1, 16), (1, 19)), mk_loc((1, 26), (1, 29))],
    );
}

#[test]
fn export_named_class() {
    mk_scope_builder_all_uses_test(
        false,
        "export class Foo {}; new Foo()",
        vec![mk_loc((1, 13), (1, 16)), mk_loc((1, 25), (1, 28))],
    );
}

#[test]
fn export_named_binding() {
    mk_scope_builder_all_uses_test(
        false,
        "export const foo = () => {}; foo()",
        vec![mk_loc((1, 13), (1, 16)), mk_loc((1, 29), (1, 32))],
    );
}

#[test]
fn export_default_function() {
    mk_scope_builder_all_uses_test(
        false,
        "export default function foo() {}; foo()",
        vec![mk_loc((1, 24), (1, 27)), mk_loc((1, 34), (1, 37))],
    );
}

#[test]
fn export_default_class() {
    mk_scope_builder_all_uses_test(
        false,
        "export default class Foo {} new Foo()",
        vec![mk_loc((1, 21), (1, 24)), mk_loc((1, 32), (1, 35))],
    );
}

#[test]
fn export_specifier() {
    mk_scope_builder_all_uses_test(
        false,
        "const A = 1; export {A};",
        vec![mk_loc((1, 6), (1, 7)), mk_loc((1, 21), (1, 22))],
    );
}

#[test]
fn export_specifier_as() {
    mk_scope_builder_all_uses_test(
        false,
        "const A = 1; const B = 1; export {A as B};",
        vec![
            mk_loc((1, 6), (1, 7)),
            mk_loc((1, 19), (1, 20)),
            mk_loc((1, 34), (1, 35)),
        ],
    );
}

#[test]
fn computed_property_destructuring() {
    mk_scope_builder_all_uses_test(
        false,
        "const x = {}; const foo = ''; const {[foo]: bar} = x;",
        vec![
            mk_loc((1, 6), (1, 7)),
            mk_loc((1, 20), (1, 23)),
            mk_loc((1, 38), (1, 41)),
            mk_loc((1, 44), (1, 47)),
            mk_loc((1, 51), (1, 52)),
        ],
    );
}

#[test]
fn enums() {
    mk_scope_builder_all_uses_test(
        true,
        "enum Foo {}\nFoo",
        vec![mk_loc((1, 5), (1, 8)), mk_loc((2, 0), (2, 3))],
    );
}

#[test]
fn enums_off() {
    mk_scope_builder_all_uses_test(false, "enum Foo {}\nFoo", vec![]);
}

#[test]
fn switch() {
    mk_scope_builder_all_uses_test(
        false,
        "switch ('') { case '': const foo = ''; foo; };",
        vec![mk_loc((1, 29), (1, 32)), mk_loc((1, 39), (1, 42))],
    );
}

#[test]
fn switch_weird() {
    mk_scope_builder_all_uses_test(
        false,
        "switch ('') { case l: 0; break; case '': let l };",
        vec![mk_loc((1, 19), (1, 20)), mk_loc((1, 45), (1, 46))],
    );
}

#[test]
fn scope_loc_function_declaration() {
    mk_scope_builder_scope_loc_test(
        false,
        "function a() {};",
        vec![
            (0, mk_loc((1, 0), (1, 16))),  // program
            (1, mk_loc((1, 13), (1, 15))), // function params and body
        ],
    );
}

#[test]
fn scope_loc_function_expression() {
    mk_scope_builder_scope_loc_test(
        false,
        "const x = function() {};",
        vec![
            (0, mk_loc((1, 0), (1, 24))),  // program
            (1, mk_loc((1, 10), (1, 23))), // function name
            (2, mk_loc((1, 21), (1, 23))), // function params and body
        ],
    );
}

#[test]
fn scope_loc_arrow_function() {
    mk_scope_builder_scope_loc_test(
        false,
        "const x = () => 1;",
        vec![
            (0, mk_loc((1, 0), (1, 18))),  // program
            (1, mk_loc((1, 10), (1, 17))), // function name (lexical)
            (2, mk_loc((1, 16), (1, 17))), // function params and body
        ],
    );
}

#[test]
fn scope_loc_arrow_function_with_default() {
    mk_scope_builder_scope_loc_test(
        false,
        "const x = (foo = 3) => 1;",
        vec![
            (0, mk_loc((1, 0), (1, 25))),  // program
            (1, mk_loc((1, 10), (1, 24))), // function name (lexical)
            (2, mk_loc((1, 23), (1, 24))), // function parameters
            (3, mk_loc((1, 23), (1, 24))), // function body
        ],
    );
}

#[test]
fn scope_loc_for_in() {
    mk_scope_builder_scope_loc_test(
        false,
        "for (let a in b) {}; 42",
        vec![
            (0, mk_loc((1, 0), (1, 23))),  // program
            (1, mk_loc((1, 0), (1, 19))),  // for in (lexical)
            (2, mk_loc((1, 17), (1, 19))), // block (lexical)
        ],
    );
}

#[test]
fn toplevel_defs_empty() {
    mk_scope_builder_toplevel_scopes_test(false, "", vec![]);
}

#[test]
fn toplevel_defs_hoisting_only() {
    mk_scope_builder_toplevel_scopes_test(false, "function test(a) {}", vec!["test"]);
}

#[test]
fn toplevel_defs_lexical_only() {
    mk_scope_builder_toplevel_scopes_test(false, "const a = b", vec!["a"]);
}

#[test]
fn toplevel_defs_hoisting_and_lexical() {
    mk_scope_builder_toplevel_scopes_test(
        false,
        "const a = b; function test(a) {}",
        vec!["a", "test"],
    );
}

#[test]
fn toplevel_defs_class() {
    mk_scope_builder_toplevel_scopes_test(false, "let a = class b { }", vec!["a"]);
}

#[test]
fn class_expr_loc() {
    mk_scope_builder_scope_loc_test(
        false,
        "let a = class b { m(c) { a; b; c; }}",
        vec![
            (0, mk_loc((1, 0), (1, 36))),
            (1, mk_loc((1, 8), (1, 36))),
            (2, mk_loc((1, 19), (1, 35))),
            (3, mk_loc((1, 23), (1, 35))),
        ],
    );
}

#[test]
fn declare_export_default() {
    mk_scope_builder_all_uses_test(
        false,
        "declare export default class Foo {}; new Foo()",
        vec![mk_loc((1, 29), (1, 32)), mk_loc((1, 41), (1, 44))],
    );
}

#[test]
fn hoisted_component_declaration_uses() {
    mk_scope_builder_all_uses_test(
        false,
        "Foo; component Foo() { }; Foo",
        vec![
            mk_loc((1, 0), (1, 3)),
            mk_loc((1, 15), (1, 18)),
            mk_loc((1, 26), (1, 29)),
        ],
    );
}

#[test]
fn component_param_scoping_1() {
    mk_scope_builder_all_uses_test(
        false,
        "component Foo(
              param,
              param2 = param
            ) {
              param;
              param2;
            }",
        vec![
            mk_loc((1, 10), (1, 13)),
            mk_loc((2, 14), (2, 19)),
            mk_loc((3, 14), (3, 20)),
            mk_loc((5, 14), (5, 19)),
            mk_loc((6, 14), (6, 20)),
        ],
    );
}

#[test]
fn component_param_scoping_out_of_scope() {
    mk_scope_builder_all_uses_test(
        false,
        "component Foo(
            p,
            q: typeof p,
        ) {
        }",
        vec![
            mk_loc((1, 10), (1, 13)),
            mk_loc((2, 12), (2, 13)),
            mk_loc((3, 12), (3, 13)),
        ],
    );
}

#[test]
fn scope_loc_component_declaration() {
    mk_scope_builder_scope_loc_test(
        false,
        "component Foo(param: T) {};",
        vec![(0, mk_loc((1, 0), (1, 27))), (1, mk_loc((1, 24), (1, 26)))],
    );
}

#[test]
fn identifier_test_fun() {
    mk_autocomplete_identifiers_test(
        false,
        "function F() {
              const xaaa = 42;
              const y = x
           }",
        mk_loc((3, 13), (3, 13)),
        vec!["F", "xaaa"],
    );
}

#[test]
fn identifier_test_component() {
    mk_autocomplete_identifiers_test(
        false,
        "component F() {
              const xaaa = 42;
              const y = x
           }",
        mk_loc((3, 13), (3, 13)),
        vec!["F", "xaaa"],
    );
}
