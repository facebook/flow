/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::types as at;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::loc_sig::LocSig;
use flow_parser_utils::ast_builder;
use flow_parser_utils::ast_builder::comments as C;
use flow_parser_utils::ast_builder::expressions as E;
use flow_parser_utils::ast_builder::identifiers as I;
use flow_parser_utils::ast_builder::statements as S;
use flow_parser_utils::ast_builder::types as T;

use crate::js_layout_generator;
use crate::layout::LayoutNode;
use crate::layout_generator_test_utils::*;
use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::*;

#[test]
fn type_union_or_intersection() {
    assert_statement_string(false, None, "type a=a|b;");
    assert_statement_string(false, None, "type a=a|b|c;");
    assert_statement_string(false, None, "type a=?(a|b);");
    assert_statement_string(false, None, "type a=a&b;");
    assert_statement_string(false, None, "type a=a&b&c;");
    assert_statement_string(false, None, "type a=?(a&b);");
    assert_statement_string(false, None, "type a=a|(b&c)|d;");
    assert_statement_string(false, None, "type a=(a|b)&c;");
    assert_statement_string(false, None, "type a=(a&b)|c;");
    assert_statement_string(false, None, "type a=a|(b|c);");
    assert_statement_string(false, None, "type a=(a&b)|c;");
    assert_statement_string(false, None, "type a=a|(()=>b)|c;");
    assert_statement_string(true, None, "type a = a | b;");
    assert_statement_string(true, None, "type a = a | b | c;");
    assert_statement_string(true, None, "type a = a & b & c;");
    assert_statement_string(
        true,
        None,
        &format!("type a = \n  | a\n  | b\n  | {};", "c".repeat(80)),
    );
}

#[test]
fn function_type_with_type_guard() {
    let params = T::functions::params(
        None,
        None,
        None,
        None,
        vec![T::functions::param(
            None,
            None,
            Some(I::identifier(None, "x")),
            T::mixed(None, None),
        )],
    );
    let return_ = T::return_type_guard_annotation(
        None,
        None,
        I::identifier(None, "x"),
        T::number(None, None),
    );
    let layout = js_layout_generator::type_(
        &opts(),
        &at::Type::new(at::TypeInner::Function {
            loc: Loc::none(),
            inner: Arc::new(T::functions::make(None, None, None, params, return_)),
        }),
    );
    assert_output(false, "(x:mixed)=>x is number", &layout);
    assert_output(true, "(x: mixed) => x is number", &layout);
}

#[test]
fn interface_declaration_statements() {
    assert_statement_string(false, None, "interface a{}");
    assert_statement_string(false, None, "interface a extends b{}");
    assert_statement_string(false, None, "interface a<a> extends b{}");
    assert_statement_string(false, None, "interface a extends b,c{}");
    assert_statement_string(true, None, "interface a {}");
    assert_statement_string(true, None, "interface a extends b, c {}");
    assert_statement_string(
        true,
        None,
        &format!("interface a {{\n  a: b,\n  d(): {},\n}}", "c".repeat(80)),
    );
}

#[test]
fn declare_class_statements() {
    assert_statement_string(false, None, "declare class a{}");
    assert_statement_string(false, None, "declare class a extends b{}");
    assert_statement_string(false, None, "declare class a implements b{}");
    assert_statement_string(
        false,
        None,
        "declare class a extends b mixins c implements d{}",
    );
    assert_statement_string(false, None, "declare class a extends b implements c{}");
    assert_statement_string(
        true,
        None,
        &format!(
            "declare class a {{\n  static a: b,\n  static d(): {},\n}}",
            "c".repeat(80)
        ),
    );
}

#[test]
fn declare_enum_statements() {
    assert_statement_string(false, None, "declare enum E{A,}");
    assert_statement_string(true, None, "declare enum E {\n  A,\n}");
}

#[test]
fn declare_function_statements() {
    assert_statement_string(false, None, "declare function a():b;");
    assert_statement_string(true, None, "declare function a(): b;");
    assert_statement_string(false, None, "declare function f():a%checks;");
    assert_statement_string(false, None, "declare function f(a:b):a%checks(!a);");
    assert_statement_string(true, None, "declare function f(a: b): a %checks(!a);");
}

#[test]
fn declare_var_statements() {
    assert_statement_string(false, None, "declare var a:b;");
    assert_statement_string(false, None, "declare let a:b;");
    assert_statement_string(false, None, "declare const a:b;");
    // multiple declarations
    assert_statement_string(false, None, "declare const a:b,c:d;");
    assert_statement_string(false, None, "declare let a:b,c:d;");
    assert_statement_string(false, None, "declare var a:b,c:d;");
    // literal initializer
    assert_statement_string(false, None, r#"declare const s="foo";"#);
    assert_statement_string(false, None, "declare const n=10;");
    assert_statement_string(false, None, "declare const b=true;");
}

#[test]
fn declare_module_exports_statements() {
    assert_statement_string(false, None, "declare module.exports:a;");
}

#[test]
fn declare_module_statements() {
    assert_statement_string(false, None, "declare module a{}");
    assert_statement_string(false, None, "declare module \"a\"{}");
    assert_statement_string(true, None, "declare module \"a\" {}");
}

#[test]
fn declare_export_declaration_statements() {
    assert_statement_string(false, None, "declare export default a;");
    assert_statement_string(false, None, "declare export var a:a;");
    assert_statement_string(false, None, "declare export function a():a;");
    assert_statement_string(false, None, "declare export default function a():a;");
    assert_statement_string(false, None, "declare export class a{}");
    assert_statement_string(false, None, "declare export default class a{}");
    assert_statement_string(false, None, "declare export abstract class a{}");
    assert_statement_string(false, None, "declare export default abstract class a{}");
    assert_statement_string(false, None, "declare export enum E{A,}");
    assert_statement_string(false, None, "declare export{}");
    assert_statement_string(false, None, "declare export{a,b}");
    assert_statement_string(false, None, "declare export{a,b}from\"a\"");
    assert_statement_string(false, None, "declare export*from\"a\"");
}

#[test]
fn regexp() {
    // flags should be sorted
    let regexp = ast_builder::test_expression_of_string("/foo/ymg");
    assert_expression(false, None, None, "/foo/gmy", &regexp);
}

#[test]
fn string_literal_quotes() {
    assert_expression(
        false,
        None,
        None,
        "\"'''\"",
        &ast_builder::test_expression_of_string("\"'''\""),
    );
    assert_expression(
        false,
        None,
        None,
        "'\"'",
        &ast_builder::test_expression_of_string("\"\\\"\""),
    );
    assert_expression(
        false,
        None,
        None,
        "\"''\"",
        &ast_builder::test_expression_of_string("'\\'\\'"),
    );
    assert_expression(
        false,
        None,
        None,
        "\"''\\\"\"",
        &ast_builder::test_expression_of_string("\"''\\\"\""),
    );
    assert_expression(
        false,
        None,
        None,
        "'\"\"\\''",
        &ast_builder::test_expression_of_string("'\"\"\\''"),
    );
}

#[test]
fn switch() {
    let case1_loc = Loc {
        start: Position { line: 1, column: 1 },
        end: Position { line: 2, column: 3 },
        ..LOC_NONE
    };
    let case2_loc = Loc {
        start: Position { line: 4, column: 1 },
        end: Position { line: 5, column: 3 },
        ..LOC_NONE
    };
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::switch(
            None,
            None,
            E::identifier(None, None, "x"),
            vec![
                S::switch_case(
                    Some(case1_loc.dupe()),
                    Some(E::literals::string(None, None, "a")),
                    None,
                    None,
                    vec![
                        S::expression(
                            None,
                            None,
                            None,
                            E::increment(false, E::identifier(None, None, "x")),
                        ),
                        S::break_(None, None, None),
                    ],
                ),
                S::switch_case(
                    Some(case2_loc.dupe()),
                    Some(E::literals::string(None, None, "b")),
                    None,
                    None,
                    vec![
                        S::expression(
                            None,
                            None,
                            None,
                            E::increment(false, E::identifier(None, None, "x")),
                        ),
                        S::break_(None, None, None),
                    ],
                ),
            ],
        ),
    );
    assert_layout(
        L::loc(
            None,
            L::fused(vec![
                L::group(vec![
                    L::atom("switch"),
                    L::pretty_space(),
                    L::atom("("),
                    L::indent(L::fused(vec![
                        L::softline(),
                        L::loc(None, L::id(None, "x")),
                    ])),
                    L::softline(),
                    L::atom(")"),
                ]),
                L::pretty_space(),
                L::atom("{"),
                L::indent(L::fused(vec![
                    L::pretty_hardline(),
                    L::loc(
                        Some(case1_loc),
                        L::fused(vec![
                            L::atom("case"),
                            L::pretty_space(),
                            L::loc(
                                None,
                                L::fused(vec![L::atom("\""), L::atom("a"), L::atom("\"")]),
                            ),
                            L::atom(":"),
                            L::indent(L::fused(vec![
                                L::pretty_hardline(),
                                L::loc(
                                    None,
                                    L::fused(vec![
                                        L::loc(
                                            None,
                                            L::fused(vec![
                                                L::loc(None, L::id(None, "x")),
                                                L::atom("++"),
                                            ]),
                                        ),
                                        L::atom(";"),
                                    ]),
                                ),
                                L::pretty_hardline(),
                                L::loc(None, L::fused(vec![L::atom("break"), L::atom(";")])),
                            ])),
                        ]),
                    ),
                    L::pretty_hardline(),
                    L::pretty_hardline(),
                    L::loc(
                        Some(case2_loc),
                        L::fused(vec![
                            L::atom("case"),
                            L::pretty_space(),
                            L::loc(
                                None,
                                L::fused(vec![L::atom("\""), L::atom("b"), L::atom("\"")]),
                            ),
                            L::atom(":"),
                            L::indent(L::fused(vec![
                                L::pretty_hardline(),
                                L::loc(
                                    None,
                                    L::fused(vec![
                                        L::loc(
                                            None,
                                            L::fused(vec![
                                                L::loc(None, L::id(None, "x")),
                                                L::atom("++"),
                                            ]),
                                        ),
                                        L::atom(";"),
                                    ]),
                                ),
                                L::pretty_hardline(),
                                L::loc(
                                    None,
                                    L::fused(vec![
                                        L::atom("break"),
                                        LayoutNode::if_pretty(L::atom(";"), L::empty()),
                                    ]),
                                ),
                            ])),
                        ]),
                    ),
                ])),
                L::pretty_hardline(),
                L::atom("}"),
            ]),
        ),
        layout.clone(),
    );
    assert_output(
        false,
        "switch(x){case\"a\":x++;break;case\"b\":x++;break}",
        &layout,
    );
    assert_output(
        true,
        &(String::from("switch (x) {\n")
            + "  case \"a\":\n"
            + "    x++;\n"
            + "    break;\n"
            + "  \n"
            // TODO: fix trailing whitespace
            + "  case \"b\":\n"
            + "    x++;\n"
            + "    break;\n"
            + "}"),
        &layout,
    );
}

#[test]
fn switch_conditional_wrap() {
    // the conditional should wrap, because `switch (xxxxx...xxx) {` does't fit on one line
    let len = 80 - "switch () {".len() + 1;
    assert_statement_string(
        true,
        None,
        // TODO: fix trailing whitespace
        &format!("switch (\n  {}\n) {{\n  \n}}", "x".repeat(len)),
    );
}

#[test]
fn switch_case_space() {
    let assert_no_space = |expr: &str| {
        let ret =
            ast_builder::test_statement_of_string(&format!("switch(x){{case {}:break}}", expr));
        assert_statement(
            false,
            None,
            &format!("switch(x){{case{}:break}}", expr),
            &ret,
        );
    };
    assert_no_space("\"foo\"");
    assert_no_space("{foo:\"bar\"}");
    assert_no_space("[foo]");
    assert_no_space("!foo");
    assert_no_space("+foo");
    assert_no_space("-foo");
    assert_no_space("~foo");

    let ret = ast_builder::test_statement_of_string("switch(x){case (foo):break}");
    assert_statement(false, None, "switch(x){case foo:break}", &ret);

    let ret = ast_builder::test_statement_of_string("switch(x){case 123:break}");
    assert_statement(false, None, "switch(x){case 123:break}", &ret);
}

#[test]
fn switch_case_empty() {
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::switch(
            None,
            None,
            E::identifier(None, None, "x"),
            vec![S::switch_case(
                None,
                Some(E::literals::string(None, None, "a")),
                None,
                None,
                vec![S::empty(None)],
            )],
        ),
    );
    assert_output(false, "switch(x){case\"a\":;}", &layout);
    assert_output(
        true,
        &(String::from("switch (x) {\n") + "  case \"a\":\n" + "    ;\n" + "}"),
        &layout,
    );
}

#[test]
fn throw_space() {
    let assert_no_space = |expr: &str| {
        let ret = ast_builder::test_statement_of_string(&format!("throw {};", expr));
        assert_statement(false, None, &format!("throw{};", expr), &ret);
    };
    assert_no_space("\"foo\"");
    assert_no_space("{foo:\"bar\"}");
    assert_no_space("[foo]");
    assert_no_space("!foo");
    assert_no_space("+foo");
    assert_no_space("-foo");
    assert_no_space("~foo");

    assert_statement_string(false, None, "throw foo;");
    assert_statement(
        false,
        None,
        "throw foo;",
        &ast_builder::test_statement_of_string("throw (foo);"),
    );
    assert_statement_string(false, None, "throw new Error();");
}

#[test]
fn string_literal() {
    let ast = E::literals::string(None, None, "a");
    let layout = js_layout_generator::expression(&opts(), None, &ast);
    assert_layout(
        L::loc(
            None,
            L::fused(vec![L::atom("\""), L::atom("a"), L::atom("\"")]),
        ),
        layout.clone(),
    );
    assert_output(false, "\"a\"", &layout);
    assert_output(true, "\"a\"", &layout);
}

#[test]
fn unicode_string_literal() {
    // escaped using Unicode codepoint
    let ast = ast_builder::test_expression_of_string("\"\\u{1F4A9}\"");
    assert_expression(false, None, None, "\"\\ud83d\\udca9\"", &ast);

    // not escaped when formatting is preserved
    assert_expression(
        false,
        None,
        Some(&preserve_formatting_opts()),
        "\"\\u{1F4A9}\"",
        &ast,
    );

    // escaped using UTF-16 (hex get lowercased)
    let ast = ast_builder::test_expression_of_string("\"\\uD83D\\uDCA9\"");
    assert_expression(false, None, None, "\"\\ud83d\\udca9\"", &ast);

    // literal emoji
    let ast = ast_builder::test_expression_of_string("\"\u{1F4A9}\"");
    assert_expression(false, None, None, "\"\\ud83d\\udca9\"", &ast);

    // zero followed by ASCII number
    let ast = ast_builder::test_expression_of_string("\"\x001\"");
    assert_expression(false, None, None, "\"\\x001\"", &ast);

    // not `\01`!
    let ast = ast_builder::test_expression_of_string("\"\x009\"");
    assert_expression(false, None, None, "\"\\x009\"", &ast);

    // not `\09`!

    // unprintable ascii, escaped
    let ast = ast_builder::test_expression_of_string("\"\\x07\"");
    assert_expression(false, None, None, "\"\\x07\"", &ast);
    let ast = ast_builder::test_expression_of_string("\"\\x11\"");
    assert_expression(false, None, None, "\"\\x11\"", &ast);

    // unprintable ascii, literal
    let ast = ast_builder::test_expression_of_string("\"\x11\"");
    assert_expression(false, None, None, "\"\\x11\"", &ast);

    // special escapes
    let ast = ast_builder::test_expression_of_string("\"\\x09\"");
    assert_expression(false, None, None, "\"\\t\"", &ast);
    let ast = ast_builder::test_expression_of_string("\"\\\\\"");
    assert_expression(false, None, None, "\"\\\\\"", &ast);
}

#[test]
fn numbers() {
    assert_expression(
        false,
        None,
        None,
        "100",
        &ast_builder::test_expression_of_string("1e2"),
    );
    assert_expression(
        false,
        None,
        None,
        "1e3",
        &ast_builder::test_expression_of_string("1000"),
    );
    assert_expression(
        false,
        None,
        None,
        "2592e6",
        &ast_builder::test_expression_of_string("2.592e+09"),
    );
}

#[test]
fn sequence_long() {
    let x80 = "x".repeat(80);
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::sequence(
            None,
            None,
            vec![
                E::identifier(None, None, &x80),
                E::identifier(None, None, &x80),
            ],
        ),
    );
    assert_output(false, &format!("{},{}", x80, x80), &layout);
    assert_output(true, &format!("{},\n{}", x80, x80), &layout);
}

#[test]
fn with_statement_with_empty_body() {
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::with_(None, None, E::identifier(None, None, "x"), S::empty(None)),
    );
    assert_output(false, "with(x);", &layout);
    assert_output(true, "with (x);", &layout);
}

#[test]
fn indexed_access() {
    let layout = js_layout_generator::type_(
        &opts(),
        &at::Type::new(at::TypeInner::IndexedAccess {
            loc: Loc::none(),
            inner: Arc::new(at::IndexedAccess {
                object: T::unqualified_generic(None, None, None, "T"),
                index: T::unqualified_generic(None, None, None, "K"),
                comments: None,
            }),
        }),
    );
    assert_output(false, "T[K]", &layout);
}

#[test]
fn optional_indexed_access() {
    let layout = js_layout_generator::type_(
        &opts(),
        &at::Type::new(at::TypeInner::OptionalIndexedAccess {
            loc: Loc::none(),
            inner: Arc::new(at::OptionalIndexedAccess {
                indexed_access: at::IndexedAccess {
                    object: T::unqualified_generic(None, None, None, "T"),
                    index: T::unqualified_generic(None, None, None, "K"),
                    comments: None,
                },
                optional: true,
            }),
        }),
    );
    assert_output(false, "T?.[K]", &layout);
}

#[test]
fn enum_of_boolean() {
    let layout = |explicit_type: bool| {
        let et = if explicit_type {
            Some((
                Loc::none(),
                ast::statement::enum_declaration::ExplicitType::Boolean,
            ))
        } else {
            None
        };
        js_layout_generator::statement(
            &opts(),
            false,
            &S::enum_declaration(
                None,
                None,
                None,
                I::identifier(None, "E"),
                S::enum_declarations::body(
                    Loc::none(),
                    Arc::from([
                        S::enum_declarations::boolean_member(
                            S::enum_declarations::initialized_member(
                                None,
                                I::identifier(None, "A"),
                                ast_builder::boolean_literal(None, true),
                            ),
                        ),
                        S::enum_declarations::boolean_member(
                            S::enum_declarations::initialized_member(
                                None,
                                I::identifier(None, "B"),
                                ast_builder::boolean_literal(None, false),
                            ),
                        ),
                    ]),
                    et,
                    None,
                    None,
                ),
            ),
        )
    };
    assert_output(false, "enum E{A=true,B=false,}", &layout(false));
    let pretty_output = String::from("enum E {\n") + "  A = true,\n" + "  B = false,\n" + "}";
    assert_output(true, &pretty_output, &layout(false));

    assert_output(false, "enum E of boolean{A=true,B=false,}", &layout(true));
    let explicit_type_pretty_output =
        String::from("enum E of boolean {\n") + "  A = true,\n" + "  B = false,\n" + "}";
    assert_output(true, &explicit_type_pretty_output, &layout(true));
}

#[test]
fn enum_of_number() {
    let layout = |explicit_type: bool| {
        let et = if explicit_type {
            Some((
                Loc::none(),
                ast::statement::enum_declaration::ExplicitType::Number,
            ))
        } else {
            None
        };
        js_layout_generator::statement(
            &opts(),
            false,
            &S::enum_declaration(
                None,
                None,
                None,
                I::identifier(None, "E"),
                S::enum_declarations::body(
                    Loc::none(),
                    Arc::from([
                        S::enum_declarations::number_member(
                            S::enum_declarations::initialized_member(
                                None,
                                I::identifier(None, "A"),
                                ast_builder::number_literal(None, 1.0, "1"),
                            ),
                        ),
                        S::enum_declarations::number_member(
                            S::enum_declarations::initialized_member(
                                None,
                                I::identifier(None, "B"),
                                ast_builder::number_literal(None, 2.0, "2"),
                            ),
                        ),
                    ]),
                    et,
                    None,
                    None,
                ),
            ),
        )
    };
    assert_output(false, "enum E{A=1,B=2,}", &layout(false));
    let pretty_output = String::from("enum E {\n") + "  A = 1,\n" + "  B = 2,\n" + "}";
    assert_output(true, &pretty_output, &layout(false));

    assert_output(false, "enum E of number{A=1,B=2,}", &layout(true));
    let explicit_type_pretty_output =
        String::from("enum E of number {\n") + "  A = 1,\n" + "  B = 2,\n" + "}";
    assert_output(true, &explicit_type_pretty_output, &layout(true));
}

#[test]
fn enum_of_string_initialized() {
    let layout = |explicit_type: bool| {
        let et = if explicit_type {
            Some((
                Loc::none(),
                ast::statement::enum_declaration::ExplicitType::String,
            ))
        } else {
            None
        };
        js_layout_generator::statement(
            &opts(),
            false,
            &S::enum_declaration(
                None,
                None,
                None,
                I::identifier(None, "E"),
                S::enum_declarations::body(
                    Loc::none(),
                    Arc::from([
                        S::enum_declarations::string_member(
                            S::enum_declarations::initialized_member(
                                None,
                                I::identifier(None, "A"),
                                ast_builder::string_literal(None, "a"),
                            ),
                        ),
                        S::enum_declarations::string_member(
                            S::enum_declarations::initialized_member(
                                None,
                                I::identifier(None, "B"),
                                ast_builder::string_literal(None, "b"),
                            ),
                        ),
                    ]),
                    et,
                    None,
                    None,
                ),
            ),
        )
    };
    assert_output(false, "enum E{A=\"a\",B=\"b\",}", &layout(false));
    let pretty_output = String::from("enum E {\n") + "  A = \"a\",\n" + "  B = \"b\",\n" + "}";
    assert_output(true, &pretty_output, &layout(false));

    assert_output(false, "enum E of string{A=\"a\",B=\"b\",}", &layout(true));
    let explicit_type_pretty_output =
        String::from("enum E of string {\n") + "  A = \"a\",\n" + "  B = \"b\",\n" + "}";
    assert_output(true, &explicit_type_pretty_output, &layout(true));
}

#[test]
fn enum_of_string_defaulted() {
    let layout = |explicit_type: bool| {
        let et = if explicit_type {
            Some((
                Loc::none(),
                ast::statement::enum_declaration::ExplicitType::String,
            ))
        } else {
            None
        };
        js_layout_generator::statement(
            &opts(),
            false,
            &S::enum_declaration(
                None,
                None,
                None,
                I::identifier(None, "E"),
                S::enum_declarations::body(
                    Loc::none(),
                    Arc::from([
                        S::enum_declarations::defaulted_member_of(
                            S::enum_declarations::defaulted_member(None, I::identifier(None, "A")),
                        ),
                        S::enum_declarations::defaulted_member_of(
                            S::enum_declarations::defaulted_member(None, I::identifier(None, "B")),
                        ),
                    ]),
                    et,
                    None,
                    None,
                ),
            ),
        )
    };
    assert_output(false, "enum E{A,B,}", &layout(false));
    let pretty_output = String::from("enum E {\n") + "  A,\n" + "  B,\n" + "}";
    assert_output(true, &pretty_output, &layout(false));

    assert_output(false, "enum E of string{A,B,}", &layout(true));
    let explicit_type_pretty_output =
        String::from("enum E of string {\n") + "  A,\n" + "  B,\n" + "}";
    assert_output(true, &explicit_type_pretty_output, &layout(true));
}

#[test]
fn enum_of_symbol() {
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::enum_declaration(
            None,
            None,
            None,
            I::identifier(None, "E"),
            S::enum_declarations::body(
                Loc::none(),
                Arc::from([
                    S::enum_declarations::defaulted_member_of(
                        S::enum_declarations::defaulted_member(None, I::identifier(None, "A")),
                    ),
                    S::enum_declarations::defaulted_member_of(
                        S::enum_declarations::defaulted_member(None, I::identifier(None, "B")),
                    ),
                ]),
                Some((
                    Loc::none(),
                    ast::statement::enum_declaration::ExplicitType::Symbol,
                )),
                None,
                None,
            ),
        ),
    );
    assert_output(false, "enum E of symbol{A,B,}", &layout);
    let pretty_output = String::from("enum E of symbol {\n") + "  A,\n" + "  B,\n" + "}";
    assert_output(true, &pretty_output, &layout);
}

#[test]
fn enum_with_unknown_members() {
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::enum_declaration(
            None,
            None,
            None,
            I::identifier(None, "E"),
            S::enum_declarations::body(
                Loc::none(),
                Arc::from([
                    S::enum_declarations::defaulted_member_of(
                        S::enum_declarations::defaulted_member(None, I::identifier(None, "A")),
                    ),
                    S::enum_declarations::defaulted_member_of(
                        S::enum_declarations::defaulted_member(None, I::identifier(None, "B")),
                    ),
                ]),
                Some((
                    Loc::none(),
                    ast::statement::enum_declaration::ExplicitType::Symbol,
                )),
                Some(Loc::none()),
                None,
            ),
        ),
    );
    assert_output(false, "enum E of symbol{A,B,...}", &layout);
    let pretty_output =
        String::from("enum E of symbol {\n") + "  A,\n" + "  B,\n" + "  ...\n" + "}";
    assert_output(true, &pretty_output, &layout);
}

#[test]
fn enum_with_internal_comment() {
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::enum_declaration(
            None,
            None,
            None,
            I::identifier(None, "E"),
            S::enum_declarations::body(
                Loc::none(),
                Arc::from([
                    S::enum_declarations::defaulted_member_of(
                        S::enum_declarations::defaulted_member(None, I::identifier(None, "A")),
                    ),
                    S::enum_declarations::defaulted_member_of(
                        S::enum_declarations::defaulted_member(None, I::identifier(None, "B")),
                    ),
                ]),
                Some((
                    Loc::none(),
                    ast::statement::enum_declaration::ExplicitType::Symbol,
                )),
                None,
                Some(ast::Syntax {
                    leading: Arc::from([]),
                    trailing: Arc::from([]),
                    internal: Arc::from([C::line(None, Some(true), " internal comment")]),
                }),
            ),
        ),
    );
    assert_output(
        false,
        "enum E of symbol{A,B,// internal comment\n}",
        &layout,
    );
    let pretty_output = "enum E of symbol {\n  A,\n  B,\n  // internal comment\n  \n}";
    assert_output(true, pretty_output, &layout);
}

#[test]
fn match_expression() {
    assert_expression_string(false, None, None, "match(x){1=>true,2=>false}");
    assert_expression_string(
        false,
        None,
        None,
        "match(x){1 if(b)=>true,2 if(f())=>false}",
    );
    assert_expression_string(
        true,
        None,
        None,
        "match (x) {\n  1 if (b) => true,\n  2 if (f()) => false,\n}",
    );
    assert_expression_string(
        true,
        None,
        None,
        "match (x) {\n  1 => true,\n  2 => false,\n}",
    );
}

#[test]
fn match_statement() {
    assert_statement_string(false, None, "match(x){1=>{const x=true}2=>{const y=false}}");
    assert_statement_string(
        false,
        None,
        "match(x){1 if(b)=>{const x=true}2 if(f())=>{const y=false}}",
    );
    assert_statement_string(
        true,
        None,
        "match (x) {\n  1 => {\n    const x = true;\n  }\n  2 => {\n    const y = false;\n  }\n}",
    );
}

#[test]
fn match_pattern_core() {
    assert_expression_string(
        false,
        None,
        None,
        "match(x){-1=>true,+2=>false,-3n=>true,+4n=>false}",
    );
    assert_expression_string(
        false,
        None,
        None,
        "match(x){0=>0,1n=>1,\"s\"=>2,true=>3,null=>4,x=>5,const y=>6,_=>7}",
    );
}

#[test]
fn match_pattern_object() {
    assert_expression_string(false, None, None, "match(x){{foo:true}=>2}");
    assert_expression_string(
        true,
        None,
        None,
        "match (x) {\n  {foo: true, bar: 1, const x, let y} => 2,\n}",
    );
    assert_expression_string(false, None, None, "match(x){{foo:1,...const x}=>x}");
}

#[test]
fn match_pattern_array() {
    assert_expression_string(false, None, None, "match(x){[1,false]=>x}");
    assert_expression_string(true, None, None, "match (x) {\n  [1, false] => x,\n}");
    assert_expression_string(false, None, None, "match(x){[1,...]=>x}");
    assert_expression_string(false, None, None, "match(x){[1,...const x]=>x}");
}

#[test]
fn match_pattern_instance() {
    assert_expression_string(false, None, None, "match(x){Point{x:1,y:2}=>0}");
    assert_expression_string(
        true,
        None,
        None,
        "match (x) {\n  Point {x: 1, const y} => y,\n}",
    );
    assert_expression_string(false, None, None, "match(x){Point{x:1,...const r}=>r}");
}

#[test]
fn match_pattern_or_as() {
    assert_expression_string(false, None, None, "match(x){1|2|3=>true}");
    assert_expression_string(false, None, None, "match(x){{foo:1|2|3}=>true}");
    assert_expression_string(true, None, None, "match (x) {\n  1 | 2 | 3 => true,\n}");
    assert_expression_string(false, None, None, "match(x){{foo:[1] as y}=>true}");
    assert_expression_string(false, None, None, "match(x){{foo:(1|2|3) as y}=>true}");
}

#[test]
fn match_pattern_member() {
    assert_expression_string(false, None, None, "match(x){foo.bar=>true}");
    assert_expression_string(false, None, None, "match(x){foo[1]=>true}");
    assert_expression_string(false, None, None, "match(x){foo[\"bar\"]=>true}");
    assert_expression_string(false, None, None, "match(x){foo.bar[1]=>true}");
    assert_expression_string(false, None, None, "match(x){foo[1].bar[\"baz\"]=>true}");
}

#[test]
fn arrow_function_with_function_return_type() {
    assert_expression_string(false, None, None, "():((x)=>y)=>{}");
    assert_expression_string(false, None, None, "():((x)=>y)%checks=>{}");
    assert_expression_string(false, None, None, "():()=>x=>{}");
    assert_expression_string(false, None, None, "():()=>x%checks=>{}");
    assert_expression_string(false, None, None, "():(...x)=>y=>{}");
    assert_expression_string(false, None, None, "():(...x)=>y%checks=>{}");
}

#[test]
fn nullish_coalesce_with_logical() {
    assert_expression_string(false, None, None, "(a&&b)??c");
    assert_expression_string(false, None, None, "a??(b&&c)");
    assert_expression_string(false, None, None, "a&&(b??c)");
    assert_expression_string(false, None, None, "(a??b)&&c");

    assert_expression_string(false, None, None, "(a||b)??c");
    assert_expression_string(false, None, None, "a??(b||c)");
    assert_expression_string(false, None, None, "a||(b??c)");
    assert_expression_string(false, None, None, "(a??b)||c");
}

#[test]
fn optional_call() {
    assert_expression_string(false, None, None, "foo?.()");
}

#[test]
fn class_preserve_blank_lines_between_elements() {
    // Single blank line is preserved
    assert_expression_string(true, None, None, "class C {\n  a;\n  \n  b;\n}");
    // Multiple blank lines are condensed to a single blank line
    assert_expression(
        true,
        None,
        None,
        "class C {\n  a;\n  \n  b;\n}",
        &ast_builder::test_expression_of_string("class C {\n  a;\n  \n  \n b;\n}"),
    );
    // Comments are not treated as blank lines
    assert_expression_string(true, None, None, "class C {\n  a;\n  //L\n  b;\n}");
}

#[test]
fn object_type_preserve_blank_lines_between_properties() {
    // Single blank line is preserved
    assert_statement_string(true, None, "type T = {\n  a: 1,\n  \n  b: 2,\n};");
    // Multiple blank lines are condensed to a single blank line
    assert_statement(
        true,
        None,
        "type T = {\n  a: 1,\n  \n  b: 2,\n};",
        &ast_builder::test_statement_of_string("type T = {\n  a: 1,\n  \n  \n b: 2,\n};"),
    );
    // Comments are not treated as blank lines
    assert_statement_string(true, None, "type T = {\n  a: 1,\n  //L\n  b: 2,\n};");
}

#[test]
fn object_pattern_preserve_blank_lines_between_properties() {
    // Single blank line is preserved
    assert_statement_string(true, None, "var {\n  a,\n  \n  b\n};");
    // Multiple blank lines are condensed to a single blank line
    assert_statement(
        true,
        None,
        "var {\n  a,\n  \n  b\n};",
        &ast_builder::test_statement_of_string("var {\n  a,\n  \n  \n b\n};"),
    );
    // Comments are not treated as blank lines
    assert_statement_string(true, None, "var {\n  a,\n  //L\n  b\n};");
}

#[test]
fn switch_preserve_blank_lines_between_cases() {
    // Single blank line is preserved
    assert_statement_string(
        true,
        None,
        "switch (true) {\n  case a:\n    break;\n  \n  case b:\n    break;\n}",
    );
    // Multiple blank lines are condensed to a single blank line
    assert_statement(
        true,
        None,
        "switch (true) {\n  case a:\n    break;\n  \n  case b:\n    break;\n}",
        &ast_builder::test_statement_of_string(
            "switch (true) {\n  case a:\n    break;\n  \n  \n  case b:\n    break;\n}",
        ),
    );
    // Comments are not treated as blank lines
    assert_statement_string(
        true,
        None,
        "switch (true) {\n  case a:\n    break;\n  //L\n  case b:\n    break;\n}",
    );
}

#[test]
fn object_type_preserve_wrapping() {
    // Object type that fits on single line with no wrapping is printed on single line
    assert_statement_string(true, None, "type T = { a: 1 };");
    // Object type that fits on single line but wraps is printed as wrapping
    assert_statement_string(true, None, "type T = {\n  a: 1,\n};");
}

#[test]
fn function_params_preserve_blank_lines_between_params() {
    // Single blank line is preserved
    assert_expression_string(true, None, None, "(\n  a,\n  \n  b,\n) => {}");
    // Multiple blank lines are condensed to a single blank line
    assert_expression(
        true,
        None,
        None,
        "(\n  a,\n  \n  b,\n) => {}",
        &ast_builder::test_expression_of_string("(\n  a,\n  \n  \n  b,\n) => {}"),
    );
    // Comments are not treated as blank lines
    assert_expression_string(true, None, None, "(\n  a,\n  //L\n  b,\n) => {}");
}

#[test]
fn function_type_params_preserve_blank_lines_between_params() {
    // Single blank line is preserved
    assert_statement_string(true, None, "type T = (\n  a,\n  \n  b\n) => c;");
    // Multiple blank lines are condensed to a single blank line
    assert_statement(
        true,
        None,
        "type T = (\n  a,\n  \n  b\n) => c;",
        &ast_builder::test_statement_of_string("type T = (\n  a,\n  \n  \n  b\n) => c;"),
    );
    // Comments are not treated as blank lines
    assert_statement_string(true, None, "type T = (\n  a,\n  //L\n  b\n) => c;");
}

#[test]
fn array_preserve_blank_lines_between_elements() {
    // Single blank line is preserved
    assert_expression_string(true, None, None, "[\n  a,\n  \n  b,\n]");
    // Multiple blank lines are condensed to a single blank line
    assert_expression(
        true,
        None,
        None,
        "[\n  a,\n  \n  b,\n]",
        &ast_builder::test_expression_of_string("[\n  a,\n  \n  \n  b,\n]"),
    );
    // Comments are not treated as blank lines
    assert_expression_string(true, None, None, "[\n  a,\n  //L\n  b,\n]");
    // Blank lines between holes are preserved
    assert_expression_string(true, None, None, "[\n  ,\n  \n  ,\n]");
    assert_expression_string(true, None, None, "[\n  a,\n  \n  ,\n]");
    assert_expression_string(true, None, None, "[\n  ,\n  \n  a,\n]");
}

#[test]
fn array_pattern_preserve_blank_lines_between_elements() {
    // Single blank line is preserved
    assert_statement_string(true, None, "var [\n  a,\n  \n  b\n];");
    // Multiple blank lines are condensed to a single blank line
    assert_statement(
        true,
        None,
        "var [\n  a,\n  \n  b\n];",
        &ast_builder::test_statement_of_string("var [\n  a,\n  \n  \n  b\n];"),
    );
    // Comments are not treated as blank lines
    assert_statement_string(true, None, "var [\n  a,\n  //L\n  b\n];");
    // Blank lines between holes are preserved
    assert_statement_string(true, None, "var [\n  ,\n  \n  ,\n  a\n];");
    assert_statement_string(true, None, "var [\n  a,\n  \n  ,\n  \n  b\n];");
}

#[test]
fn call_preserve_blank_lines_between_args() {
    // Single blank line is preserved
    assert_expression_string(true, None, None, "foo(\n  a,\n  \n  b,\n)");
    // Multiple blank lines are condensed to a single blank line
    assert_expression(
        true,
        None,
        None,
        "foo(\n  a,\n  \n  b,\n)",
        &ast_builder::test_expression_of_string("foo(\n  a,\n  \n  \n  b,\n)"),
    );
    // Comments are not treated as blank lines
    assert_expression_string(true, None, None, "foo(\n  a,\n  //L\n  b,\n)");
}

#[test]
fn new_preserve_blank_lines_between_args() {
    // Single blank line is preserved
    assert_expression_string(true, None, None, "new Foo(\n  a,\n  \n  b,\n)");
    // Multiple blank lines are condensed to a single blank line
    assert_expression(
        true,
        None,
        None,
        "new Foo(\n  a,\n  \n  b,\n)",
        &ast_builder::test_expression_of_string("new Foo(\n  a,\n  \n  \n  b,\n)"),
    );
    // Comments are not treated as blank lines
    assert_expression_string(true, None, None, "new Foo(\n  a,\n  //L\n  b,\n)");
}

#[test]
fn type_args_preserve_blank_lines_between_args() {
    // Single blank line is preserved
    assert_statement_string(true, None, "type Foo = Bar<\n  a,\n  \n  b,\n>;");
    // Multiple blank lines are condensed to a single blank line
    assert_statement(
        true,
        None,
        "type Foo = Bar<\n  a,\n  \n  b,\n>;",
        &ast_builder::test_statement_of_string("type Foo = Bar<\n  a,\n  \n  \n  b,\n>;"),
    );
    // Comments are not treated as blank lines
    assert_statement_string(true, None, "type Foo = Bar<\n  a,\n  //L\n  b,\n>;");
}

#[test]
fn call_type_args_preserve_blank_lines_between_args() {
    // Single blank line is preserved
    assert_expression_string(true, None, None, "foo<\n  a,\n  \n  b,\n>()");
    // Multiple blank lines are condensed to a single blank line
    assert_expression(
        true,
        None,
        None,
        "foo<\n  a,\n  \n  b,\n>()",
        &ast_builder::test_expression_of_string("foo<\n  a,\n  \n  \n  b,\n>()"),
    );
    // Comments are not treated as blank lines
    assert_expression_string(true, None, None, "foo<\n  a,\n  //L\n  b,\n>()");
}

#[test]
fn call_template_wrapping() {
    assert_expression_string(true, None, None, "foo(`a\nb`)");
    assert_expression_string(true, None, None, "foo(\n  `a\nb`,\n)");
}

#[test]
fn type_params_preserve_blank_lines_between_params() {
    // Single blank line is preserved
    assert_expression_string(true, None, None, "<\n  a,\n  \n  b,\n>() => {}");
    // Multiple blank lines are condensed to a single blank line
    assert_expression(
        true,
        None,
        None,
        "<\n  a,\n  \n  b,\n>() => {}",
        &ast_builder::test_expression_of_string("<\n  a,\n  \n  \n  b,\n>() => {}"),
    );
    // Comments are not treated as blank lines
    assert_expression_string(true, None, None, "<\n  a,\n  //L\n  b,\n>() => {}");
}

#[test]
fn function_params_break_before_return_type() {
    // If both function params and return type can break, params should break first
    let a20 = "a".repeat(20);
    let b20 = "b".repeat(20);
    assert_statement_string(
        true,
        None,
        &format!(
            "function f(\n  {},\n  {},\n): {{ {}: t, {}: t }} {{}}",
            a20, b20, a20, b20
        ),
    );
}

#[test]
fn function_type_params_break_before_return_type() {
    // If both function param types and return type can break, params should break first
    let a20 = "a".repeat(20);
    let b20 = "b".repeat(20);
    assert_statement_string(
        true,
        None,
        &format!(
            "type T = (\n  {},\n  {}\n) => {{ {}: t, {}: t }};",
            a20, b20, a20, b20
        ),
    );
}

#[test]
fn jsx_in_new_expression() {
    // Verify that JSX wrapped in parens is not reprinted with extra blank lines before and after
    let a80 = "a".repeat(80);
    assert_expression(
        true,
        None,
        None,
        &(String::from("new Foo(\n")
            + "  a,\n"
            + "  <jsx>\n"
            + &format!("    {}\n", a80)
            + "  </jsx>,\n"
            + "  b,\n"
            + ")"),
        &ast_builder::test_expression_of_string(
            &(String::from("new Foo(\n")
                + "  a,\n"
                + "  (\n"
                + "    <jsx>\n"
                + &format!("  {}\n", a80)
                + "    </jsx>\n"
                + "  ),\n"
                + "  b,\n"
                + ")"),
        ),
    );
}

#[test]
fn assignment_arrow_function_rhs() {
    assert_statement_string(true, None, "const x = () => {};");
    assert_statement_string(true, None, "const x = () => y = 123;");
}

#[test]
fn mapped_types() {
    assert_statement_string(true, None, "type T = { [key in keyof O]: T };");
    assert_statement_string(true, None, "type T = { +[key in keyof O]: T };");
    assert_statement_string(true, None, "type T = { -[key in keyof O]: T };");
    assert_statement_string(true, None, "type T = { [key in keyof O]+?: T };");
    assert_statement_string(true, None, "type T = { [key in keyof O]-?: T };");
    assert_statement_string(true, None, "type T = { [key in keyof O]?: T };");
    assert_statement_string(true, None, "type T = { +[key in keyof O]: T };");
    assert_statement_string(true, None, "type T = { -[key in keyof O]: T };");
    assert_statement_string(false, None, "type T={[key in keyof O]:T};");
    assert_statement_string(false, None, "type T={+[key in keyof O]:T};");
    assert_statement_string(false, None, "type T={-[key in keyof O]:T};");
    assert_statement_string(false, None, "type T={[key in keyof O]+?:T};");
    assert_statement_string(false, None, "type T={[key in keyof O]-?:T};");
    assert_statement_string(false, None, "type T={[key in keyof O]?:T};");
    assert_statement_string(false, None, "type T={+[key in keyof O]:T};");
    assert_statement_string(false, None, "type T={-[key in keyof O]:T};");
}

#[test]
fn type_guards() {
    assert_expression_string(true, None, None, "(x: any): x is true => true");
    assert_expression_string(
        true,
        None,
        None,
        "(x: any): x is ((x: true) => true) => true",
    );
    assert_expression_string(true, None, None, "(x: any): implies x is true => true");
    assert_expression_string(
        true,
        None,
        None,
        "(x: any): x is ((x: true) => x is true) => true",
    );
    assert_expression_string(
        true,
        None,
        None,
        "(x: any): x is ((x: true) => x is (x: true) => x is true) => true",
    );
    assert_statement_string(true, None, "declare function f(x: any): x is true;");
    assert_expression_string(true, None, None, "(x: any): asserts x => true");
    assert_expression_string(true, None, None, "(x: any): asserts x is true => true");
    assert_expression_string(
        true,
        None,
        None,
        "(x: any): asserts x is ((x: true) => true) => true",
    );
    assert_statement_string(true, None, "declare function f(x: any): asserts;");
    assert_statement_string(true, None, "declare function f(x: any): asserts x is true;");
}

#[test]
fn render_types() {
    assert_statement_string(true, None, "type X = renders number;");
    assert_statement_string(true, None, "type X = renders (number | string);");
    assert_statement_string(false, None, "type X=renders (number|string);");
    assert_statement_string(true, None, "type X = renders? number;");
    assert_statement_string(false, None, "type X=renders? number;");
    assert_statement_string(true, None, "type X = renders* number;");
    assert_statement_string(false, None, "type X=renders* number;");
}

#[test]
fn unique_symbol() {
    assert_statement_string(true, None, "declare const x: unique symbol;");
    assert_statement_string(true, None, "type T = unique symbol;");
}

#[test]
fn records() {
    assert_statement_string(
        true,
        None,
        "record R<T> implements IFace {\n  foo: number,\n  bar: boolean = true,\n}",
    );
    assert_statement_string(true, None, "record R implements IFace {}");
    assert_statement_string(true, None, "record R<T> {}");
    assert_statement_string(
        false,
        None,
        "record R<T>implements IFace{foo:number,bar:boolean=true,}",
    );
    assert_statement_string(true, None, "record R {\n  f() {}\n}");
    assert_statement_string(true, None, "record R {\n  static async *f() {}\n}");
    assert_statement_string(
        true,
        None,
        "record R implements IFace {\n  static foo: number = 1,\n}",
    );
    assert_expression_string(true, None, None, "R {}");
    assert_expression_string(true, None, None, "R { a: 1, b: 0 }");
    assert_expression_string(true, None, None, "R { a: 1, b: 0 }");
    assert_expression_string(true, None, None, "R { ...x, b: 0 }");
    assert_expression_string(true, None, None, "R<> {}");
    assert_expression_string(true, None, None, "R<T, S> {}");
}
