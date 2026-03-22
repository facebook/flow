/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser_utils::ast_builder::expressions as E;
use flow_parser_utils::ast_builder::patterns as P;
use flow_parser_utils::ast_builder::statements as S;

use crate::js_layout_generator;
use crate::layout_generator_test_utils::*;
use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::*;

#[test]
fn let_simple_assign() {
    let opts = js_layout_generator::default_opts();
    let mk_layout = |a: &str| {
        js_layout_generator::statement(
            &opts,
            false,
            &S::let_declaration(
                None,
                vec![S::variable_declarator(
                    None,
                    Some(E::identifier(None, None, "a")),
                    None,
                    a,
                )],
            ),
        )
    };
    let layout = mk_layout("a");
    assert_layout(
        L::loc(
            None,
            L::loc(
                None,
                L::fused(vec![
                    L::atom("let"),
                    L::space(),
                    L::loc(
                        None,
                        L::fused(vec![
                            L::loc(None, L::id(None, "a")),
                            L::pretty_space(),
                            L::atom("="),
                            L::pretty_space(),
                            L::loc(None, L::id(None, "a")),
                        ]),
                    ),
                    L::atom(";"),
                ]),
            ),
        ),
        layout.clone(),
    );
    assert_output(false, "let a=a;", &layout);
    assert_output(true, "let a = a;", &layout);

    let a80 = "a".repeat(80);
    let layout = mk_layout(&a80);
    assert_output(false, &format!("let {}=a;", a80), &layout);
    assert_output(true, &format!("let {} = a;", a80), &layout);
}

#[test]
fn let_simple_object_assign() {
    let opts = js_layout_generator::default_opts();
    let mk_layout = |a: &str| {
        js_layout_generator::statement(
            &opts,
            false,
            &S::let_declaration(
                None,
                vec![S::variable_declarator_generic(
                    None,
                    P::object_(a),
                    Some(E::identifier(None, None, "a")),
                )],
            ),
        )
    };
    let layout = mk_layout("a");
    assert_layout(
        L::loc(
            None,
            L::loc(
                None,
                L::fused(vec![
                    L::atom("let"),
                    L::pretty_space(),
                    L::loc(
                        None,
                        L::fused(vec![
                            L::loc(
                                None,
                                L::group(vec![
                                    L::atom("{"),
                                    L::indent(L::fused(vec![
                                        L::softline(),
                                        L::loc(None, L::id(None, "a")),
                                    ])),
                                    L::softline(),
                                    L::atom("}"),
                                ]),
                            ),
                            L::pretty_space(),
                            L::atom("="),
                            L::pretty_space(),
                            L::loc(None, L::id(None, "a")),
                        ]),
                    ),
                    L::atom(";"),
                ]),
            ),
        ),
        layout.clone(),
    );
    assert_output(false, "let{a}=a;", &layout);
    assert_output(true, "let {a} = a;", &layout);

    let a80 = "a".repeat(80);
    let layout = mk_layout(&a80);
    assert_output(false, &format!("let{{{}}}=a;", a80), &layout);
    assert_output(true, &format!("let {{\n  {}\n}} = a;", a80), &layout);
}

#[test]
fn let_assign_annotation() {
    assert_statement_string(false, None, "let a:b=a;");
}

#[test]
fn let_empty_object() {
    assert_statement_string(false, None, "let{}=a;");
}

#[test]
fn let_empty_object_annotation() {
    assert_statement_string(false, None, "let{}:b=a;");
}

#[test]
fn let_object_single_var() {
    assert_statement_string(false, None, "let{a}=a;");
}

#[test]
fn let_object_aliased_var() {
    assert_statement_string(false, None, "let{a:b}=a;");
    assert_statement_string(true, None, "let {a: b} = a;");
}

#[test]
fn let_object_multiple_vars() {
    assert_statement_string(false, None, "let{a,b}=a;");
}

#[test]
fn let_object_multiple_vars_aliased() {
    assert_statement_string(false, None, "let{a:b,c}=a;");
    assert_statement_string(true, None, "let {a: b, c} = a;");
}

#[test]
fn let_object_nested() {
    assert_statement_string(false, None, "let{a,b:{c}}=a;");
}

#[test]
fn let_object_default() {
    assert_statement_string(false, None, "let{a=b}=a;");
}

#[test]
fn let_object_aliased_default() {
    assert_statement_string(false, None, "let{a:b=c}=a;");
}

#[test]
fn let_object_alias_and_default() {
    assert_statement_string(false, None, "let{a:b,c=d}=a;");
    assert_statement_string(true, None, "let {a: b, c = d} = a;");
}

#[test]
fn let_object_alias_and_default_long() {
    let d80 = "d".repeat(80);
    assert_statement_string(false, None, &format!("let{{a:b,c={}}}=a;", d80));
    assert_statement_string(
        true,
        None,
        &format!("let {{\n  a: b,\n  c = {}\n}} = a;", d80),
    );
}

#[test]
fn let_object_default_expression() {
    assert_statement_string(false, None, "let{a=++b}=a;");
}

#[test]
fn let_object_rest() {
    assert_statement_string(false, None, "let{...a}=a;");
    assert_statement_string(true, None, "let {...a} = a;");
}

#[test]
fn let_object_var_rest() {
    assert_statement_string(false, None, "let{a,...b}=a;");
}

#[test]
fn let_object_var_rest_long() {
    let c80 = "c".repeat(80);
    assert_statement_string(false, None, &format!("let{{a,...{}}}=a;", c80));
    assert_statement_string(true, None, &format!("let {{\n  a,\n  ...{}\n}} = a;", c80));
}

#[test]
fn let_array_empty() {
    assert_statement_string(false, None, "let[]=a;");
}

#[test]
fn let_array_annotated() {
    assert_statement_string(false, None, "let[]:a=a;");
}

#[test]
fn let_array_single_item() {
    let opts = js_layout_generator::default_opts();
    let mk_layout = |a: &str| {
        js_layout_generator::statement(
            &opts,
            false,
            &S::let_declaration(
                None,
                vec![S::variable_declarator_generic(
                    None,
                    P::array(vec![Some(P::identifier(None, None, a))]),
                    Some(E::identifier(None, None, "a")),
                )],
            ),
        )
    };
    let layout = mk_layout("a");
    assert_layout(
        L::loc(
            None,
            L::loc(
                None,
                L::fused(vec![
                    L::atom("let"),
                    L::pretty_space(),
                    L::loc(
                        None,
                        L::fused(vec![
                            L::loc(
                                None,
                                L::group(vec![
                                    L::atom("["),
                                    L::indent(L::fused(vec![
                                        L::softline(),
                                        L::loc(None, L::loc(None, L::id(None, "a"))),
                                    ])),
                                    L::softline(),
                                    L::atom("]"),
                                ]),
                            ),
                            L::pretty_space(),
                            L::atom("="),
                            L::pretty_space(),
                            L::loc(None, L::id(None, "a")),
                        ]),
                    ),
                    L::atom(";"),
                ]),
            ),
        ),
        layout.clone(),
    );
    assert_output(false, "let[a]=a;", &layout);
    assert_output(true, "let [a] = a;", &layout);

    let a80 = "a".repeat(80);
    let layout = mk_layout(&a80);
    assert_output(false, &format!("let[{}]=a;", a80), &layout);
    assert_output(true, &format!("let [\n  {}\n] = a;", a80), &layout);
}

#[test]
fn let_array_annotated_item() {
    assert_statement_string(false, None, "let[a:b]=a;");
}

#[test]
fn let_array_multiple_items() {
    assert_statement_string(false, None, "let[a,b]=a;");
    assert_statement_string(true, None, "let [a, b] = a;");
}

#[test]
fn let_array_holes() {
    assert_statement_string(false, None, "let[,,a]=a;");
    assert_statement_string(true, None, "let [a, , b] = a;");
    assert_statement_string(
        true,
        None,
        &format!("let [\n  a,\n  ,\n  {}\n] = a;", "b".repeat(80)),
    );
}

#[test]
fn let_nested_array() {
    assert_statement_string(false, None, "let[[]]=a;");
}

#[test]
fn let_array_holes_and_nested() {
    assert_statement_string(false, None, "let[,,[a]]=a;");
}

#[test]
fn let_array_spread() {
    assert_statement_string(false, None, "let[...a]=a;");
}

#[test]
fn let_array_item_and_spread() {
    assert_statement_string(false, None, "let[a,...b]=a;");
    assert_statement_string(true, None, "let [a, ...b] = a;");
    assert_statement_string(
        true,
        None,
        &format!("let [\n  a,\n  ...b{}\n] = a;", "c".repeat(80)),
    );
}
