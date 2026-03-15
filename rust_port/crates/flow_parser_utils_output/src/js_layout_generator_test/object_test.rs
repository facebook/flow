/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser_utils::ast_builder::expressions as E;

use crate::js_layout_generator;
use crate::layout::LayoutNode;
use crate::layout_generator_test_utils::*;
use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::*;

fn expected_object2_layout(
    prop1: &flow_parser::ast::expression::object::Property<
        flow_parser::loc::Loc,
        flow_parser::loc::Loc,
    >,
    prop2: &flow_parser::ast::expression::object::Property<
        flow_parser::loc::Loc,
        flow_parser::loc::Loc,
    >,
) -> LayoutNode {
    let opts = opts();
    let prop1_layout = js_layout_generator::object_property(&opts, prop1);
    let prop2_layout = js_layout_generator::object_property(&opts, prop2);
    L::loc(
        None,
        L::group(vec![
            L::atom("{"),
            L::indent(L::fused(vec![
                L::pretty_line(),
                prop1_layout,
                L::atom(","),
                L::pretty_line(),
                prop2_layout,
                LayoutNode::if_break(L::atom(","), L::empty()),
            ])),
            L::pretty_line(),
            L::atom("}"),
        ]),
    )
}

#[test]
fn empty_object() {
    let ast = E::object_(None, None, vec![]);
    let layout = js_layout_generator::expression(&opts(), None, &ast);
    assert_layout(
        L::loc(None, L::group(vec![L::atom("{"), L::atom("}")])),
        layout.clone(),
    );
    assert_output(false, "{}", &layout);
    assert_output(true, "{}", &layout);
}

// `{ foo: x, bar: y }` rather than `{foo: x, bar: y}`
#[test]
fn flat_spaces_inside_braces() {
    let prop1 = E::object_property(
        None,
        None,
        E::object_property_key(None, "foo"),
        E::identifier(None, None, "x"),
    );
    let prop2 = E::object_property(
        None,
        None,
        E::object_property_key(None, "bar"),
        E::identifier(None, None, "y"),
    );
    let ast = E::object_(None, None, vec![prop1.clone(), prop2.clone()]);
    let layout = js_layout_generator::expression(&opts(), None, &ast);
    assert_layout(expected_object2_layout(&prop1, &prop2), layout.clone());
    assert_output(false, "{foo:x,bar:y}", &layout);
    assert_output(true, "{ foo: x, bar: y }", &layout);
}

// if it wraps, there's a trailing comma
#[test]
fn newlines_and_trailing_comma() {
    let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
    let prop1 = E::object_property(
        None,
        None,
        E::object_property_key(None, "foo"),
        E::identifier(None, None, x40),
    );
    let prop2 = E::object_property(
        None,
        None,
        E::object_property_key(None, "bar"),
        E::identifier(None, None, x40),
    );
    let ast = E::object_(None, None, vec![prop1.clone(), prop2.clone()]);
    let layout = js_layout_generator::expression(&opts(), None, &ast);
    assert_layout(expected_object2_layout(&prop1, &prop2), layout.clone());
    assert_output(false, &format!("{{foo:{},bar:{}}}", x40, x40), &layout);
    assert_output(
        true,
        &format!("{{\n  foo: {},\n  bar: {},\n}}", x40, x40),
        &layout,
    );
}

#[test]
fn object_property_is_function() {
    let prop1 = E::object_property(
        None,
        None,
        E::object_property_key(None, "foo"),
        E::identifier(None, None, "x"),
    );
    let prop2 = E::object_property(
        None,
        None,
        E::object_property_key(None, "bar"),
        E::function_(None, None, None, None, None, None),
    );
    let prop3 = E::object_property(
        None,
        None,
        E::object_property_key(None, "baz"),
        E::identifier(None, None, "y"),
    );
    let ast = E::object_(
        None,
        None,
        vec![prop1.clone(), prop2.clone(), prop3.clone()],
    );
    let layout = js_layout_generator::expression(&opts(), None, &ast);
    let prop1_layout = js_layout_generator::object_property(&opts(), &prop1);
    let prop2_layout = js_layout_generator::object_property(&opts(), &prop2);
    let prop3_layout = js_layout_generator::object_property(&opts(), &prop3);
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::atom("{"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    prop1_layout,
                    L::atom(","),
                    L::pretty_line(),
                    prop2_layout,
                    L::atom(","),
                    L::pretty_line(),
                    prop3_layout,
                    LayoutNode::if_break(L::atom(","), L::empty()),
                ])),
                L::pretty_line(),
                L::atom("}"),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "{foo:x,bar:function(){},baz:y}", &layout);
    assert_output(true, "{ foo: x, bar: function() {}, baz: y }", &layout);
}

#[test]
fn object_property_is_method() {
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::object_(
            None,
            None,
            vec![E::object_method(
                None,
                None,
                None,
                None,
                None,
                E::object_property_key(None, "foo"),
            )],
        ),
    );
    assert_output(false, "{foo(){}}", &layout);
    assert_output(true, "{ foo() {} }", &layout);
}

#[test]
fn object_property_is_generator_method() {
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::object_(
            None,
            None,
            vec![E::object_method(
                None,
                None,
                None,
                Some(true),
                None,
                E::object_property_key(None, "foo"),
            )],
        ),
    );
    assert_output(false, "{*foo(){}}", &layout);
    assert_output(true, "{ *foo() {} }", &layout);
}

#[test]
fn object_property_is_sequence() {
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::object_(
            None,
            None,
            vec![E::object_property(
                None,
                None,
                E::object_property_key(None, "foo"),
                E::sequence(
                    None,
                    None,
                    vec![
                        E::identifier(None, None, "x"),
                        E::identifier(None, None, "y"),
                    ],
                ),
            )],
        ),
    );
    assert_output(false, "{foo:(x,y)}", &layout);
    assert_output(true, "{ foo: (x, y) }", &layout);
}

#[test]
fn object_property_key_is_literal() {
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::object_(
            None,
            None,
            vec![E::object_property_with_string_literal(
                None,
                None,
                "foo",
                E::literals::string(None, None, "bar"),
            )],
        ),
    );
    assert_output(false, "{\"foo\":\"bar\"}", &layout);
    assert_output(true, "{ \"foo\": \"bar\" }", &layout);
}

#[test]
fn object_property_key_is_computed() {
    let b80 = "b".repeat(80);
    let ast = E::object_(
        None,
        None,
        vec![E::object_property(
            None,
            None,
            E::object_property_computed_key(None, None, E::identifier(None, None, &b80)),
            E::literals::number(None, None, 123.0, "123"),
        )],
    );
    let layout = js_layout_generator::expression(&opts(), None, &ast);
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::atom("{"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::loc(
                        None,
                        L::group(vec![
                            L::atom("["),
                            L::indent(L::fused(vec![
                                L::pretty_line(),
                                L::loc(None, L::id(None, &b80)),
                            ])),
                            L::pretty_line(),
                            L::atom("]"),
                            L::atom(":"),
                            L::pretty_space(),
                            L::loc(None, L::atom("123")),
                        ]),
                    ),
                    LayoutNode::if_break(L::atom(","), L::empty()),
                ])),
                L::pretty_line(),
                L::atom("}"),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, &format!("{{[{}]:123}}", b80), &layout);
    assert_output(
        true,
        &format!("{{\n  [\n    {}\n  ]: 123,\n}}", b80),
        &layout,
    );

    let b40 = "b".repeat(40);
    let ast = E::object_(
        None,
        None,
        vec![E::object_property(
            None,
            None,
            E::object_property_computed_key(
                None,
                None,
                E::binary(
                    None,
                    None,
                    flow_parser::ast::expression::BinaryOperator::Plus,
                    E::identifier(None, None, &b40),
                    E::identifier(None, None, &b40),
                ),
            ),
            E::literals::number(None, None, 123.0, "123"),
        )],
    );
    let layout = js_layout_generator::expression(&opts(), None, &ast);
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::atom("{"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::loc(
                        None,
                        L::group(vec![
                            L::atom("["),
                            L::indent(L::fused(vec![
                                L::pretty_line(),
                                L::loc(
                                    None,
                                    L::fused(vec![
                                        L::loc(None, L::id(None, &b40)),
                                        L::pretty_space(),
                                        L::atom("+"),
                                        L::pretty_space(),
                                        L::loc(None, L::id(None, &b40)),
                                    ]),
                                ),
                            ])),
                            L::pretty_line(),
                            L::atom("]"),
                            L::atom(":"),
                            L::pretty_space(),
                            L::loc(None, L::atom("123")),
                        ]),
                    ),
                    LayoutNode::if_break(L::atom(","), L::empty()),
                ])),
                L::pretty_line(),
                L::atom("}"),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, &format!("{{[{}+{}]:123}}", b40, b40), &layout);

    // TODO: the second b40 should wrap
    assert_output(
        true,
        &format!("{{\n  [\n    {} + {}\n  ]: 123,\n}}", b40, b40),
        &layout,
    );
}

#[test]
fn preserve_blank_lines_between_properties() {
    // Single blank line is preserved
    assert_expression_string(true, None, None, "{\n  a: 1,\n  \n  b: 2,\n}");
    // Multiple blank lines are condensed to a single blank line
    assert_expression(
        true,
        None,
        None,
        "{\n  a: 1,\n  \n  b: 2,\n}",
        &flow_parser_utils::ast_builder::test_expression_of_string("{\n  a: 1,\n  \n  \n b: 2,\n}"),
    );
    // Comments are not treated as blank lines
    assert_expression_string(true, None, None, "{\n  a: 1,\n  //L\n  b: 2,\n}");
}

#[test]
fn preserve_wrapping() {
    // Object that fits on single line with no wrapping is printed on single line
    assert_expression_string(true, None, None, "{ a: 1 }");
    // Object that fits on single line but wraps is printed as wrapping
    assert_expression_string(true, None, None, "{\n  a: 1,\n}");
}

#[test]
fn bracket_spacing() {
    assert_expression_string(true, None, None, "{ a: 1 }");
    assert_expression_string(
        true,
        None,
        Some(&js_layout_generator::Opts {
            bracket_spacing: false,
            ..js_layout_generator::default_opts()
        }),
        "{a: 1}",
    );
}
