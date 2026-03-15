/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser_utils::ast_builder;
use flow_parser_utils::ast_builder::expressions as E;
use flow_parser_utils::ast_builder::jsxs as J;
use flow_parser_utils::ast_builder::statements as S;

use crate::js_layout_generator;
use crate::layout_generator_test_utils::*;
use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::*;

fn make_loc(start_line: i32, end_line: i32) -> Loc {
    Loc {
        source: None,
        start: Position {
            line: start_line,
            column: 0,
        },
        end: Position {
            line: end_line,
            column: 0,
        },
    }
}

#[test]
fn test_simple_self_closing() {
    assert_expression_string(false, None, None, "<A/>");
}

#[test]
fn test_simple() {
    assert_expression_string(false, None, None, "<A></A>");
}

#[test]
fn test_namespaced() {
    assert_expression_string(false, None, None, "<A:a/>");
}

#[test]
fn test_member() {
    assert_expression_string(false, None, None, "<A.b/>");
}

#[test]
fn test_nested_member() {
    assert_expression_string(false, None, None, "<A.b.c/>");
}

#[test]
fn test_simple_with_child() {
    assert_expression_string(false, None, None, "<A><B/></A>");
}

#[test]
fn test_simple_with_attribute_and_child() {
    assert_expression_string(true, None, None, "<A a=\"a\"><B /></A>");
}

#[test]
fn test_long_attribute_with_children() {
    let a_loc = make_loc(1, 4);
    let b_loc = make_loc(2, 2);
    let c_loc = make_loc(3, 3);
    let ast = E::jsx_element(
        Some(a_loc.dupe()),
        J::element(
            None,
            None,
            Some(vec![J::attr(
                None,
                J::attr_identifier(None, None, "a".into()),
                Some(J::attr_literal(ast_builder::literals::string(
                    None,
                    &"a".repeat(80),
                ))),
            )]),
            Some(vec![
                J::child_element(
                    Some(b_loc.dupe()),
                    Some(true),
                    None,
                    None,
                    J::identifier(None, "B".into()),
                ),
                J::child_element(
                    Some(c_loc.dupe()),
                    Some(true),
                    None,
                    None,
                    J::identifier(None, "C".into()),
                ),
            ]),
            None,
            J::identifier(None, "A".into()),
        ),
    );
    let layout = L::loc(
        Some(a_loc.dupe()),
        L::fused(vec![
            L::loc(
                None,
                L::group(vec![
                    L::atom("<"),
                    L::id(None, "A"),
                    L::indent(L::fused(vec![
                        L::line(),
                        L::loc(
                            None,
                            L::fused(vec![
                                L::id(None, "a"),
                                L::atom("="),
                                L::loc(
                                    None,
                                    L::fused(vec![
                                        L::atom("\""),
                                        L::atom(&"a".repeat(80)),
                                        L::atom("\""),
                                    ]),
                                ),
                            ]),
                        ),
                    ])),
                    L::atom(">"),
                ]),
            ),
            L::indent(L::fused(vec![
                L::pretty_hardline(),
                L::loc(
                    Some(b_loc.dupe()),
                    L::loc(
                        None,
                        L::group(vec![
                            L::atom("<"),
                            L::id(None, "B"),
                            L::pretty_space(),
                            L::atom("/>"),
                        ]),
                    ),
                ),
                L::hardline(),
                L::loc(
                    Some(c_loc.dupe()),
                    L::loc(
                        None,
                        L::group(vec![
                            L::atom("<"),
                            L::id(None, "C"),
                            L::pretty_space(),
                            L::atom("/>"),
                        ]),
                    ),
                ),
            ])),
            L::pretty_hardline(),
            L::loc(
                None,
                L::fused(vec![L::atom("</"), L::id(None, "A"), L::atom(">")]),
            ),
        ]),
    );
    assert_layout_of_expression(None, None, layout, &ast);
    assert_expression(
        true,
        None,
        None,
        &format!("<A\n  a=\"{}\">\n  <B />\n  <C />\n</A>", "a".repeat(80)),
        &ast,
    );
    assert_expression(
        false,
        None,
        None,
        &format!("<A a=\"{}\"><B/>\n<C/></A>", "a".repeat(80)),
        &ast,
    );
}

#[test]
fn test_borderline_length_with_children() {
    // opening tag is 80 columns. if it's indented, make sure it breaks.
    //
    //    <aaaaaaaaaaaaa bbbb="cccccccccccccccccccccccccccccccccccc" ddddd="eeeeeeeeeeee">
    //      <f />
    //    </aaaaaaaaaaaaa>
    let a_loc = make_loc(1, 4);
    let f_loc = make_loc(2, 2);
    let ast = E::jsx_element(
        Some(a_loc.dupe()),
        J::element(
            None,
            None,
            Some(vec![
                J::attr(
                    None,
                    J::attr_identifier(None, None, "bbbb".into()),
                    Some(J::attr_literal(ast_builder::literals::string(
                        None,
                        "cccccccccccccccccccccccccccccccccccc",
                    ))),
                ),
                J::attr(
                    None,
                    J::attr_identifier(None, None, "ddddd".into()),
                    Some(J::attr_literal(ast_builder::literals::string(
                        None,
                        "eeeeeeeeeeee",
                    ))),
                ),
            ]),
            Some(vec![J::child_element(
                Some(f_loc.dupe()),
                Some(true),
                None,
                None,
                J::identifier(None, "f".into()),
            )]),
            None,
            J::identifier(None, "aaaaaaaaaaaaa".into()),
        ),
    );
    let opts = opts();
    let layout = js_layout_generator::expression(&opts, None, &ast);
    assert_layout(
        L::loc(
            Some(a_loc),
            L::fused(vec![
                L::loc(
                    None,
                    L::group(vec![
                        L::atom("<"),
                        L::id(None, "aaaaaaaaaaaaa"),
                        L::indent(L::fused(vec![
                            L::line(),
                            L::loc(
                                None,
                                L::fused(vec![
                                    L::id(None, "bbbb"),
                                    L::atom("="),
                                    L::loc(
                                        None,
                                        L::fused(vec![
                                            L::atom("\""),
                                            L::atom("cccccccccccccccccccccccccccccccccccc"),
                                            L::atom("\""),
                                        ]),
                                    ),
                                ]),
                            ),
                            L::pretty_line(),
                            L::loc(
                                None,
                                L::fused(vec![
                                    L::id(None, "ddddd"),
                                    L::atom("="),
                                    L::loc(
                                        None,
                                        L::fused(vec![
                                            L::atom("\""),
                                            L::atom("eeeeeeeeeeee"),
                                            L::atom("\""),
                                        ]),
                                    ),
                                ]),
                            ),
                        ])),
                        L::atom(">"),
                    ]),
                ),
                L::indent(L::fused(vec![
                    L::pretty_hardline(),
                    L::loc(
                        Some(f_loc),
                        L::loc(
                            None,
                            L::group(vec![
                                L::atom("<"),
                                L::id(None, "f"),
                                L::pretty_space(),
                                L::atom("/>"),
                            ]),
                        ),
                    ),
                ])),
                L::pretty_hardline(),
                L::loc(
                    None,
                    L::fused(vec![
                        L::atom("</"),
                        L::id(None, "aaaaaaaaaaaaa"),
                        L::atom(">"),
                    ]),
                ),
            ]),
        ),
        layout,
    );
    let layout = js_layout_generator::expression(&opts, None, &ast);
    assert_output(
        false,
        &format!(
            "{}{}{}",
            r#"<aaaaaaaaaaaaa bbbb="cccccccccccccccccccccccccccccccccccc"ddddd="eeeeeeeeeeee">"#,
            "<f/>",
            "</aaaaaaaaaaaaa>"
        ),
        &layout,
    );
    assert_output(
        true,
        &format!(
            "{}\n{}\n{}",
            r#"<aaaaaaaaaaaaa bbbb="cccccccccccccccccccccccccccccccccccc" ddddd="eeeeeeeeeeee">"#,
            "  <f />",
            "</aaaaaaaaaaaaa>"
        ),
        &layout,
    );

    let block_layout = js_layout_generator::statement(
        &opts,
        false,
        &S::block(None, vec![S::expression(None, None, None, ast.clone())]),
    );
    assert_output(
        false,
        &format!(
            "{}{}{}{}{}",
            "{",
            "<aaaaaaaaaaaaa bbbb=\"cccccccccccccccccccccccccccccccccccc\"ddddd=\"eeeeeeeeeeee\">",
            "<f/>",
            "</aaaaaaaaaaaaa>",
            "}"
        ),
        &block_layout,
    );
    assert_output(
        true,
        &format!(
            "{}\n{}\n{}\n{}\n{}\n{}\n{}",
            "{",
            "  <aaaaaaaaaaaaa",
            "    bbbb=\"cccccccccccccccccccccccccccccccccccc\"",
            "    ddddd=\"eeeeeeeeeeee\">",
            "    <f />",
            "  </aaaaaaaaaaaaa>;",
            "}"
        ),
        &block_layout,
    );
}

#[test]
fn test_long_child_text() {
    assert_expression_string(
        true,
        None,
        None,
        &format!("<A a=\"a\">\n  {}\n</A>", "b".repeat(80)),
    );
}

#[test]
fn test_literal_whitespace() {
    assert_expression_string(true, None, None, "<A a=\"a\">\n  a{\" \"}\n  b\n</A>");
}

#[test]
fn test_children_fit_on_one_line() {
    assert_expression_string(true, None, None, "<A><B /><C /></A>");
}

#[test]
fn test_long_child_element() {
    assert_expression_string(
        true,
        None,
        None,
        &format!("<A>\n  <{} />\n  <C />\n</A>", "B".repeat(80)),
    );
}

#[test]
fn test_user_supplied_newlines() {
    // TODO: Utils_jsx.trim_jsx_text is overly aggressive for pretty
    //       printing, user supplied newlines between words should be
    //       maintained.
}

#[test]
fn test_valueless_attribute() {
    // TODO: valueless attributes shouldnt print trailing spaces when last
    assert_expression_string(false, None, None, "<A a />");
}

#[test]
fn test_namespaced_valueless_attribute() {
    assert_expression_string(false, None, None, "<A a:a />");
}

#[test]
fn test_attribute_braces() {
    assert_expression_string(false, None, None, "<A a={1}/>");
}

#[test]
fn test_attribute_string() {
    assert_expression_string(false, None, None, "<A a=\"\"/>");
}

#[test]
fn test_attribute_spread() {
    assert_expression_string(false, None, None, "<A {...a}/>");
}

#[test]
fn test_attribute_spread_between_attributes() {
    assert_expression_string(false, None, None, "<A a {...a}b />");
}

#[test]
fn test_multiple_shorthand_attributes() {
    assert_expression_string(false, None, None, "<A a b />");
}

#[test]
fn test_shorthand_and_longhand_attributes() {
    assert_expression_string(false, None, None, "<A a b=\"\"/>");
}

#[test]
fn test_multiple_longhand_attributes() {
    assert_expression_string(false, None, None, "<A a=\"a\"b={b}/>");
    assert_expression_string(true, None, None, "<A a=\"a\" b=\"b\" />");
    assert_expression_string(
        true,
        None,
        None,
        &format!("<A\n  a=\"{}\"\n  b=\"b\"\n/>", "a".repeat(80)),
    );
}

#[test]
fn test_string_longhand_and_shorthand_attributes() {
    assert_expression_string(false, None, None, "<A b=\"\"a />");
    assert_expression_string(true, None, None, "<A a=\"a\" b />");
    assert_expression_string(
        true,
        None,
        None,
        &format!("<A\n  a=\"{}\"\n  b\n/>", "a".repeat(80)),
    );
}

#[test]
fn test_expression_longhand_and_shorthand_attributes() {
    assert_expression_string(false, None, None, "<A b={1}a />");
}

#[test]
fn test_preserve_blank_lines_between_children() {
    // Single blank line is preserved
    assert_expression_string(true, None, None, "<A>\n  <B />\n  \n  <C />\n</A>");
    // Multiple blank lines are condensed to a single blank line
    assert_expression(
        true,
        None,
        None,
        "<A>\n  <B />\n  \n  <C />\n</A>",
        &ast_builder::test_expression_of_string("<A>\n  <B />\n  \n  \n  <C />\n</A>"),
    );
}

#[test]
fn test_type_args() {
    assert_expression_string(false, None, None, "<A<string>/>");
    assert_expression_string(false, None, None, "<A<string>><div/></A>");
}
