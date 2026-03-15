/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser_utils::ast_builder;
use flow_parser_utils::ast_builder::classes;
use flow_parser_utils::ast_builder::expressions as E;
use flow_parser_utils::ast_builder::expressions;
use flow_parser_utils::ast_builder::functions as F;
use flow_parser_utils::ast_builder::identifiers as I;
use flow_parser_utils::ast_builder::jsxs as J;
use flow_parser_utils::ast_builder::patterns;
use flow_parser_utils::ast_builder::statements as S;

use crate::js_layout_generator;
use crate::layout::LayoutNode;
use crate::layout_generator_test_utils::*;
use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::layout_matcher;
use crate::layout_test_utils::*;

#[test]
fn binary_in_space() {
    let ast = ast_builder::test_statement_of_string(r#"if("foo" in {"foo": bar}){}"#);
    assert_statement(false, None, r#"if("foo"in{"foo":bar}){}"#, &ast);

    let ast = ast_builder::test_statement_of_string(r#"if("foo" in bar){}"#);
    assert_statement(false, None, r#"if("foo"in bar){}"#, &ast);

    let ast = ast_builder::test_statement_of_string(r#"if(foo in {"foo":bar}){}"#);
    assert_statement(false, None, r#"if(foo in{"foo":bar}){}"#, &ast);
}

#[test]
fn binary_instanceof_space() {
    {
        let ast = E::instanceof(
            E::literals::string(None, None, "foo"),
            E::object_(None, None, vec![]),
        );
        let layout = js_layout_generator::expression(&opts(), None, &ast);
        assert_layout(
            L::loc(
                None,
                L::fused(vec![
                    L::loc(
                        None,
                        L::fused(vec![L::atom("\""), L::atom("foo"), L::atom("\"")]),
                    ),
                    L::pretty_space(),
                    L::atom("instanceof"),
                    L::pretty_space(),
                    L::loc(None, L::group(vec![L::atom("{"), L::atom("}")])),
                ]),
            ),
            layout.clone(),
        );
        assert_output(false, r#""foo"instanceof{}"#, &layout);
        assert_output(true, r#""foo" instanceof {}"#, &layout);
    }

    {
        let ast = E::instanceof(
            E::literals::string(None, None, "foo"),
            E::identifier(None, None, "bar"),
        );
        let layout = js_layout_generator::expression(&opts(), None, &ast);
        assert_layout(
            L::loc(
                None,
                L::fused(vec![
                    L::loc(
                        None,
                        L::fused(vec![L::atom("\""), L::atom("foo"), L::atom("\"")]),
                    ),
                    L::pretty_space(),
                    L::atom("instanceof"),
                    L::space(),
                    L::loc(None, L::id(None, "bar")),
                ]),
            ),
            layout.clone(),
        );
        assert_output(false, r#""foo"instanceof bar"#, &layout);
        assert_output(true, r#""foo" instanceof bar"#, &layout);
    }

    let ast = E::instanceof(
        E::identifier(None, None, "foo"),
        E::object_(None, None, vec![]),
    );
    let layout = js_layout_generator::expression(&opts(), None, &ast);
    assert_layout(
        L::loc(
            None,
            L::fused(vec![
                L::loc(None, L::id(None, "foo")),
                L::space(),
                L::atom("instanceof"),
                L::pretty_space(),
                L::loc(None, L::group(vec![L::atom("{"), L::atom("}")])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, r#"foo instanceof{}"#, &layout);
    assert_output(true, r#"foo instanceof {}"#, &layout);
}

#[test]
fn logical_wrapping() {
    let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
    let ast = E::logical_and(
        E::identifier(None, None, x40),
        E::identifier(None, None, x40),
    );
    let layout = js_layout_generator::expression(&opts(), None, &ast);
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::loc(
                    None,
                    L::id(None, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                ),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    LayoutNode::if_break(L::hardline(), L::pretty_space()),
                    L::loc(
                        None,
                        L::id(None, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                    ),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, &format!("{}&&{}", x40, x40), &layout);
    assert_output(true, &format!("{} &&\n  {}", x40, x40), &layout);
}

#[test]
fn return_statement_parens() {
    let ret = S::return_(None, None, None);
    assert_statement(false, None, "return;", &ret);

    let x = E::identifier(None, None, "x");
    let y = E::identifier(None, None, "y");
    let seq = E::sequence(None, None, vec![x.clone(), y.clone()]);
    let ret = S::return_(None, None, Some(seq));
    assert_statement(false, None, "return x,y;", &ret);
    assert_statement(true, None, "return x, y;", &ret);

    // sequences get split across lines and wrapped in parens
    let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
    let y40 = "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy";
    let func = S::function_declaration(
        None,
        None,
        None,
        None,
        Some(F::body(
            None,
            None,
            vec![S::return_(
                None,
                None,
                Some(E::sequence(
                    None,
                    None,
                    vec![
                        E::identifier(None, None, x40),
                        E::identifier(None, None, y40),
                    ],
                )),
            )],
        )),
        I::identifier(None, "f"),
    );
    assert_layout_result(
        L::loc(
            None,
            L::fused(vec![
                L::atom("return"),
                L::space(),
                L::group(vec![
                    LayoutNode::if_break(L::atom("("), L::empty()),
                    L::indent(L::fused(vec![
                        L::softline(),
                        L::loc(
                            None,
                            L::group(vec![
                                L::loc(
                                    None,
                                    L::id(None, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                ),
                                L::atom(","),
                                L::pretty_line(),
                                L::loc(
                                    None,
                                    L::id(None, "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"),
                                ),
                            ]),
                        ),
                    ])),
                    L::softline(),
                    LayoutNode::if_break(L::atom(")"), L::empty()),
                ]),
                LayoutNode::if_pretty(L::atom(";"), L::empty()),
            ]),
        ),
        layout_matcher::body_of_function_declaration(&func)
            .and_then(|l| layout_matcher::nth_fused(0, &l)),
    );
    assert_statement(
        false,
        None,
        &format!("function f(){{return {},{}}}", x40, y40),
        &func,
    );
    assert_statement(
        true,
        None,
        &format!(
            "function f() {{\n  return (\n    {},\n    {}\n  );\n}}",
            x40, y40
        ),
        &func,
    );

    // logicals get split
    let logical = E::logical_and(
        E::identifier(None, None, x40),
        E::identifier(None, None, y40),
    );
    let func = S::function_declaration(
        None,
        None,
        None,
        None,
        Some(F::body(
            None,
            None,
            vec![S::return_(None, None, Some(logical.clone()))],
        )),
        I::identifier(None, "f"),
    );
    assert_layout_result(
        L::loc(
            None,
            L::fused(vec![
                L::atom("return"),
                L::space(),
                L::group(vec![
                    LayoutNode::if_break(L::atom("("), L::empty()),
                    L::indent(L::fused(vec![
                        L::softline(),
                        L::expression(None, None, &logical),
                    ])),
                    L::softline(),
                    LayoutNode::if_break(L::atom(")"), L::empty()),
                ]),
                LayoutNode::if_pretty(L::atom(";"), L::empty()),
            ]),
        ),
        layout_matcher::body_of_function_declaration(&func)
            .and_then(|l| layout_matcher::nth_fused(0, &l)),
    );
    assert_statement(
        true,
        None,
        &format!(
            "function f() {{\n  return (\n    {} &&\n      {}\n  );\n}}",
            x40, y40
        ),
        &func,
    );

    // binary expressions get split
    let plus = E::plus(
        E::identifier(None, None, x40),
        E::identifier(None, None, y40),
    );
    let func = S::function_declaration(
        None,
        None,
        None,
        None,
        Some(F::body(
            None,
            None,
            vec![S::return_(None, None, Some(plus.clone()))],
        )),
        I::identifier(None, "f"),
    );
    assert_layout_result(
        L::loc(
            None,
            L::fused(vec![
                L::atom("return"),
                L::space(),
                L::group(vec![
                    LayoutNode::if_break(L::atom("("), L::empty()),
                    L::indent(L::fused(vec![
                        L::softline(),
                        L::expression(None, None, &plus),
                    ])),
                    L::softline(),
                    LayoutNode::if_break(L::atom(")"), L::empty()),
                ]),
                LayoutNode::if_pretty(L::atom(";"), L::empty()),
            ]),
        ),
        layout_matcher::body_of_function_declaration(&func)
            .and_then(|l| layout_matcher::nth_fused(0, &l)),
    );
    assert_statement(
        true,
        None,
        &format!(
            "function f() {{\n  return (\n    {} + {}\n  );\n}}",
            x40, y40
        ),
        &func,
    );

    // jsx gets split
    let long_name = "A".repeat(80);
    let jsx = E::jsx_element(
        None,
        J::element(
            None,
            None,
            None,
            None,
            None,
            J::identifier(None, FlowSmolStr::from(long_name.as_str())),
        ),
    );
    let func = S::function_declaration(
        None,
        None,
        None,
        None,
        Some(F::body(
            None,
            None,
            vec![S::return_(None, None, Some(jsx.clone()))],
        )),
        I::identifier(None, "f"),
    );
    assert_layout_result(
        L::loc(
            None,
            L::fused(vec![
                L::atom("return"),
                L::pretty_space(),
                L::group(vec![
                    LayoutNode::if_break(L::atom("("), L::empty()),
                    L::indent(L::fused(vec![
                        L::softline(),
                        L::expression(None, None, &jsx),
                    ])),
                    L::softline(),
                    LayoutNode::if_break(L::atom(")"), L::empty()),
                ]),
                LayoutNode::if_pretty(L::atom(";"), L::empty()),
            ]),
        ),
        layout_matcher::body_of_function_declaration(&func)
            .and_then(|l| layout_matcher::nth_fused(0, &l)),
    );
    assert_statement(
        true,
        None,
        &format!(
            "function f() {{\n  return (\n    <{}></{}>\n  );\n}}",
            long_name, long_name
        ),
        &func,
    );

    // a string doesn't get split
    let x80 = format!("{}{}", x40, x40);
    let func = S::function_declaration(
        None,
        None,
        None,
        None,
        Some(F::body(
            None,
            None,
            vec![S::return_(
                None,
                None,
                Some(E::identifier(None, None, &x80)),
            )],
        )),
        I::identifier(None, "f"),
    );
    assert_layout_result(
        L::loc(
            None,
            L::fused(vec![
                L::atom("return"),
                L::atom(" "),
                L::loc(
                    None,
                    L::id(
                        None,
                        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                    ),
                ),
                LayoutNode::if_pretty(L::atom(";"), L::empty()),
            ]),
        ),
        layout_matcher::body_of_function_declaration(&func)
            .and_then(|l| layout_matcher::nth_fused(0, &l)),
    );
    assert_statement(
        false,
        None,
        &format!("function f(){{return {}}}", x80),
        &func,
    );
    assert_statement(
        true,
        None,
        &format!("function f() {{\n  return {};\n}}", x80),
        &func,
    );
}

#[test]
fn return_statement_space() {
    let assert_no_space = |expr: &str| {
        let ret = ast_builder::test_statement_of_string(&format!("return {};", expr));
        assert_statement(false, None, &format!("return{};", expr), &ret);
    };
    assert_no_space(r#""foo""#);
    assert_no_space(r#"{foo:"bar"}"#);
    assert_no_space("[foo]");
    assert_no_space("!foo");
    assert_no_space("+foo");
    assert_no_space("-foo");
    assert_no_space("~foo");

    let ret = ast_builder::test_statement_of_string("return (foo);");
    assert_statement(false, None, "return foo;", &ret);

    let ret = ast_builder::test_statement_of_string("return 123;");
    assert_statement(false, None, "return 123;", &ret);
}

#[test]
fn for_loop() {
    let x80 = "x".repeat(80);
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::for_(
            None,
            E::identifier(None, None, &x80),
            None,
            None,
            S::empty(None),
        ),
    );
    assert_layout(
        L::loc(
            None,
            L::fused(vec![
                L::group(vec![
                    L::atom("for"),
                    L::pretty_space(),
                    L::atom("("),
                    L::indent(L::fused(vec![
                        L::softline(),
                        L::loc(None, L::id(None, &x80)),
                        L::atom(";"),
                        L::pretty_line(),
                        L::atom(";"),
                        L::pretty_line(),
                    ])),
                    L::softline(),
                    L::atom(")"),
                ]),
                L::loc(None, L::atom(";")),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, &format!("for({};;);", x80), &layout);
    assert_output(
        true,
        &format!("for (\n  {};\n  ;\n  \n);", x80), // TODO: remove trailing whitespace
        &layout,
    );

    // should wrap because `for (xxxxx...xxx; true; true) {` does't fit on one line
    let len = 80 - "for (; true; true) {".len() + 1;
    assert_statement_string(
        true,
        None,
        &format!("for (\n  {};\n  true;\n  true\n) {{}}", "x".repeat(len)),
    );
}

#[test]
fn binary_in_in_for_loops() {
    let ast = {
        let (x, y) = (
            E::identifier(None, None, "x"),
            E::identifier(None, None, "y"),
        );
        let init = E::in_(x, y);
        let body = S::empty(None);
        S::for_(None, init, None, None, body)
    };
    assert_statement(false, None, "for((x in y);;);", &ast);

    let ast = {
        let (y, z) = (
            E::identifier(None, None, "y"),
            E::identifier(None, None, "z"),
        );
        let true_ = expressions::true_();
        let in_expr = E::in_(y, z);
        let eq_expr = E::equal(true_, in_expr);
        let init = E::assignment(
            None,
            None,
            patterns::identifier(None, None, "x"),
            None,
            eq_expr,
        );
        let body = S::empty(None);
        S::for_(None, init, None, None, body)
    };
    assert_statement(false, None, "for(x=true==(y in z);;);", &ast);
}

#[test]
fn for_in_space() {
    let ast = ast_builder::test_statement_of_string(r#"for(var x in {"foo": bar}){}"#);
    assert_statement(false, None, r#"for(var x in{"foo":bar}){}"#, &ast);

    let ast = ast_builder::test_statement_of_string("for(var x in bar){}");
    assert_statement(false, None, "for(var x in bar){}", &ast);
}

#[test]
fn for_statement_without_block() {
    assert_statement_string(false, None, "for(;;)x;");
    assert_statement_string(false, None, "{for(;;)x}");
}

#[test]
fn if_statement_with_labeled_consequent() {
    let ast = S::if_(
        None,
        None,
        E::identifier(None, None, "x"),
        S::labeled(
            None,
            I::identifier(None, "y"),
            S::expression(None, None, None, E::identifier(None, None, "z")),
        ),
        Some(S::if_alternate(
            None,
            None,
            S::expression(None, None, None, E::identifier(None, None, "z")),
        )),
    );
    assert_statement(false, None, "if(x)y:z;else z;", &ast);
    assert_statement(true, None, "if (x) y: z; else z;", &ast);
}

#[test]
fn if_statement_without_block() {
    let if_stmt = S::if_(
        None,
        None,
        E::identifier(None, None, "x"),
        S::expression(None, None, None, E::identifier(None, None, "y")),
        None,
    );
    assert_statement(false, None, "if(x)y;", &if_stmt);
    assert_statement(true, None, "if (x) y;", &if_stmt);

    let ast = S::block(
        None,
        vec![
            if_stmt,
            S::expression(None, None, None, E::identifier(None, None, "z")),
        ],
    );
    assert_statement(false, None, "{if(x)y;z}", &ast);
    assert_statement(true, None, "{\n  if (x) y;\n  z;\n}", &ast);
}

#[test]
fn if_statement_with_empty_consequent() {
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::if_(
            None,
            None,
            E::identifier(None, None, "x"),
            S::empty(None),
            None,
        ),
    );
    assert_output(false, "if(x);", &layout);
    assert_output(true, "if (x);", &layout);
}

#[test]
fn if_else_statement_without_block() {
    let if_else_stmt = S::if_(
        None,
        None,
        E::identifier(None, None, "x"),
        S::expression(None, None, None, E::identifier(None, None, "y")),
        Some(S::if_alternate(
            None,
            None,
            S::expression(None, None, None, E::identifier(None, None, "z")),
        )),
    );
    assert_statement(false, None, "if(x)y;else z;", &if_else_stmt);
    assert_statement(true, None, "if (x) y; else z;", &if_else_stmt);

    let ast = S::block(None, vec![if_else_stmt]);
    assert_statement(false, None, "{if(x)y;else z}", &ast);
    assert_statement(true, None, "{\n  if (x) y; else z;\n}", &ast);

    let ast = S::if_(
        None,
        None,
        E::identifier(None, None, "x"),
        S::expression(None, None, None, E::identifier(None, None, "y")),
        Some(S::if_alternate(
            None,
            None,
            S::expression(
                None,
                None,
                None,
                E::increment(true, E::identifier(None, None, "z")),
            ),
        )),
    );
    assert_statement(false, None, "if(x)y;else++z;", &ast);
    assert_statement(true, None, "if (x) y; else ++z;", &ast);
}

#[test]
fn if_statement_without_block_long() {
    let a80 = "A".repeat(80);
    let if_stmt = S::if_(
        None,
        None,
        E::identifier(None, None, &a80),
        S::expression(None, None, None, E::identifier(None, None, "y")),
        None,
    );
    assert_statement(false, None, &format!("if({})y;", a80), &if_stmt);
    assert_statement(true, None, &format!("if (\n  {}\n)\n  y;", a80), &if_stmt);

    let ast = S::block(
        None,
        vec![
            if_stmt,
            S::expression(None, None, None, E::identifier(None, None, "z")),
        ],
    );
    assert_statement(false, None, &format!("{{if({})y;z}}", a80), &ast);
    assert_statement(
        true,
        None,
        &format!("{{\n  if (\n    {}\n  )\n    y;\n  z;\n}}", a80),
        &ast,
    );
}

#[test]
fn if_else_statement_with_empty_consequent() {
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::if_(
            None,
            None,
            E::identifier(None, None, "x"),
            S::empty(None),
            Some(S::if_alternate(
                None,
                None,
                S::expression(None, None, None, E::identifier(None, None, "y")),
            )),
        ),
    );
    assert_output(false, "if(x);else y;", &layout);
    assert_output(true, "if (x); else y;", &layout);
}

#[test]
fn if_else_statement_with_empty_alternate() {
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::if_(
            None,
            None,
            E::identifier(None, None, "x"),
            S::expression(None, None, None, E::identifier(None, None, "y")),
            Some(S::if_alternate(None, None, S::empty(None))),
        ),
    );
    assert_output(false, "if(x)y;else;", &layout);
    assert_output(true, "if (x) y; else ;", &layout);
}
// TODO: remove extra space

#[test]
fn if_else_statement_with_empty_consequent_and_alternate() {
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::if_(
            None,
            None,
            E::identifier(None, None, "x"),
            S::empty(None),
            Some(S::if_alternate(None, None, S::empty(None))),
        ),
    );
    assert_output(false, "if(x);else;", &layout);
    assert_output(true, "if (x); else ;", &layout);
}
// TODO: remove extra space

#[test]
fn while_statement_without_block() {
    let while_stmt = S::while_(
        E::identifier(None, None, "x"),
        None,
        S::expression(None, None, None, E::identifier(None, None, "y")),
    );
    assert_statement(false, None, "while(x)y;", &while_stmt);

    let ast = S::block(None, vec![while_stmt]);
    assert_statement(false, None, "{while(x)y}", &ast);

    let ast = S::while_(E::identifier(None, None, "x"), None, S::empty(None));
    assert_statement(false, None, "while(x);", &ast);
    assert_statement(true, None, "while (x);", &ast);
}

#[test]
fn do_while_statements() {
    let ast = S::do_while(
        S::labeled(
            None,
            I::identifier(None, "x"),
            S::expression(None, None, None, E::identifier(None, None, "z")),
        ),
        None,
        E::identifier(None, None, "y"),
    );
    assert_statement(false, None, "do x:z;while(y);", &ast);
    assert_statement(true, None, "do x: z; while (y);", &ast);

    let ast = S::do_while(
        S::expression(
            None,
            None,
            None,
            E::increment(true, E::identifier(None, None, "x")),
        ),
        None,
        E::identifier(None, None, "y"),
    );
    assert_statement(false, None, "do++x;while(y);", &ast);
}

#[test]
fn labeled_empty_statement() {
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::labeled(None, I::identifier(None, "x"), S::empty(None)),
    );
    assert_output(false, "x:;", &layout);
    assert_output(true, "x: ;", &layout);
}

#[test]
fn array_expressions() {
    assert_expression_string(false, None, None, "[]");
    assert_expression_string(false, None, None, "[a]");
    assert_expression_string(false, None, None, "[a,b]");
    assert_expression_string(false, None, None, "[a,,b]");
    assert_expression_string(false, None, None, "[a,b,,]");
    assert_expression_string(true, None, None, "[a]");
    assert_expression_string(true, None, None, "[a, b]");
    assert_expression_string(true, None, None, "[a, b, ,]");
    assert_expression_string(
        true,
        None,
        None,
        &format!("[\n  a,\n  {},\n  ,\n]", "b".repeat(80)),
    );
}

#[test]
fn array_with_trailing_hole() {
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::array(
            None,
            None,
            vec![
                E::array_expression(E::identifier(None, None, "a")),
                E::array_hole(None),
            ],
        ),
    );
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::atom("["),
                L::indent(L::fused(vec![
                    L::softline(),
                    L::loc(None, L::id(None, "a")),
                    L::atom(","),
                    L::pretty_line(),
                    L::atom(","),
                ])),
                L::softline(),
                L::atom("]"),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "[a,,]", &layout);
    assert_output(true, "[a, ,]", &layout);

    let a80 = "a".repeat(80);
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::array(
            None,
            None,
            vec![
                E::array_expression(E::identifier(None, None, &a80)),
                E::array_hole(None),
            ],
        ),
    );
    assert_output(false, &format!("[{},,]", a80), &layout);
    assert_output(true, &format!("[\n  {},\n  ,\n]", a80), &layout);
}

#[test]
fn function_statements() {
    assert_statement_string(false, None, "function a(){}");
    assert_statement_string(false, None, "async function a(){}");
    assert_statement_string(false, None, "function* a(){}");
    assert_statement_string(false, None, "function a(a){}");
    assert_statement_string(false, None, "function a(a,b){}");
    assert_statement_string(false, None, "function a(a:b){}");
    assert_statement_string(false, None, "function a(a:b,c:d){}");
    assert_statement_string(false, None, "function a(a:?b=b){}");
    assert_statement_string(false, None, "function a():a{}");
    assert_statement_string(false, None, "function a<b>(){}");
    assert_statement_string(false, None, "function a():%checks{}");
    assert_statement_string(false, None, "function a():a%checks{}");
    assert_statement_string(false, None, "function a():a%checks(a){}");
    assert_statement_string(true, None, "function a(): a %checks(a) {}");
    assert_statement_string(true, None, "function a(a: a, b: b): a {}");
    assert_statement_string(
        true,
        None,
        &format!("function a(\n  a: a,\n  b: {},\n): a {{}}", "b".repeat(80)),
    );
    assert_statement_string(true, None, "function a() {\n  a;\n}");
}

#[test]
fn function_expressions() {
    assert_expression_string(false, None, None, "function(){}");
    assert_expression_string(false, None, None, "function a(){}");
    assert_expression_string(false, None, None, "async function(){}");
    assert_expression_string(false, None, None, "function*(){}");
    assert_expression_string(false, None, None, "function(a){}");
    assert_expression_string(false, None, None, "function(a,b){}");
    assert_expression_string(false, None, None, "function(a:a,b:b):c{}");
    assert_expression_string(false, None, None, "function<a>(){}");
    assert_expression_string(true, None, None, "function(a: a, b: b): c {}");
    assert_expression_string(false, None, None, "()=>a");
    assert_expression_string(false, None, None, "()=>{}");
    assert_expression_string(false, None, None, "():* =>{}");
    assert_expression_string(false, None, None, "async()=>{}");
    assert_expression_string(false, None, None, "a=>{}");
    assert_expression_string(false, None, None, "async a=>{}");
    assert_expression_string(false, None, None, "<a>(a)=>{}");
    assert_expression_string(false, None, None, "(a,b)=>{}");
    assert_expression_string(false, None, None, "(a):%checks=>{}");
    assert_expression_string(false, None, None, "({a})=>a");
    assert_expression_string(false, None, None, "({a})=>({a:b})");
    assert_expression_string(false, None, None, "({a})=>[]");
    assert_expression_string(false, None, None, "({a})=>i++");
    assert_expression_string(false, None, None, "({a})=>a()");
    assert_expression_string(false, None, None, "(a:b)=>{}");
    assert_expression_string(false, None, None, "(a?:b)=>{}");
    assert_expression_string(false, None, None, "(a):b=>{}");
    assert_expression_string(false, None, None, "():c=>{}");
    assert_expression_string(false, None, None, "(a):c=>{}");
    assert_expression_string(false, None, None, "(a:a,b:b):c=>{}");
    assert_expression_string(true, None, None, "(a: a, b: b): c => {}");
}

#[test]
fn function_parameters() {
    let long_a = "a".repeat(80);
    let long_b = "b".repeat(80);
    assert_expression_string(
        true,
        None,
        None,
        &format!("function f(\n  {},\n  {},\n) {{}}", long_a, long_b),
    );
    assert_expression_string(
        true,
        None,
        None,
        &format!("function f(\n  {},\n  ...{}\n) {{}}", long_a, long_b),
    );
    assert_expression_string(
        true,
        None,
        None,
        &format!("(\n  {},\n  {},\n) => {{}}", long_a, long_b),
    );
    assert_expression_string(
        true,
        None,
        None,
        &format!("(\n  {},\n  ...{}\n) => {{}}", long_a, long_b),
    );
}

#[test]
fn class_statements() {
    let long_a = "a".repeat(80);
    let long_b = "b".repeat(80);
    assert_statement_string(false, None, "class a{}");
    assert_statement_string(false, None, "class a extends b{}");
    assert_statement_string(false, None, "class a<a> extends b{}");
    assert_statement_string(false, None, "class a extends b<b>{}");
    assert_statement_string(true, None, &format!("class {} {{}}", long_a));
    assert_statement_string(true, None, &format!("class a\n  extends {} {{}}", long_b));
    assert_statement_string(false, None, "@a class a extends b{}");
    assert_statement_string(false, None, "@a@b class a extends b{}");
    assert_statement_string(false, None, "@a()@b class a extends b{}");
    assert_statement_string(false, None, "@(++a)@b class a extends b{}");
    assert_statement_string(false, None, "@(a&&b)@b class a extends b{}");
    assert_statement_string(false, None, "@(()=>{})@b class a extends b{}");
    assert_statement_string(true, None, "@a\nclass a extends b {}");
    assert_statement_string(true, None, "@a\n@b\nclass a extends b {}");
    assert_statement_string(false, None, "class a implements b{}");
    assert_statement_string(false, None, "class a implements b<b>{}");
    assert_statement_string(false, None, "class a implements b,c{}");
    assert_statement_string(false, None, "class a implements b<b>,c<c>{}");
    assert_statement_string(false, None, "class a implements ns.IFace{}");
    assert_statement_string(true, None, "class a implements ns.IFace {}");
    assert_statement_string(false, None, "class a implements ns.IFace<T>{}");

    {
        let ast = S::class_declaration(
            Some(E::identifier(None, None, "b")),
            Some(vec![classes::implements(None, I::identifier(None, "c"))]),
            Some(I::identifier(None, "a")),
            vec![],
        );
        let layout = js_layout_generator::statement(&opts(), false, &ast);
        assert_layout(
            L::loc(
                None,
                L::group(vec![L::loc(
                    None,
                    L::group(vec![
                        L::atom("class"),
                        L::space(),
                        L::id(None, "a"),
                        L::indent(L::fused(vec![
                            L::line(),
                            L::loc(
                                None,
                                L::fused(vec![
                                    L::atom("extends"),
                                    L::space(),
                                    L::loc(None, L::loc(None, L::id(None, "b"))),
                                ]),
                            ),
                            L::line(),
                            L::loc(
                                None,
                                L::fused(vec![
                                    L::atom("implements"),
                                    L::space(),
                                    L::loc(None, L::id(None, "c")),
                                ]),
                            ),
                        ])),
                        L::pretty_space(),
                        L::loc(None, L::atom("{}")),
                    ]),
                )]),
            ),
            layout.clone(),
        );
        assert_output(false, "class a extends b implements c{}", &layout);
        assert_output(true, "class a extends b implements c {}", &layout);
    }

    {
        let x35 = "x".repeat(35);
        let y29 = "y".repeat(29);
        let c2 = S::class_declaration(
            Some(E::identifier(None, None, &y29)),
            None,
            Some(I::identifier(None, &x35)),
            vec![],
        );
        let ast = S::block(None, vec![c2]);
        let layout = js_layout_generator::statement(&opts(), false, &ast);
        assert_layout(
            L::loc(
                None,
                L::loc(
                    None,
                    L::group(vec![
                        L::atom("{"),
                        L::indent(L::fused(vec![
                            L::pretty_hardline(),
                            L::loc(
                                None,
                                L::group(vec![L::loc(
                                    None,
                                    L::group(vec![
                                        L::atom("class"),
                                        L::space(),
                                        L::id(None, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                        L::indent(L::fused(vec![
                                            L::line(),
                                            L::loc(
                                                None,
                                                L::fused(vec![
                                                    L::atom("extends"),
                                                    L::space(),
                                                    L::loc(
                                                        None,
                                                        L::loc(
                                                            None,
                                                            L::id(
                                                                None,
                                                                "yyyyyyyyyyyyyyyyyyyyyyyyyyyyy",
                                                            ),
                                                        ),
                                                    ),
                                                ]),
                                            ),
                                        ])),
                                        L::pretty_space(),
                                        L::loc(None, L::atom("{}")),
                                    ]),
                                )]),
                            ),
                        ])),
                        L::pretty_hardline(),
                        L::atom("}"),
                    ]),
                ),
            ),
            layout.clone(),
        );
        assert_output(
            false,
            &format!("{{class {} extends {}{{}}}}", x35, y29),
            &layout,
        );
        assert_output(
            true,
            &format!("{{\n  class {}\n    extends {} {{}}\n}}", x35, y29),
            &layout,
        );
    }

    assert_statement_string(
        true,
        None,
        &format!("class a\n  extends {}\n  implements c {{}}", long_b),
    );

    assert_statement_string(
        true,
        None,
        &format!(
            "class a\n  extends {}\n  implements {} {{}}",
            long_b, long_b
        ),
    );

    // TODO: this seems wrong, `c {` should break onto a new line
    assert_statement_string(
        true,
        None,
        &format!(
            "class a\n  extends {}\n  implements {}, c {{}}",
            long_b, long_b
        ),
    );
}
