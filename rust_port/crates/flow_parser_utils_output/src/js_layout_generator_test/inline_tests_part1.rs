/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser_utils::ast_builder::expressions as E;
use flow_parser_utils::ast_builder::functions;
use flow_parser_utils::ast_builder::patterns;
use flow_parser_utils::ast_builder::statements as S;

use crate::js_layout_generator;
use crate::layout::LayoutNode;
use crate::layout_generator_test_utils::*;
use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::*;

#[test]
fn unary_plus_binary() {
    let x = E::identifier(None, None, "x");
    let y = E::identifier(None, None, "y");
    let plus_y = E::unary_plus(y.clone());
    let minus_y = E::unary_minus(y.clone());
    let ast = E::plus(x.clone(), plus_y.clone());
    assert_expression(false, None, None, "x+ +y", &ast);

    let ast = E::plus(plus_y.clone(), x.clone());
    assert_expression(false, None, None, "+y+x", &ast);

    let ast = E::minus(x.clone(), minus_y.clone());
    assert_expression(false, None, None, "x- -y", &ast);

    let ast = E::plus(x.clone(), minus_y.clone());
    assert_expression(false, None, None, "x+-y", &ast);

    let ast = E::minus(x.clone(), plus_y.clone());
    assert_expression(false, None, None, "x-+y", &ast);

    let ast = E::plus(
        x.clone(),
        E::conditional(None, None, plus_y.clone(), y.clone(), y.clone()),
    );
    assert_expression(false, None, None, "x+(+y?y:y)", &ast);

    let ast = E::plus(x.clone(), E::plus(plus_y.clone(), y.clone()));
    assert_expression(false, None, None, "x+(+y+y)", &ast);

    // `*` is higher precedence than `+`, so would not normally need parens if
    // not for the `+y`
    let ast = E::plus(x.clone(), E::mult(plus_y.clone(), y.clone()));
    assert_expression(false, None, None, "x+(+y)*y", &ast);

    // parens are necessary around the inner `+y+y`, but would be reundant
    // around the multiplication. that is, we don't need `x+((+y+y)*y)`.
    let ast = E::plus(
        x.clone(),
        E::mult(E::plus(plus_y.clone(), y.clone()), y.clone()),
    );
    assert_expression(false, None, None, "x+(+y+y)*y", &ast);
}

#[test]
fn update_plus_binary() {
    let x = E::identifier(None, None, "x");
    let y = E::identifier(None, None, "y");
    let x_incr = E::increment(false, x.clone());
    let x_decr = E::decrement(false, x.clone());
    let incr_y = E::increment(true, y.clone());
    let decr_y = E::decrement(true, y.clone());
    {
        let ast = E::plus(x.clone(), incr_y.clone());
        let layout = js_layout_generator::expression(&opts(), None, &ast);
        assert_layout(
            L::loc(
                None,
                L::fused(vec![
                    L::loc(None, L::id(None, "x")),
                    L::pretty_space(),
                    L::atom("+"),
                    L::pretty_space(),
                    L::ugly_space(),
                    L::loc(
                        None,
                        L::fused(vec![L::atom("++"), L::loc(None, L::id(None, "y"))]),
                    ),
                ]),
            ),
            layout.clone(),
        );
        assert_output(false, "x+ ++y", &layout);
        assert_output(true, "x + ++y", &layout);
    }

    let ast = E::minus(x.clone(), incr_y.clone());
    assert_expression(false, None, None, "x-++y", &ast);

    let ast = E::minus(x.clone(), decr_y.clone());
    assert_expression(false, None, None, "x- --y", &ast);

    let ast = E::plus(x.clone(), decr_y.clone());
    assert_expression(false, None, None, "x+--y", &ast);

    let ast = E::plus(x_incr.clone(), y.clone());
    assert_expression(false, None, None, "x+++y", &ast);

    let ast = E::minus(x_decr.clone(), y.clone());
    assert_expression(false, None, None, "x---y", &ast);

    let ast = E::plus(x_incr.clone(), incr_y.clone());
    assert_expression(false, None, None, "x+++ ++y", &ast);

    let ast = E::minus(x_decr.clone(), decr_y.clone());
    assert_expression(false, None, None, "x--- --y", &ast);
}

#[test]
fn do_while_semicolon() {
    // do { x } while (y)
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::do_while(
            S::block(
                None,
                vec![S::expression(
                    None,
                    None,
                    None,
                    E::identifier(None, None, "x"),
                )],
            ),
            None,
            E::identifier(None, None, "y"),
        ),
    );
    assert_output(false, "do{x}while(y);", &layout);
    assert_output(true, "do {\n  x;\n} while (y);", &layout);
}

#[test]
fn do_while_long() {
    // do { xxxx... } while (yyyy...)
    let x80 = "x".repeat(80);
    let y80 = "y".repeat(80);
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::do_while(
            S::block(
                None,
                vec![S::expression(
                    None,
                    None,
                    None,
                    E::identifier(None, None, &x80),
                )],
            ),
            None,
            E::identifier(None, None, &y80),
        ),
    );
    assert_output(false, &format!("do{{{}}}while({});", x80, y80), &layout);
    assert_output(
        true,
        &format!("do {{\n  {};\n}} while (\n  {}\n);", x80, y80),
        &layout,
    );
}

#[test]
fn do_while_single_statement() {
    // do x; while (y)
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::do_while(
            S::expression(None, None, None, E::identifier(None, None, "x")),
            None,
            E::identifier(None, None, "y"),
        ),
    );
    assert_output(false, "do x;while(y);", &layout);
    assert_output(true, "do x; while (y);", &layout);
}

#[test]
fn do_while_single_statement_long() {
    // do xxxx...; while (yyyy...)
    let x80 = "x".repeat(80);
    let y80 = "y".repeat(80);
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::do_while(
            S::expression(None, None, None, E::identifier(None, None, &x80)),
            None,
            E::identifier(None, None, &y80),
        ),
    );
    assert_output(false, &format!("do {};while({});", x80, y80), &layout);
    assert_output(
        true,
        &format!("do {}; while (\n  {}\n);", x80, y80),
        &layout,
    );
}

#[test]
fn do_while_empty_statement() {
    // do ; while (y)
    let layout = js_layout_generator::statement(
        &opts(),
        false,
        &S::do_while(S::empty(None), None, E::identifier(None, None, "y")),
    );
    assert_output(false, "do;while(y);", &layout);
    assert_output(true, "do ; while (y);", &layout);
}
// TODO: remove space after do

#[test]
fn conditionals() {
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::conditional(
            None,
            None,
            E::identifier(None, None, "a"),
            E::identifier(None, None, "b"),
            E::identifier(None, None, "c"),
        ),
    );
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::loc(None, L::id(None, "a")),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::atom("?"),
                    L::pretty_space(),
                    L::loc(None, L::id(None, "b")),
                    L::pretty_line(),
                    L::atom(":"),
                    L::pretty_space(),
                    L::loc(None, L::id(None, "c")),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "a?b:c", &layout);
    assert_output(true, "a ? b : c", &layout);

    let a80 = "a".repeat(80);
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::conditional(
            None,
            None,
            E::identifier(None, None, &a80),
            E::identifier(None, None, "b"),
            E::identifier(None, None, "c"),
        ),
    );
    assert_output(false, &format!("{}?b:c", a80), &layout);
    assert_output(true, &format!("{}\n  ? b\n  : c", a80), &layout);

    let b80 = "b".repeat(80);
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::conditional(
            None,
            None,
            E::identifier(None, None, "a"),
            E::identifier(None, None, &b80),
            E::identifier(None, None, "c"),
        ),
    );
    assert_output(false, &format!("a?{}:c", b80), &layout);
    assert_output(true, &format!("a\n  ? {}\n  : c", b80), &layout);
}

#[test]
fn conditional_expression_parens() {
    let a = E::identifier(None, None, "a");
    let b = E::identifier(None, None, "b");
    let c = E::identifier(None, None, "c");
    let d = E::identifier(None, None, "d");
    let e = E::identifier(None, None, "e");

    // a ? b++ : c--
    let update = E::conditional(
        None,
        None,
        a.clone(),
        E::increment(false, b.clone()),
        E::decrement(false, c.clone()),
    );
    assert_expression(false, None, None, "a?b++:c--", &update);

    // a ? +b : -c
    let unary = E::conditional(
        None,
        None,
        a.clone(),
        E::unary_plus(b.clone()),
        E::unary_minus(c.clone()),
    );
    assert_expression(false, None, None, "a?+b:-c", &unary);

    // (a || b) ? c : d
    let logical_test = E::conditional(
        None,
        None,
        E::logical_or(a.clone(), b.clone()),
        c.clone(),
        d.clone(),
    );
    assert_expression(false, None, None, "a||b?c:d", &logical_test);

    // (a ? b : c) ? d : e
    let nested_in_test = E::conditional(
        None,
        None,
        E::conditional(None, None, a.clone(), b.clone(), c.clone()),
        d.clone(),
        e.clone(),
    );
    assert_expression(false, None, None, "(a?b:c)?d:e", &nested_in_test);

    // a ? (b ? c : d) : e
    let nested_in_consequent = E::conditional(
        None,
        None,
        a.clone(),
        E::conditional(None, None, b.clone(), c.clone(), d.clone()),
        e.clone(),
    );
    assert_expression(false, None, None, "a?b?c:d:e", &nested_in_consequent);

    // a ? b : (c ? d : e)
    let nested_in_alternate = E::conditional(
        None,
        None,
        a.clone(),
        b.clone(),
        E::conditional(None, None, c.clone(), d.clone(), e.clone()),
    );
    assert_expression(false, None, None, "a?b:c?d:e", &nested_in_alternate);

    let assignment = E::conditional(
        None,
        None,
        a.clone(),
        E::assignment(
            None,
            None,
            patterns::identifier(None, None, "x"),
            None,
            b.clone(),
        ),
        E::assignment(
            None,
            None,
            patterns::identifier(None, None, "y"),
            None,
            c.clone(),
        ),
    );
    assert_expression(false, None, None, "a?x=b:y=c", &assignment);

    let sequence = E::conditional(
        None,
        None,
        a.clone(),
        E::sequence(None, None, vec![b.clone(), c.clone()]),
        E::sequence(None, None, vec![d.clone(), e.clone()]),
    );
    assert_expression(false, None, None, "a?(b,c):(d,e)", &sequence);
}

#[test]
fn call_expression_parens() {
    let x = E::identifier(None, None, "x");
    // `(x++)()`
    let update = E::call(None, None, E::increment(false, x.clone()));
    assert_expression(false, None, None, "(x++)()", &update);

    // `x.y()`
    let member = E::call(
        None,
        None,
        E::member(None, E::members::identifier_by_name(None, "y", x.clone())),
    );
    assert_expression(false, None, None, "x.y()", &member);

    // `x.y.z()`
    let two_members = E::call(
        None,
        None,
        E::member(
            None,
            E::members::identifier_by_name(
                None,
                "z",
                E::member(None, E::members::identifier_by_name(None, "y", x.clone())),
            ),
        ),
    );
    assert_expression(false, None, None, "x.y.z()", &two_members);

    // `x()()`
    let call = E::call(None, None, E::call(None, None, x.clone()));
    assert_expression(false, None, None, "x()()", &call);

    // `new x()()`
    let new_ = E::call(
        None,
        None,
        E::new_(
            None,
            None,
            None,
            Some(E::arg_list(None, None, vec![])),
            x.clone(),
        ),
    );
    assert_expression(false, None, None, "new x()()", &new_);

    // `(new x)()`
    let new_ = E::call(None, None, E::new_(None, None, None, None, x.clone()));
    assert_expression(false, None, None, "(new x)()", &new_);

    // `function() {}()`
    let func = E::call(None, None, E::function_(None, None, None, None, None, None));
    assert_expression(false, None, None, "function(){}()", &func);

    // `(function() {}.foo)()`
    let func = E::call(
        None,
        None,
        E::member(
            None,
            E::members::identifier_by_name(
                None,
                "foo",
                E::function_(None, None, None, None, None, None),
            ),
        ),
    );
    assert_expression(false, None, None, "function(){}.foo()", &func);

    // `(() => {})()`
    let arrow = E::call(None, None, E::arrow_function(None, None, None, None));
    assert_expression(false, None, None, "(()=>{})()", &arrow);

    // `(foo, bar)()`
    let seq = E::call(
        None,
        None,
        E::sequence(None, None, vec![x.clone(), E::identifier(None, None, "y")]),
    );
    assert_expression(false, None, None, "(x,y)()", &seq);

    // `__d("a", [], (function() {}), 1)`
    let underscore_d = E::call(
        None,
        Some(E::arg_list(
            None,
            None,
            vec![
                E::expression_or_spread(E::literals::string(None, None, "a")),
                E::expression_or_spread(E::literals::string(None, None, "b")),
                E::expression_or_spread(E::sequence(
                    None,
                    None,
                    vec![E::function_(None, None, None, None, None, None)],
                )),
                E::expression_or_spread(E::literals::number(None, None, 1.0, "1")),
            ],
        )),
        E::identifier(None, None, "__d"),
    );
    assert_expression(
        false,
        None,
        None,
        r#"__d("a","b",(function(){}),1)"#,
        &underscore_d,
    );
}

#[test]
fn member_expression_parens() {
    let x = E::identifier(None, None, "x");
    // `(x++).y`
    let update = E::member(
        None,
        E::members::identifier_by_name(None, "y", E::increment(false, x.clone())),
    );
    assert_expression(false, None, None, "(x++).y", &update);

    // `x.y.z`
    let member = E::member(
        None,
        E::members::identifier_by_name(
            None,
            "z",
            E::member(None, E::members::identifier_by_name(None, "y", x.clone())),
        ),
    );
    assert_expression(false, None, None, "x.y.z", &member);

    // x().y
    let call = E::member(
        None,
        E::members::identifier_by_name(None, "y", E::call(None, None, x.clone())),
    );
    assert_expression(false, None, None, "x().y", &call);

    // x()[y]
    let computed = E::member(
        None,
        E::members::expression(
            None,
            E::identifier(None, None, "y"),
            E::call(None, None, x.clone()),
        ),
    );
    assert_expression(false, None, None, "x()[y]", &computed);

    // `(function() {}).x`
    let func = E::member(
        None,
        E::members::identifier_by_name(None, "x", E::function_(None, None, None, None, None, None)),
    );
    assert_expression(false, None, None, "function(){}.x", &func);

    // `(() => {}).x`
    let func = E::member(
        None,
        E::members::identifier_by_name(None, "x", E::arrow_function(None, None, None, None)),
    );
    assert_expression(false, None, None, "(()=>{}).x", &func);

    // `(x, y).z`
    let seq = E::member(
        None,
        E::members::identifier_by_name(
            None,
            "z",
            E::sequence(None, None, vec![x.clone(), E::identifier(None, None, "y")]),
        ),
    );
    assert_expression(false, None, None, "(x,y).z", &seq);

    let num = E::member(
        None,
        E::members::identifier_by_name(None, "z", E::literals::number(None, None, 1.0, "1")),
    );
    assert_expression(false, None, None, "1..z", &num);
    let num = E::member(
        None,
        E::members::identifier_by_name(None, "z", E::literals::number(None, None, 1.1, "1.1")),
    );
    assert_expression(false, None, None, "1.1.z", &num);
    let num = E::member(
        None,
        E::members::identifier_by_name(
            None,
            "z",
            E::literals::number(None, None, 0.0000001, "0.0000001"),
        ),
    );
    assert_expression(false, None, None, "1e-7.z", &num);
}

#[test]
fn new_expression_empty_params() {
    // `new xxxxxxx....()`
    let x80 = "x".repeat(80);
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::new_(
            None,
            None,
            None,
            Some(E::arg_list(None, None, vec![])),
            E::identifier(None, None, &x80),
        ),
    );
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::atom("new"),
                L::space(),
                L::loc(None, L::id(None, &x80)),
                L::loc(None, L::group(vec![L::atom("("), L::atom(")")])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, &format!("new {}()", x80), &layout);
    assert_output(true, &format!("new {}()", x80), &layout);
}

#[test]
fn new_expression_params() {
    // `new Foo(x, y)`
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::new_(
            None,
            None,
            None,
            Some(E::arg_list(
                None,
                None,
                vec![
                    E::expression_or_spread(E::identifier(None, None, "x")),
                    E::expression_or_spread(E::identifier(None, None, "y")),
                ],
            )),
            E::identifier(None, None, "Foo"),
        ),
    );
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::atom("new"),
                L::space(),
                L::loc(None, L::id(None, "Foo")),
                L::loc(
                    None,
                    L::group(vec![
                        L::atom("("),
                        L::indent(L::fused(vec![
                            L::softline(),
                            L::loc(None, L::id(None, "x")),
                            L::atom(","),
                            L::pretty_line(),
                            L::loc(None, L::id(None, "y")),
                            LayoutNode::if_break(L::atom(","), L::empty()),
                        ])),
                        L::softline(),
                        L::atom(")"),
                    ]),
                ),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "new Foo(x,y)", &layout);
    assert_output(true, "new Foo(x, y)", &layout);
}

#[test]
fn new_expression_params_long() {
    // `new Foo(xxxxxxx....)`
    let x80 = "x".repeat(80);
    let layout = js_layout_generator::expression(
        &opts(),
        None,
        &E::new_(
            None,
            None,
            None,
            Some(E::arg_list(
                None,
                None,
                vec![E::expression_or_spread(E::identifier(None, None, &x80))],
            )),
            E::identifier(None, None, "Foo"),
        ),
    );
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::atom("new"),
                L::space(),
                L::loc(None, L::id(None, "Foo")),
                L::loc(
                    None,
                    L::group(vec![
                        L::atom("("),
                        L::indent(L::fused(vec![
                            L::softline(),
                            L::loc(None, L::id(None, &x80)),
                            LayoutNode::if_break(L::atom(","), L::empty()),
                        ])),
                        L::softline(),
                        L::atom(")"),
                    ]),
                ),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, &format!("new Foo({})", x80), &layout);
    assert_output(true, &format!("new Foo(\n  {},\n)", x80), &layout);
}

#[test]
fn new_expression_parens() {
    let x80 = "x".repeat(80);
    let x = E::identifier(None, None, "x");
    let y = E::identifier(None, None, "y");
    let z = E::identifier(None, None, "z");
    let id80 = E::identifier(None, None, &x80);
    {
        // `new (x++)()`
        let layout = js_layout_generator::expression(
            &opts(),
            None,
            &E::new_(
                None,
                None,
                None,
                Some(E::arg_list(None, None, vec![])),
                E::increment(false, x.clone()),
            ),
        );
        assert_layout(
            L::loc(
                None,
                L::group(vec![
                    L::atom("new"),
                    L::pretty_space(),
                    L::wrap_in_parens(L::loc(
                        None,
                        L::fused(vec![L::loc(None, L::id(None, "x")), L::atom("++")]),
                    )),
                    L::loc(None, L::group(vec![L::atom("("), L::atom(")")])),
                ]),
            ),
            layout.clone(),
        );
        assert_output(false, "new(x++)()", &layout);
        assert_output(true, "new (x++)()", &layout);

        let update = E::new_(
            None,
            None,
            None,
            Some(E::arg_list(None, None, vec![])),
            E::increment(false, id80.clone()),
        );
        assert_expression(false, None, None, &format!("new({}++)()", x80), &update);
        assert_expression(true, None, None, &format!("new ({}++)()", x80), &update);
    }

    // `new (x())()`
    let call = E::new_(
        None,
        None,
        None,
        Some(E::arg_list(None, None, vec![])),
        E::call(None, None, x.clone()),
    );
    assert_expression(false, None, None, "new(x())()", &call);

    // `new x.y()`
    let member = E::new_(
        None,
        None,
        None,
        Some(E::arg_list(None, None, vec![])),
        E::member(None, E::members::identifier_by_name(None, "y", x.clone())),
    );
    assert_expression(false, None, None, "new x.y()", &member);

    // `new (x.y())()`
    let member_call = E::new_(
        None,
        None,
        None,
        Some(E::arg_list(None, None, vec![])),
        E::call(
            None,
            None,
            E::member(None, E::members::identifier_by_name(None, "y", x.clone())),
        ),
    );
    assert_expression(false, None, None, "new(x.y())()", &member_call);

    // `new (x().y)()`
    let call_member = E::new_(
        None,
        None,
        None,
        Some(E::arg_list(None, None, vec![])),
        E::member(
            None,
            E::members::identifier_by_name(None, "y", E::call(None, None, x.clone())),
        ),
    );
    assert_expression(false, None, None, "new(x().y)()", &call_member);

    // `new (x ? y : z)()`
    let cond = E::new_(
        None,
        None,
        None,
        Some(E::arg_list(None, None, vec![])),
        E::conditional(None, None, x.clone(), y.clone(), z.clone()),
    );
    assert_expression(false, None, None, "new(x?y:z)()", &cond);
}

#[test]
fn unary_expression_parens() {
    // `+(+x)`
    let plus = E::unary_plus(E::unary_plus(E::identifier(None, None, "x")));
    assert_expression(false, None, None, "+(+x)", &plus);

    // `+-x`
    let minus = E::unary_plus(E::unary_minus(E::identifier(None, None, "x")));
    assert_expression(false, None, None, "+-x", &minus);

    // `+(++x)`
    let prefix_incr = E::unary_plus(E::increment(true, E::identifier(None, None, "x")));
    assert_expression(false, None, None, "+(++x)", &prefix_incr);

    // `+--x`
    let prefix_decr = E::unary_plus(E::decrement(true, E::identifier(None, None, "x")));
    assert_expression(false, None, None, "+--x", &prefix_decr);

    // `+x++`
    let suffix_incr = E::unary_plus(E::increment(false, E::identifier(None, None, "x")));
    assert_expression(false, None, None, "+x++", &suffix_incr);

    // `+x--`
    let suffix_decr = E::unary_plus(E::decrement(false, E::identifier(None, None, "x")));
    assert_expression(false, None, None, "+x--", &suffix_decr);

    // `+x()`
    let call = E::unary_plus(E::call(None, None, E::identifier(None, None, "x")));
    assert_expression(false, None, None, "+x()", &call);

    // `+new x()`
    let new_ = E::unary_plus(E::new_(
        None,
        None,
        None,
        Some(E::arg_list(None, None, vec![])),
        E::identifier(None, None, "x"),
    ));
    assert_expression(false, None, None, "+new x()", &new_);
}

#[test]
fn expression_statement_parens() {
    let obj = S::expression(None, None, None, E::object_(None, None, vec![]));
    assert_statement(false, None, "({});", &obj);

    let func = S::expression(
        None,
        None,
        None,
        E::function_(None, None, None, None, None, None),
    );
    assert_statement(false, None, "(function(){});", &func);

    let arrow = S::expression(None, None, None, E::arrow_function(None, None, None, None));
    assert_statement(false, None, "()=>{};", &arrow);

    let klass = S::expression(None, None, None, E::class_(None, None, None, vec![]));
    assert_statement(false, None, "(class{});", &klass);

    let func_call = S::expression(
        None,
        None,
        None,
        E::call(None, None, E::function_(None, None, None, None, None, None)),
    );
    assert_statement(false, None, "(function(){})();", &func_call);

    let func_member = S::expression(
        None,
        None,
        None,
        E::member(
            None,
            E::members::identifier_by_name(
                None,
                "foo",
                E::function_(None, None, None, None, None, None),
            ),
        ),
    );
    assert_statement(false, None, "(function(){}).foo;", &func_member);

    let class_member = S::expression(
        None,
        None,
        None,
        E::member(
            None,
            E::members::identifier_by_name(None, "foo", E::class_(None, None, None, vec![])),
        ),
    );
    assert_statement(false, None, "(class{}).foo;", &class_member);

    let func_member_call = S::expression(
        None,
        None,
        None,
        E::call(
            None,
            None,
            E::member(
                None,
                E::members::identifier_by_name(
                    None,
                    "foo",
                    E::function_(None, None, None, None, None, None),
                ),
            ),
        ),
    );
    assert_statement(false, None, "(function(){}).foo();", &func_member_call);

    let func_call_member = S::expression(
        None,
        None,
        None,
        E::member(
            None,
            E::members::identifier_by_name(
                None,
                "foo",
                E::call(None, None, E::function_(None, None, None, None, None, None)),
            ),
        ),
    );
    assert_statement(false, None, "(function(){})().foo;", &func_call_member);

    let func_sequence = S::expression(
        None,
        None,
        None,
        E::sequence(
            None,
            None,
            vec![
                E::function_(None, None, None, None, None, None),
                E::identifier(None, None, "x"),
            ],
        ),
    );
    assert_statement(false, None, "(function(){}),x;", &func_sequence);
}

#[test]
fn arrow_body_parens() {
    let x = E::identifier(None, None, "x");
    let y = E::identifier(None, None, "y");
    let z = E::identifier(None, None, "z");
    let arrow = E::arrow_function(
        None,
        None,
        None,
        Some(functions::body_expression(E::sequence(
            None,
            None,
            vec![x.clone(), y.clone()],
        ))),
    );
    assert_expression(false, None, None, "()=>(x,y)", &arrow);

    let arrow = E::arrow_function(
        None,
        None,
        None,
        Some(functions::body_expression(E::conditional(
            None,
            None,
            x.clone(),
            y.clone(),
            z.clone(),
        ))),
    );
    assert_expression(false, None, None, "()=>x?y:z", &arrow);

    let arrow = E::arrow_function(
        None,
        None,
        None,
        Some(functions::body_expression(x.clone())),
    );
    assert_expression(
        false,
        None,
        None,
        "(()=>x)?y:z",
        &E::conditional(None, None, arrow, y.clone(), z.clone()),
    );

    let arrow = {
        let arrow = E::arrow_function(None, None, None, None);
        E::arrow_function(None, None, None, Some(functions::body_expression(arrow)))
    };
    assert_expression(false, None, None, "()=>()=>{}", &arrow);
}

#[test]
fn argument_parens() {
    let f = E::identifier(None, None, "f");
    let x = E::identifier(None, None, "x");
    let y = E::identifier(None, None, "y");
    let z = E::identifier(None, None, "z");
    let call = E::call(None, Some(E::arg_list(None, None, vec![])), f.clone());
    assert_expression(false, None, None, "f()", &call);

    let call = E::call(
        None,
        Some(E::arg_list(
            None,
            None,
            vec![E::expression_or_spread(E::sequence(
                None,
                None,
                vec![x.clone(), y.clone()],
            ))],
        )),
        f.clone(),
    );
    assert_expression(false, None, None, "f((x,y))", &call);

    let call = E::call(
        None,
        Some(E::arg_list(
            None,
            None,
            vec![E::spread(
                None,
                None,
                E::sequence(None, None, vec![x.clone(), y.clone()]),
            )],
        )),
        f.clone(),
    );
    assert_expression(false, None, None, "f(...(x,y))", &call);

    let call = E::call(
        None,
        Some(E::arg_list(
            None,
            None,
            vec![E::expression_or_spread(E::conditional(
                None,
                None,
                x.clone(),
                y.clone(),
                z.clone(),
            ))],
        )),
        f.clone(),
    );
    assert_expression(false, None, None, "f(x?y:z)", &call);

    let call = {
        let arrow = E::arrow_function(None, None, None, None);
        E::call(
            None,
            Some(E::arg_list(
                None,
                None,
                vec![E::expression_or_spread(arrow)],
            )),
            f.clone(),
        )
    };
    assert_expression(false, None, None, "f(()=>{})", &call);

    let call = {
        let seq = E::sequence(None, None, vec![x.clone(), y.clone()]);
        let logical = E::logical_or(seq, z.clone());
        E::call(
            None,
            Some(E::arg_list(
                None,
                None,
                vec![E::expression_or_spread(logical)],
            )),
            f.clone(),
        )
    };
    assert_expression(false, None, None, "f((x,y)||z)", &call);
}
