/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::ast::expression;
use flow_parser_utils::ast_builder::expressions as E;

use crate::layout_generator_test_utils::*;
use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::*;

fn x() -> expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc> {
    E::identifier(None, None, "x")
}
fn y() -> expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc> {
    E::identifier(None, None, "y")
}
fn z() -> expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc> {
    E::identifier(None, None, "z")
}

fn x40() -> expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc> {
    E::identifier(None, None, &"x".repeat(40))
}

fn str_() -> expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc> {
    E::literals::string(None, None, "a")
}

fn and(
    a: expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc>,
    b: expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc>,
) -> expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc> {
    E::logical_and(a, b)
}

fn or(
    a: expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc>,
    b: expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc>,
) -> expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc> {
    E::logical_or(a, b)
}

fn plus(
    a: expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc>,
    b: expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc>,
) -> expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc> {
    E::binary(None, None, expression::BinaryOperator::Plus, a, b)
}

fn minus(
    a: expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc>,
    b: expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc>,
) -> expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc> {
    E::binary(None, None, expression::BinaryOperator::Minus, a, b)
}

#[test]
fn and_with_and_lhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &and(and(x(), y()), z()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &and(x(), y())),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &z()),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x&&y&&z", &layout);
    assert_output(true, "x && y && z", &layout);

    let layout =
        crate::js_layout_generator::expression(&opts, None, &and(and(x40(), x40()), x40()));
    assert_output(
        false,
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
        &layout,
    );
    assert_output(
        true,
        &("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n".to_string()
            + "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n"
            + "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
        &layout,
    );
}

#[test]
fn and_with_and_rhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &and(x(), and(y(), z())));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &x()),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::wrap_in_parens(L::expression(None, None, &and(y(), z()))),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x&&(y&&z)", &layout);
    assert_output(true, "x && (y && z)", &layout);

    let layout =
        crate::js_layout_generator::expression(&opts, None, &and(x40(), and(x40(), x40())));
    assert_output(
        false,
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)",
        &layout,
    );
    assert_output(
        true,
        &("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n".to_string()
            + "  (xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n"
            + "    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)"),
        &layout,
    );
}

#[test]
fn or_with_and_lhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &or(and(x(), y()), z()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &and(x(), y())),
                L::pretty_space(),
                L::atom("||"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &z()),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x&&y||z", &layout);
    assert_output(true, "x && y || z", &layout);

    let layout = crate::js_layout_generator::expression(&opts, None, &or(and(x40(), x40()), x40()));
    assert_output(
        false,
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
        &layout,
    );
    assert_output(
        true,
        &("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n".to_string()
            + "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ||\n"
            + "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
        &layout,
    );
}

#[test]
fn and_with_or_rhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &and(x(), or(y(), z())));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &x()),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::wrap_in_parens(L::expression(None, None, &or(y(), z()))),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x&&(y||z)", &layout);
    assert_output(true, "x && (y || z)", &layout);

    let layout = crate::js_layout_generator::expression(&opts, None, &and(x40(), or(x40(), x40())));
    assert_output(
        false,
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&&(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)",
        &layout,
    );
    assert_output(
        true,
        &("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx &&\n".to_string()
            + "  (xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ||\n"
            + "    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)"),
        &layout,
    );
}

#[test]
fn or_with_or_lhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &or(or(x(), y()), z()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &or(x(), y())),
                L::pretty_space(),
                L::atom("||"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &z()),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x||y||z", &layout);
    assert_output(true, "x || y || z", &layout);

    let layout = crate::js_layout_generator::expression(&opts, None, &or(or(x40(), x40()), x40()));
    assert_output(
        false,
        "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
        &layout,
    );
    assert_output(
        true,
        &("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ||\n".to_string()
            + "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ||\n"
            + "  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
        &layout,
    );
}

#[test]
fn or_with_or_rhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &or(x(), or(y(), z())));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &x()),
                L::pretty_space(),
                L::atom("||"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::wrap_in_parens(L::expression(None, None, &or(y(), z()))),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x||(y||z)", &layout);
    assert_output(true, "x || (y || z)", &layout);
}

#[test]
fn and_with_or_lhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &and(or(x(), y()), z()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::wrap_in_parens(L::expression(None, None, &or(x(), y()))),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &z()),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "(x||y)&&z", &layout);
    assert_output(true, "(x || y) && z", &layout);
}

#[test]
fn or_with_and_rhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &or(x(), and(y(), z())));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &x()),
                L::pretty_space(),
                L::atom("||"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &and(y(), z())),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x||y&&z", &layout);
    assert_output(true, "x || y && z", &layout);
}

#[test]
fn plus_with_plus_lhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &plus(plus(x(), y()), z()));
    assert_layout(
        L::loc(
            None,
            L::fused(vec![
                L::expression(None, None, &plus(x(), y())),
                L::pretty_space(),
                L::atom("+"),
                L::pretty_space(),
                L::expression(None, None, &z()),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x+y+z", &layout);
    assert_output(true, "x + y + z", &layout);
}

#[test]
fn plus_with_plus_rhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &plus(x(), plus(y(), z())));
    assert_layout(
        L::loc(
            None,
            L::fused(vec![
                L::expression(None, None, &x()),
                L::pretty_space(),
                L::atom("+"),
                L::pretty_space(),
                L::wrap_in_parens(L::expression(None, None, &plus(y(), z()))),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x+(y+z)", &layout);
    assert_output(true, "x + (y + z)", &layout);
}

#[test]
fn minus_with_plus_lhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &minus(plus(x(), y()), z()));
    assert_layout(
        L::loc(
            None,
            L::fused(vec![
                L::expression(None, None, &plus(x(), y())),
                L::pretty_space(),
                L::atom("-"),
                L::pretty_space(),
                L::expression(None, None, &z()),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x+y-z", &layout);
    assert_output(true, "x + y - z", &layout);
}

#[test]
fn plus_with_minus_rhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &plus(x(), minus(y(), z())));
    assert_layout(
        L::loc(
            None,
            L::fused(vec![
                L::expression(None, None, &x()),
                L::pretty_space(),
                L::atom("+"),
                L::pretty_space(),
                L::wrap_in_parens(L::expression(None, None, &minus(y(), z()))),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x+(y-z)", &layout);
    assert_output(true, "x + (y - z)", &layout);
}

#[test]
fn and_with_plus_lhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &and(plus(x(), y()), z()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &plus(x(), y())),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &z()),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x+y&&z", &layout);
    assert_output(true, "x + y && z", &layout);
}

#[test]
fn plus_with_and_rhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &plus(x(), and(y(), z())));
    assert_layout(
        L::loc(
            None,
            L::fused(vec![
                L::expression(None, None, &x()),
                L::pretty_space(),
                L::atom("+"),
                L::pretty_space(),
                L::wrap_in_parens(L::expression(None, None, &and(y(), z()))),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x+(y&&z)", &layout);
    assert_output(true, "x + (y && z)", &layout);
}

#[test]
fn plus_with_and_lhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &plus(and(x(), y()), z()));
    assert_layout(
        L::loc(
            None,
            L::fused(vec![
                L::wrap_in_parens(L::expression(None, None, &and(x(), y()))),
                L::pretty_space(),
                L::atom("+"),
                L::pretty_space(),
                L::expression(None, None, &z()),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "(x&&y)+z", &layout);
    assert_output(true, "(x && y) + z", &layout);
}

#[test]
fn and_with_plus_rhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &and(x(), plus(y(), z())));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &x()),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &plus(y(), z())),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x&&y+z", &layout);
    assert_output(true, "x && y + z", &layout);
}

#[test]
fn and_literal_lhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &and(str_(), x()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &str_()),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &x()),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "\"a\"&&x", &layout);
    assert_output(true, "\"a\" && x", &layout);
}

#[test]
fn and_literal_rhs() {
    let opts = crate::js_layout_generator::default_opts();

    let layout = crate::js_layout_generator::expression(&opts, None, &and(x(), str_()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &x()),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &str_()),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x&&\"a\"", &layout);
    assert_output(true, "x && \"a\"", &layout);
}

#[test]
fn function_() {
    let opts = crate::js_layout_generator::default_opts();

    let fn_ = E::function_(None, None, None, None, None, None);
    let layout = crate::js_layout_generator::expression(&opts, None, &and(fn_.dupe(), x()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &fn_),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &x()),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "function(){}&&x", &layout);
    assert_output(true, "function() {} && x", &layout);

    let layout = crate::js_layout_generator::expression(&opts, None, &and(x(), fn_.dupe()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &x()),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &fn_),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x&&function(){}", &layout);
    assert_output(true, "x && function() {}", &layout);
}

#[test]
fn sequence() {
    let opts = crate::js_layout_generator::default_opts();

    let seq = E::sequence(None, None, vec![x(), y()]);
    let layout = crate::js_layout_generator::expression(&opts, None, &and(seq.dupe(), z()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::wrap_in_parens(L::expression(None, None, &seq)),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::expression(None, None, &z()),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "(x,y)&&z", &layout);
    assert_output(true, "(x, y) && z", &layout);

    let layout = crate::js_layout_generator::expression(&opts, None, &and(z(), seq.dupe()));
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::expression(None, None, &z()),
                L::pretty_space(),
                L::atom("&&"),
                L::indent(L::fused(vec![
                    L::pretty_line(),
                    L::wrap_in_parens(L::expression(None, None, &seq)),
                ])),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "z&&(x,y)", &layout);
    assert_output(true, "z && (x, y)", &layout);

    let layout = crate::js_layout_generator::expression(
        &opts,
        None,
        &E::sequence(None, None, vec![z(), seq.dupe()]),
    );
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::loc(None, L::id(None, "z")),
                L::atom(","),
                L::pretty_line(),
                L::wrap_in_parens(L::expression(None, None, &seq)),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "z,(x,y)", &layout);
    assert_output(true, "z, (x, y)", &layout);

    let layout = crate::js_layout_generator::expression(
        &opts,
        None,
        &E::sequence(None, None, vec![seq.dupe(), z()]),
    );
    assert_layout(
        L::loc(
            None,
            L::group(vec![
                L::wrap_in_parens(L::expression(None, None, &seq)),
                L::atom(","),
                L::pretty_line(),
                L::loc(None, L::id(None, "z")),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "(x,y),z", &layout);
    assert_output(true, "(x, y), z", &layout);
}
