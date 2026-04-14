/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::loc::Loc;
use flow_parser_utils::ast_builder;

use crate::js_layout_generator;
use crate::layout::LayoutNode;
use crate::source::Source;

pub fn opts() -> js_layout_generator::Opts {
    js_layout_generator::default_opts()
}

pub fn preserve_formatting_opts() -> js_layout_generator::Opts {
    js_layout_generator::Opts {
        preserve_formatting: true,
        ..js_layout_generator::default_opts()
    }
}

pub fn no_bracket_spacing(opts: &js_layout_generator::Opts) -> js_layout_generator::Opts {
    js_layout_generator::Opts {
        bracket_spacing: false,
        ..opts.clone()
    }
}

pub fn assert_output(pretty: bool, expected_str: &str, layout: &LayoutNode) {
    let source: Source = if pretty {
        crate::pretty_printer::print(false, layout)
    } else {
        crate::compact_printer::print(layout)
    };
    let out = source.contents();
    // remove trailing \n
    let out = &out[..out.len() - 1];
    let printer = |x: &str| -> String { x.replace(' ', "\u{2423}").replace('\n', "\u{00AC}\n") };
    assert_eq!(
        printer(expected_str),
        printer(out),
        "expected: {}\nactual: {}",
        expected_str,
        out
    );
}

pub fn assert_expression(
    pretty: bool,
    expr_ctxt: Option<&js_layout_generator::ExpressionContext>,
    opts: Option<&js_layout_generator::Opts>,
    expected_str: &str,
    ast: &flow_parser::ast::expression::Expression<Loc, Loc>,
) {
    let default_opts = js_layout_generator::default_opts();
    let opts = opts.unwrap_or(&default_opts);
    let layout = js_layout_generator::expression(opts, expr_ctxt, ast);
    assert_output(pretty, expected_str, &layout);
}

pub fn assert_expression_string(
    pretty: bool,
    expr_ctxt: Option<&js_layout_generator::ExpressionContext>,
    opts: Option<&js_layout_generator::Opts>,
    s: &str,
) {
    let ast = ast_builder::test_expression_of_string(s);
    assert_expression(pretty, expr_ctxt, opts, s, &ast);
}

pub fn assert_statement(
    pretty: bool,
    opts: Option<&js_layout_generator::Opts>,
    expected_str: &str,
    ast: &flow_parser::ast::statement::Statement<Loc, Loc>,
) {
    let default_opts = js_layout_generator::default_opts();
    let opts = opts.unwrap_or(&default_opts);
    let layout = js_layout_generator::statement(opts, false, ast);
    assert_output(pretty, expected_str, &layout);
}

pub fn assert_statement_string(pretty: bool, opts: Option<&js_layout_generator::Opts>, s: &str) {
    let ast = ast_builder::test_statement_of_string(s);
    let default_opts = js_layout_generator::default_opts();
    let opts = opts.unwrap_or(&default_opts);
    let layout = js_layout_generator::statement(opts, false, &ast);
    assert_output(pretty, s, &layout);
}

pub fn assert_statement_string_with_filename(
    pretty: bool,
    filename: &str,
    opts: Option<&js_layout_generator::Opts>,
    s: &str,
) {
    let ast = ast_builder::test_statement_of_string_with_filename(Some(filename), s);
    let default_opts = js_layout_generator::default_opts();
    let opts = opts.unwrap_or(&default_opts);
    let layout = js_layout_generator::statement(opts, false, &ast);
    assert_output(pretty, s, &layout);
}

pub fn assert_program_string(pretty: bool, s: &str) {
    let ast = ast_builder::test_program_of_string(s);
    let opts = js_layout_generator::default_opts();
    let layout = js_layout_generator::program_simple(&opts, &ast);
    assert_output(pretty, s, &layout);
}
