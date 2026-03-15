/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser_utils::ast_builder;
use flow_parser_utils::ast_builder::expressions as E;
use flow_parser_utils::ast_builder::statements as S;

use crate::js_layout_generator;
use crate::layout_generator_test_utils::*;
use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::*;

#[test]
fn blank_lines_if_in_original() {
    let ast = ast_builder::test_program_of_string("var x = 1;\n\n\nvar y = 2;");
    let layout =
        js_layout_generator::program(&js_layout_generator::default_opts(), false, None, &ast);
    let loc_1_0_1_10 = Loc {
        start: flow_parser::loc::Position { line: 1, column: 0 },
        end: flow_parser::loc::Position {
            line: 1,
            column: 10,
        },
        ..LOC_NONE
    };
    let loc_1_4_1_9 = Loc {
        start: flow_parser::loc::Position { line: 1, column: 4 },
        end: flow_parser::loc::Position { line: 1, column: 9 },
        ..LOC_NONE
    };
    let loc_1_4_1_5 = Loc {
        start: flow_parser::loc::Position { line: 1, column: 4 },
        end: flow_parser::loc::Position { line: 1, column: 5 },
        ..LOC_NONE
    };
    let loc_1_8_1_9 = Loc {
        start: flow_parser::loc::Position { line: 1, column: 8 },
        end: flow_parser::loc::Position { line: 1, column: 9 },
        ..LOC_NONE
    };
    let loc_4_0_4_10 = Loc {
        start: flow_parser::loc::Position { line: 4, column: 0 },
        end: flow_parser::loc::Position {
            line: 4,
            column: 10,
        },
        ..LOC_NONE
    };
    let loc_4_4_4_9 = Loc {
        start: flow_parser::loc::Position { line: 4, column: 4 },
        end: flow_parser::loc::Position { line: 4, column: 9 },
        ..LOC_NONE
    };
    let loc_4_4_4_5 = Loc {
        start: flow_parser::loc::Position { line: 4, column: 4 },
        end: flow_parser::loc::Position { line: 4, column: 5 },
        ..LOC_NONE
    };
    let loc_4_8_4_9 = Loc {
        start: flow_parser::loc::Position { line: 4, column: 8 },
        end: flow_parser::loc::Position { line: 4, column: 9 },
        ..LOC_NONE
    };
    assert_layout(
        L::program(
            None,
            L::group(vec![
                L::loc(
                    Some(loc_1_0_1_10.dupe()),
                    L::loc(
                        Some(loc_1_0_1_10),
                        L::fused(vec![
                            L::atom("var"),
                            L::space(),
                            L::loc(
                                Some(loc_1_4_1_9),
                                L::fused(vec![
                                    L::loc(Some(loc_1_4_1_5.dupe()), L::id(Some(loc_1_4_1_5), "x")),
                                    L::pretty_space(),
                                    L::atom("="),
                                    L::pretty_space(),
                                    L::loc(Some(loc_1_8_1_9), L::atom("1")),
                                ]),
                            ),
                            L::atom(";"),
                        ]),
                    ),
                ),
                L::pretty_hardline(),
                L::pretty_hardline(),
                L::loc(
                    Some(loc_4_0_4_10.dupe()),
                    L::loc(
                        Some(loc_4_0_4_10),
                        L::fused(vec![
                            L::atom("var"),
                            L::space(),
                            L::loc(
                                Some(loc_4_4_4_9),
                                L::fused(vec![
                                    L::loc(Some(loc_4_4_4_5.dupe()), L::id(Some(loc_4_4_4_5), "y")),
                                    L::pretty_space(),
                                    L::atom("="),
                                    L::pretty_space(),
                                    L::loc(Some(loc_4_8_4_9), L::atom("2")),
                                ]),
                            ),
                            L::atom(";"),
                        ]),
                    ),
                ),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "var x=1;var y=2;", &layout);
    assert_output(true, "var x = 1;\n\nvar y = 2;", &layout);
}

#[test]
fn program_artifact_newline() {
    let ast = ast_builder::mk_program(
        None,
        None,
        None,
        None,
        vec![S::expression(
            None,
            None,
            None,
            E::identifier(None, None, "x"),
        )],
    );
    let layout = js_layout_generator::program(
        &js_layout_generator::default_opts(),
        false,
        Some("@artifact abc123"),
        &ast,
    );
    assert_layout(
        L::program(
            None,
            L::fused(vec![
                L::group(vec![L::loc(
                    None,
                    L::fused(vec![L::loc(None, L::id(None, "x")), L::atom(";")]),
                )]),
                L::hardline(),
                L::atom("/* @artifact abc123 */"),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x;\n/* @artifact abc123 */", &layout);
    assert_output(true, "x;\n/* @artifact abc123 */", &layout);
}

#[test]
fn program_trailing_semicolon() {
    let ast = ast_builder::mk_program(
        None,
        None,
        None,
        None,
        vec![
            S::expression(None, None, None, E::identifier(None, None, "x")),
            S::expression(None, None, None, E::identifier(None, None, "y")),
        ],
    );
    let layout =
        js_layout_generator::program(&js_layout_generator::default_opts(), false, None, &ast);
    assert_layout(
        L::program(
            None,
            L::group(vec![
                L::loc(
                    None,
                    L::fused(vec![L::loc(None, L::id(None, "x")), L::atom(";")]),
                ),
                L::pretty_hardline(),
                L::loc(
                    None,
                    L::fused(vec![L::loc(None, L::id(None, "y")), L::atom(";")]),
                ),
            ]),
        ),
        layout.clone(),
    );
    assert_output(false, "x;y;", &layout);
    assert_output(true, "x;\ny;", &layout);
}

#[test]
fn preserve_docblock() {
    let c_loc = Loc {
        start: flow_parser::loc::Position { line: 1, column: 1 },
        ..LOC_NONE
    };
    let s_loc = Loc {
        start: flow_parser::loc::Position { line: 2, column: 1 },
        ..LOC_NONE
    };
    let all_comments = vec![ast_builder::comments::line(
        Some(c_loc.dupe()),
        None,
        " hello world",
    )];
    let statements = vec![S::expression(
        Some(s_loc.dupe()),
        None,
        None,
        E::identifier(None, None, "x"),
    )];
    let ast = ast_builder::mk_program(None, None, None, Some(all_comments), statements);

    {
        let layout =
            js_layout_generator::program(&js_layout_generator::default_opts(), true, None, &ast);
        assert_layout(
            L::program(
                None,
                L::group(vec![
                    L::loc(
                        Some(c_loc),
                        L::fused(vec![L::atom("//"), L::atom(" hello world"), L::hardline()]),
                    ),
                    L::pretty_hardline(),
                    L::loc(
                        Some(s_loc.dupe()),
                        L::fused(vec![L::loc(None, L::id(None, "x")), L::atom(";")]),
                    ),
                ]),
            ),
            layout.clone(),
        );
        assert_output(false, "// hello world\nx;", &layout);

        // TODO: inserts an extra line between line comments
        assert_output(true, "// hello world\n\nx;", &layout);
    }

    let layout =
        js_layout_generator::program(&js_layout_generator::default_opts(), false, None, &ast);
    assert_layout(
        L::program(
            None,
            L::group(vec![L::loc(
                Some(s_loc),
                L::fused(vec![L::loc(None, L::id(None, "x")), L::atom(";")]),
            )]),
        ),
        layout.clone(),
    );
    assert_output(false, "x;", &layout);
    assert_output(true, "x;", &layout);
}
