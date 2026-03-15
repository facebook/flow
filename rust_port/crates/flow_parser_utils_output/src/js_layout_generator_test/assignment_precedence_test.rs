/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser_utils::ast_builder::expressions as E;
use flow_parser_utils::ast_builder::statements as S;
use flow_parser_utils::ast_builder::*;

use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::*;

#[test]
fn test() {
    let rhs = E::sequence(
        None,
        None,
        vec![
            E::identifier(None, None, "y"),
            E::identifier(None, None, "z"),
        ],
    );
    let ast = E::assignment(
        None,
        None,
        patterns::identifier(None, None, "x"),
        None,
        rhs.clone(),
    );
    assert_layout_of_expression(
        None,
        None,
        L::loc(
            None,
            L::fused(vec![
                L::loc(None, L::id(None, "x")),
                L::pretty_space(),
                L::atom("="),
                L::pretty_space(),
                L::wrap_in_parens(L::expression(None, None, &rhs)),
            ]),
        ),
        &ast,
    );

    let rhs = E::assignment(
        None,
        None,
        patterns::identifier(None, None, "y"),
        None,
        E::identifier(None, None, "z"),
    );
    let ast = E::assignment(
        None,
        None,
        patterns::identifier(None, None, "x"),
        None,
        rhs.clone(),
    );
    assert_layout_of_expression(
        None,
        None,
        L::loc(
            None,
            L::fused(vec![
                L::loc(None, L::id(None, "x")),
                L::pretty_space(),
                L::atom("="),
                L::pretty_space(),
                L::expression(None, None, &rhs),
            ]),
        ),
        &ast,
    );

    let rhs = E::function_(None, None, None, None, None, None);
    let ast = E::assignment(
        None,
        None,
        patterns::identifier(None, None, "x"),
        None,
        rhs.clone(),
    );
    let expected = L::loc(
        None,
        L::fused(vec![
            L::loc(None, L::id(None, "x")),
            L::pretty_space(),
            L::atom("="),
            L::pretty_space(),
            L::expression(None, None, &rhs),
        ]),
    );
    assert_layout_of_expression(None, None, expected.clone(), &ast);
    assert_layout_of_statement(
        None,
        L::loc(None, L::fused(vec![expected, L::atom(";")])),
        &S::expression(None, None, None, ast),
    );

    assert_layout_of_statement_string(
        L::loc(
            Some(Loc {
                source: None,
                start: Position { line: 1, column: 0 },
                end: Position { line: 1, column: 8 },
            }),
            L::fused(vec![
                L::wrap_in_parens(L::loc(
                    Some(Loc {
                        source: None,
                        start: Position { line: 1, column: 1 },
                        end: Position { line: 1, column: 6 },
                    }),
                    L::fused(vec![
                        L::loc(
                            Some(Loc {
                                source: None,
                                start: Position { line: 1, column: 1 },
                                end: Position { line: 1, column: 4 },
                            }),
                            L::group(vec![
                                L::atom("{"),
                                L::indent(L::fused(vec![
                                    L::softline(),
                                    L::loc(
                                        Some(Loc {
                                            source: None,
                                            start: Position { line: 1, column: 2 },
                                            end: Position { line: 1, column: 3 },
                                        }),
                                        L::id(
                                            Some(Loc {
                                                source: None,
                                                start: Position { line: 1, column: 2 },
                                                end: Position { line: 1, column: 3 },
                                            }),
                                            "a",
                                        ),
                                    ),
                                ])),
                                L::softline(),
                                L::atom("}"),
                            ]),
                        ),
                        L::pretty_space(),
                        L::atom("="),
                        L::pretty_space(),
                        L::loc(
                            Some(Loc {
                                source: None,
                                start: Position { line: 1, column: 5 },
                                end: Position { line: 1, column: 6 },
                            }),
                            L::id(
                                Some(Loc {
                                    source: None,
                                    start: Position { line: 1, column: 5 },
                                    end: Position { line: 1, column: 6 },
                                }),
                                "b",
                            ),
                        ),
                    ]),
                )),
                L::atom(";"),
            ]),
        ),
        "({a}=b);",
    );
}
