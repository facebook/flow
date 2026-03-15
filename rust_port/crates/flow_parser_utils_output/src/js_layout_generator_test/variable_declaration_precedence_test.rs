/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser_utils::ast_builder::expressions as E;
use flow_parser_utils::ast_builder::statements as S;
use flow_parser_utils::ast_builder::*;

use crate::layout_test_utils::layout_builder as L;
use crate::layout_test_utils::*;

#[test]
fn test() {
    let seq = E::sequence(
        None,
        None,
        vec![
            E::identifier(None, None, "y"),
            E::identifier(None, None, "z"),
        ],
    );
    let ast = S::variable_declaration(
        None,
        None,
        None,
        vec![S::variable_declarator(None, Some(seq.clone()), None, "x")],
    );
    assert_layout_of_statement(
        None,
        L::loc(
            None,
            L::loc(
                None,
                L::fused(vec![
                    L::atom("var"),
                    L::atom(" "),
                    L::loc(
                        None,
                        L::fused(vec![
                            L::loc(None, L::id(None, "x")),
                            L::pretty_space(),
                            L::atom("="),
                            L::pretty_space(),
                            L::wrap_in_parens(L::expression(None, None, &seq)),
                        ]),
                    ),
                    L::atom(";"),
                ]),
            ),
        ),
        &ast,
    );

    let ast = {
        let init = E::assignment(
            None,
            None,
            patterns::identifier(None, None, "y"),
            None,
            E::identifier(None, None, "z"),
        );
        S::variable_declaration(
            None,
            None,
            None,
            vec![S::variable_declarator(None, Some(init), None, "x")],
        )
    };
    assert_layout_of_statement(
        None,
        L::loc(
            None,
            L::loc(
                None,
                L::fused(vec![
                    L::atom("var"),
                    L::atom(" "),
                    L::loc(
                        None,
                        L::fused(vec![
                            L::loc(None, L::id(None, "x")),
                            L::pretty_space(),
                            L::atom("="),
                            L::pretty_space(),
                            L::loc(
                                None,
                                L::fused(vec![
                                    L::loc(None, L::id(None, "y")),
                                    L::pretty_space(),
                                    L::atom("="),
                                    L::pretty_space(),
                                    L::loc(None, L::id(None, "z")),
                                ]),
                            ),
                        ]),
                    ),
                    L::atom(";"),
                ]),
            ),
        ),
        &ast,
    );

    let fn_ast = E::function_(None, None, None, None, None, None);
    let ast = S::variable_declaration(
        None,
        None,
        None,
        vec![S::variable_declarator(
            None,
            Some(fn_ast.clone()),
            None,
            "x",
        )],
    );
    assert_layout_of_statement(
        None,
        L::loc(
            None,
            L::loc(
                None,
                L::fused(vec![
                    L::atom("var"),
                    L::atom(" "),
                    L::loc(
                        None,
                        L::fused(vec![
                            L::loc(None, L::id(None, "x")),
                            L::pretty_space(),
                            L::atom("="),
                            L::pretty_space(),
                            L::expression(None, None, &fn_ast),
                        ]),
                    ),
                    L::atom(";"),
                ]),
            ),
        ),
        &ast,
    );
}
