/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::layout::LayoutNode;
use crate::layout::ListConfig;
use crate::layout::WhenToBreak;
use crate::layout::fuse;
use crate::layout::line;
use crate::layout::list;
use crate::layout::pretty_hardline;
use crate::layout::pretty_space;
use crate::layout::space;

fn flat_pretty_space() -> LayoutNode {
    LayoutNode::if_break(LayoutNode::empty(), pretty_space())
}

fn assert_pretty_print(expected_str: &str, layout: LayoutNode) {
    let out = crate::pretty_printer::print(true, &layout);
    let out = out.contents();
    let out = out.trim();
    let printer = |x: &str| -> String { x.replace(' ', "\u{2423}") };
    assert_eq!(
        printer(expected_str),
        printer(out),
        "expected: {}\nactual: {}",
        expected_str,
        out
    );
}

#[test]
fn test_breaks_in_list() {
    let layout = fuse(vec![
        LayoutNode::atom("return".to_string()),
        space(),
        list(
            None,
            Some((
                LayoutNode::if_break(LayoutNode::atom("(".to_string()), LayoutNode::empty()),
                LayoutNode::if_break(LayoutNode::atom(")".to_string()), LayoutNode::empty()),
            )),
            None,
            true,
            None,
            None,
            vec![LayoutNode::atom("null".to_string())],
        ),
    ]);
    assert_pretty_print("return null", layout);

    let long_string = "x".repeat(80);
    let layout = fuse(vec![
        LayoutNode::atom("return".to_string()),
        space(),
        list(
            None,
            Some((
                LayoutNode::if_break(LayoutNode::atom("(".to_string()), LayoutNode::empty()),
                LayoutNode::if_break(LayoutNode::atom(")".to_string()), LayoutNode::empty()),
            )),
            None,
            true,
            None,
            None,
            vec![LayoutNode::atom(long_string.clone())],
        ),
    ]);
    assert_pretty_print(&format!("return (\n  {}\n)", long_string), layout);
}

#[test]
fn test_force_breaks_in_list() {
    let short_string = "x".repeat(10);
    let layout = fuse(vec![
        LayoutNode::atom("myFunc".to_string()),
        list(
            None,
            Some((
                LayoutNode::atom("(".to_string()),
                LayoutNode::atom(")".to_string()),
            )),
            Some(LayoutNode::atom(",".to_string())),
            true,
            None,
            None,
            vec![
                LayoutNode::atom("a".to_string()),
                fuse(vec![
                    LayoutNode::atom("b".to_string()),
                    space(),
                    LayoutNode::atom("=>".to_string()),
                    LayoutNode::indent(fuse(vec![
                        pretty_hardline(),
                        LayoutNode::atom(short_string.clone()),
                    ])),
                ]),
            ],
        ),
    ]);
    assert_pretty_print(
        &format!("myFunc(\n  a,\n  b =>\n    {},\n)", short_string),
        layout,
    );
}

#[test]
fn test_sequence_inline_after() {
    let short_string = "x".repeat(10);
    let long_string = "x".repeat(80);

    {
        let layout = fuse(vec![
            LayoutNode::atom(short_string.clone()),
            LayoutNode::sequence(
                ListConfig {
                    break_mode: WhenToBreak::BreakIfNeeded,
                    inline: (false, true),
                    indent: 2,
                },
                vec![fuse(vec![
                    flat_pretty_space(),
                    LayoutNode::atom(short_string.clone()),
                ])],
            ),
        ]);
        assert_pretty_print(&format!("{} {}", short_string, short_string), layout);
    }

    {
        let layout = fuse(vec![
            fuse(vec![
                LayoutNode::atom(short_string.clone()),
                LayoutNode::sequence(
                    ListConfig {
                        break_mode: WhenToBreak::BreakIfNeeded,
                        inline: (false, true),
                        indent: 2,
                    },
                    vec![fuse(vec![
                        flat_pretty_space(),
                        LayoutNode::atom(short_string.clone()),
                    ])],
                ),
            ]),
            LayoutNode::sequence(
                ListConfig {
                    break_mode: WhenToBreak::BreakIfNeeded,
                    inline: (false, true),
                    indent: 2,
                },
                vec![fuse(vec![
                    flat_pretty_space(),
                    LayoutNode::atom(short_string.clone()),
                ])],
            ),
        ]);
        assert_pretty_print(
            &format!("{} {} {}", short_string, short_string, short_string),
            layout,
        );
    }

    {
        let layout = fuse(vec![
            LayoutNode::atom(long_string.clone()),
            LayoutNode::sequence(
                ListConfig {
                    break_mode: WhenToBreak::BreakIfNeeded,
                    inline: (false, true),
                    indent: 2,
                },
                vec![fuse(vec![
                    flat_pretty_space(),
                    LayoutNode::atom(long_string.clone()),
                ])],
            ),
        ]);
        assert_pretty_print(&format!("{}\n  {}", long_string, long_string), layout);
    }

    let layout = fuse(vec![
        fuse(vec![
            LayoutNode::atom(long_string.clone()),
            LayoutNode::sequence(
                ListConfig {
                    break_mode: WhenToBreak::BreakIfNeeded,
                    inline: (false, true),
                    indent: 2,
                },
                vec![fuse(vec![
                    flat_pretty_space(),
                    LayoutNode::atom(long_string.clone()),
                ])],
            ),
        ]),
        LayoutNode::sequence(
            ListConfig {
                break_mode: WhenToBreak::BreakIfNeeded,
                inline: (false, true),
                indent: 2,
            },
            vec![fuse(vec![
                flat_pretty_space(),
                LayoutNode::atom(long_string.clone()),
            ])],
        ),
    ]);
    assert_pretty_print(
        &format!("{}\n  {}\n  {}", long_string, long_string, long_string),
        layout,
    );
}

#[test]
fn test_if_break_inside_concat_inside_sequence() {
    let a40 = "A".repeat(40);

    let layout = LayoutNode::sequence(
        ListConfig {
            break_mode: WhenToBreak::BreakIfNeeded,
            inline: (true, true),
            indent: 0,
        },
        vec![LayoutNode::concat(vec![
            LayoutNode::atom(a40.clone()),
            LayoutNode::if_break(LayoutNode::empty(), LayoutNode::atom(" ".to_string())),
            LayoutNode::atom(a40.clone()),
        ])],
    );
    assert_pretty_print(&format!("{}{}", a40, a40), layout);

    let layout = LayoutNode::sequence(
        ListConfig {
            break_mode: WhenToBreak::BreakIfNeeded,
            inline: (true, true),
            indent: 0,
        },
        vec![
            LayoutNode::concat(vec![
                LayoutNode::atom(a40.clone()),
                LayoutNode::if_break(LayoutNode::empty(), LayoutNode::atom(" ".to_string())),
                LayoutNode::atom(a40.clone()),
            ]),
            LayoutNode::atom(a40.clone()),
        ],
    );
    assert_pretty_print(&format!("{}{}\n{}", a40, a40, a40), layout);
}

#[test]
fn test_break_if_needed_sequence_inside_concat() {
    let a80 = "A".repeat(80);

    let layout = LayoutNode::concat(vec![
        LayoutNode::atom("(".to_string()),
        LayoutNode::sequence(
            ListConfig {
                break_mode: WhenToBreak::BreakIfNeeded,
                inline: (false, false),
                indent: 2,
            },
            vec![LayoutNode::atom("a".to_string())],
        ),
        LayoutNode::atom(")".to_string()),
    ]);
    assert_pretty_print("(a)", layout);

    let layout = LayoutNode::concat(vec![
        LayoutNode::atom("(".to_string()),
        LayoutNode::sequence(
            ListConfig {
                break_mode: WhenToBreak::BreakIfNeeded,
                inline: (false, false),
                indent: 2,
            },
            vec![LayoutNode::atom(a80.clone())],
        ),
        LayoutNode::atom(")".to_string()),
    ]);
    assert_pretty_print(&format!("(\n  {}\n)", a80), layout);

    let layout = LayoutNode::concat(vec![
        LayoutNode::atom("(".to_string()),
        LayoutNode::sequence(
            ListConfig {
                break_mode: WhenToBreak::BreakIfNeeded,
                inline: (true, true),
                indent: 2,
            },
            vec![LayoutNode::atom(a80.clone())],
        ),
        LayoutNode::atom(")".to_string()),
    ]);
    assert_pretty_print(&format!("({})", a80), layout);
}

#[test]
fn test_group_break() {
    let a40 = "A".repeat(40);
    let a80 = "A".repeat(80);

    assert_pretty_print(
        &format!("({})", a40),
        LayoutNode::group(vec![
            LayoutNode::atom("(".to_string()),
            LayoutNode::atom(a40.clone()),
            LayoutNode::atom(")".to_string()),
        ]),
    );

    assert_pretty_print(
        &format!("( {} )", a40),
        LayoutNode::group(vec![
            LayoutNode::atom("(".to_string()),
            line(),
            LayoutNode::atom(a40.clone()),
            line(),
            LayoutNode::atom(")".to_string()),
        ]),
    );

    assert_pretty_print(
        &format!("({})", a80),
        LayoutNode::group(vec![
            LayoutNode::atom("(".to_string()),
            LayoutNode::atom(a80.clone()),
            LayoutNode::atom(")".to_string()),
        ]),
    );

    assert_pretty_print(
        &format!("(\n{}\n)", a80),
        LayoutNode::group(vec![
            LayoutNode::atom("(".to_string()),
            line(),
            LayoutNode::atom(a80.clone()),
            line(),
            LayoutNode::atom(")".to_string()),
        ]),
    );

    assert_pretty_print(
        &format!("( {} )", a40),
        LayoutNode::group(vec![
            LayoutNode::atom("(".to_string()),
            LayoutNode::indent(LayoutNode::concat(vec![
                line(),
                LayoutNode::atom(a40.clone()),
            ])),
            line(),
            LayoutNode::atom(")".to_string()),
        ]),
    );

    assert_pretty_print(
        &format!("(\n  {}\n)", a80),
        LayoutNode::group(vec![
            LayoutNode::atom("(".to_string()),
            LayoutNode::indent(LayoutNode::concat(vec![
                line(),
                LayoutNode::atom(a80.clone()),
            ])),
            line(),
            LayoutNode::atom(")".to_string()),
        ]),
    );
}
