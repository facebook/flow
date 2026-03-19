/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(dead_code)]

use flow_aloc::ALoc;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;

use crate::js_layout_generator;
use crate::layout::LayoutNode;
use crate::layout::LayoutNodeInner;
use crate::layout::ListConfig;
use crate::layout::WhenToBreak;

pub mod layout_builder {
    use super::*;

    pub fn expression(
        expr_ctxt: Option<&js_layout_generator::ExpressionContext>,
        opts: Option<&js_layout_generator::Opts>,
        ast: &flow_parser::ast::expression::Expression<Loc, Loc>,
    ) -> LayoutNode {
        let default_opts = js_layout_generator::default_opts();
        let opts = opts.unwrap_or(&default_opts);
        js_layout_generator::expression(opts, expr_ctxt, ast)
    }

    pub fn empty() -> LayoutNode {
        LayoutNode::empty()
    }

    pub fn loc(loc: Option<Loc>, node: LayoutNode) -> LayoutNode {
        let loc = loc.unwrap_or(LOC_NONE);
        LayoutNode::source_location(ALoc::of_loc(loc), node)
    }

    pub fn program_loc(loc: Loc) -> Loc {
        Loc {
            start: flow_parser::loc::Position { line: 1, column: 0 },
            ..loc
        }
    }

    pub fn program(prog_loc: Option<Loc>, node: LayoutNode) -> LayoutNode {
        let prog_loc = prog_loc.unwrap_or_else(Loc::none);
        loc(Some(program_loc(prog_loc)), node)
    }

    pub fn sequence(
        break_mode: WhenToBreak,
        inline: Option<(bool, bool)>,
        indent: Option<i32>,
        items: Vec<LayoutNode>,
    ) -> LayoutNode {
        let inline = inline.unwrap_or((false, false));
        let indent = indent.unwrap_or(2);
        LayoutNode::sequence(
            ListConfig {
                break_mode,
                inline,
                indent,
            },
            items,
        )
    }

    pub fn group(items: Vec<LayoutNode>) -> LayoutNode {
        LayoutNode::group(items)
    }

    pub fn fused(items: Vec<LayoutNode>) -> LayoutNode {
        LayoutNode::concat(items)
    }

    pub fn fused_vertically(
        indent: Option<i32>,
        inline: Option<(bool, bool)>,
        items: Vec<LayoutNode>,
    ) -> LayoutNode {
        let indent = indent.unwrap_or(0);
        let inline = inline.unwrap_or((false, false));
        LayoutNode::sequence(
            ListConfig {
                break_mode: WhenToBreak::BreakIfPretty,
                indent,
                inline,
            },
            items,
        )
    }

    pub fn id(loc: Option<Loc>, s: &str) -> LayoutNode {
        let loc = loc.unwrap_or_else(Loc::none);
        LayoutNode::identifier(
            ALoc::of_loc(loc),
            flow_data_structure_wrapper::smol_str::FlowSmolStr::from(s),
        )
    }

    pub fn atom(s: &str) -> LayoutNode {
        LayoutNode::atom(s.to_string())
    }

    pub fn space() -> LayoutNode {
        LayoutNode::atom(" ".to_string())
    }

    pub fn hardline() -> LayoutNode {
        LayoutNode::newline()
    }

    pub fn pretty_space() -> LayoutNode {
        LayoutNode::if_pretty(space(), LayoutNode::empty())
    }

    pub fn ugly_space() -> LayoutNode {
        LayoutNode::if_pretty(LayoutNode::empty(), space())
    }

    pub fn flat_pretty_space() -> LayoutNode {
        LayoutNode::if_break(LayoutNode::empty(), pretty_space())
    }

    pub fn pretty_hardline() -> LayoutNode {
        LayoutNode::if_pretty(LayoutNode::newline(), LayoutNode::empty())
    }

    pub fn line() -> LayoutNode {
        LayoutNode::if_break(LayoutNode::newline(), space())
    }

    pub fn pretty_line() -> LayoutNode {
        LayoutNode::if_break(LayoutNode::newline(), pretty_space())
    }

    pub fn softline() -> LayoutNode {
        LayoutNode::if_break(LayoutNode::newline(), LayoutNode::empty())
    }

    pub fn indent(node: LayoutNode) -> LayoutNode {
        LayoutNode::indent(node)
    }

    pub fn wrap_in_parens(x: LayoutNode) -> LayoutNode {
        LayoutNode::group(vec![atom("("), x, atom(")")])
    }

    enum PrinterPos {
        Word(String),
        Phrase(String),
    }

    pub fn printer(node: &LayoutNode) -> String {
        use LayoutNodeInner::*;

        fn is_program_loc(loc: &ALoc) -> bool {
            let loc = loc.to_loc_exn();
            loc.source.is_none()
                && loc.start.line == 1
                && loc.start.column == 0
                && loc.end.line == 0
                && loc.end.column == 0
        }

        fn string_of_loc(loc: &ALoc) -> String {
            let loc = loc.to_loc_exn();
            format!(
                "{{Loc.none with start={{Loc.line={}; column={}}}; _end={{Loc.line={}; column={}}}}}",
                loc.start.line, loc.start.column, loc.end.line, loc.end.column
            )
        }

        fn string_of_when_to_break(b: &WhenToBreak) -> &'static str {
            match b {
                WhenToBreak::BreakIfNeeded => "Layout.Break_if_needed",
                WhenToBreak::BreakIfPretty => "Layout.Break_if_pretty",
            }
        }

        fn top(i: usize, node: &LayoutNode) -> PrinterPos {
            match node.inner() {
                SourceLocation(loc, child) => {
                    if is_program_loc(loc) {
                        PrinterPos::Phrase(format!("program {}", helper(i, child)))
                    } else {
                        let loc_str = if *loc.to_loc_exn() == LOC_NONE {
                            String::new()
                        } else {
                            format!(" ~loc:{}", string_of_loc(loc))
                        };
                        PrinterPos::Phrase(format!("loc{} {}", loc_str, helper(i, child)))
                    }
                }
                Group(items) if items.len() == 3 => {
                    if let (Atom(a), _, Atom(b)) = (items[0].inner(), &items[1], items[2].inner()) {
                        if a == "(" && b == ")" {
                            return PrinterPos::Phrase(format!(
                                "wrap_in_parens {}",
                                helper(i, &items[1])
                            ));
                        }
                    }
                    PrinterPos::Phrase(format!("group {}", list(i, items)))
                }
                Group(items) => PrinterPos::Phrase(format!("group {}", list(i, items))),
                Concat(items) => PrinterPos::Phrase(format!("fused {}", list(i, items))),
                Sequence(config, items)
                    if config.break_mode == WhenToBreak::BreakIfPretty
                        && config.inline == (false, false)
                        && config.indent == 0 =>
                {
                    PrinterPos::Phrase(format!("fused_vertically {}", list(i, items)))
                }
                Sequence(config, items) => {
                    let break_str =
                        format!(" ~break:{}", string_of_when_to_break(&config.break_mode));
                    let inline_str = if config.inline == (false, false) {
                        String::new()
                    } else {
                        format!(" ~inline:({}, {})", config.inline.0, config.inline.1)
                    };
                    let indent_str = if config.indent == 2 {
                        String::new()
                    } else {
                        format!(" ~indent:{}", config.indent)
                    };
                    PrinterPos::Phrase(format!(
                        "sequence{}{}{} {}",
                        break_str,
                        inline_str,
                        indent_str,
                        list(i, items)
                    ))
                }
                Atom(s) if s == " " => PrinterPos::Word("space".to_string()),
                Atom(s) => PrinterPos::Phrase(format!("atom {:?}", s)),
                Identifier(loc, s) => {
                    let loc_str = if *loc.to_loc_exn() == LOC_NONE {
                        String::new()
                    } else {
                        format!(" ~loc:{}", string_of_loc(loc))
                    };
                    PrinterPos::Phrase(format!("id{} {:?}", loc_str, s.as_str()))
                }
                IfPretty(left, right) => match (left.inner(), right.inner()) {
                    (Atom(s), Empty) if s == " " => PrinterPos::Word("pretty_space".to_string()),
                    (Newline, Empty) => PrinterPos::Word("pretty_hardline".to_string()),
                    (Empty, Atom(s)) if s == " " => PrinterPos::Word("ugly_space".to_string()),
                    _ => PrinterPos::Phrase(format!(
                        "Layout.IfPretty ({}, {})",
                        helper(i, left),
                        helper(i, right)
                    )),
                },
                IfBreak(left, right) => match (left.inner(), right.inner()) {
                    (Empty, IfPretty(inner_l, inner_r))
                        if matches!(inner_l.inner(), Atom(s) if s == " ")
                            && matches!(inner_r.inner(), Empty) =>
                    {
                        PrinterPos::Word("flat_pretty_space".to_string())
                    }
                    (Newline, Atom(s)) if s == " " => PrinterPos::Word("line".to_string()),
                    (Newline, IfPretty(inner_l, inner_r))
                        if matches!(inner_l.inner(), Atom(s) if s == " ")
                            && matches!(inner_r.inner(), Empty) =>
                    {
                        PrinterPos::Word("pretty_line".to_string())
                    }
                    (Newline, Empty) => PrinterPos::Word("softline".to_string()),
                    _ => PrinterPos::Phrase(format!(
                        "Layout.IfBreak ({}, {})",
                        helper(i, left),
                        helper(i, right)
                    )),
                },
                Indent(node) => PrinterPos::Phrase(format!("indent ({})", helper(i, node))),
                Newline => PrinterPos::Word("hardline".to_string()),
                Empty => PrinterPos::Word("empty".to_string()),
            }
        }

        fn list(i: usize, nodes: &[LayoutNode]) -> String {
            let indent = " ".repeat(i * 2);
            let items: Vec<String> = nodes
                .iter()
                .map(|node| {
                    let s = match top(i + 1, node) {
                        PrinterPos::Word(s) | PrinterPos::Phrase(s) => s,
                    };
                    format!("  {}{};", indent, s)
                })
                .collect();
            format!("[\n{}\n{}]", items.join("\n"), indent)
        }

        fn helper(i: usize, node: &LayoutNode) -> String {
            match top(i, node) {
                PrinterPos::Word(s) => s,
                PrinterPos::Phrase(s) => format!("({})", s),
            }
        }

        format!("L.{}", helper(0, node))
    }
}

pub mod layout_matcher {
    use super::*;

    pub type MatchResult<T> = Result<T, (String, LayoutNode)>;

    pub fn empty(node: &LayoutNode) -> MatchResult<()> {
        match node.inner() {
            LayoutNodeInner::Empty => Ok(()),
            _ => Err(("expected Empty".to_string(), node.clone())),
        }
    }

    pub fn loc(node: &LayoutNode) -> MatchResult<LayoutNode> {
        match node.inner() {
            LayoutNodeInner::SourceLocation(_, x) => Ok(x.clone()),
            _ => Err(("expected SourceLocation".to_string(), node.clone())),
        }
    }

    pub fn indent(node: &LayoutNode) -> MatchResult<LayoutNode> {
        match node.inner() {
            LayoutNodeInner::Indent(x) => Ok(x.clone()),
            _ => Err(("expected Indent".to_string(), node.clone())),
        }
    }

    pub fn group(node: &LayoutNode) -> MatchResult<Vec<LayoutNode>> {
        match node.inner() {
            LayoutNodeInner::Group(x) => Ok(x.clone()),
            _ => Err(("expected Group".to_string(), node.clone())),
        }
    }

    pub fn nth_group(n: usize, node: &LayoutNode) -> MatchResult<LayoutNode> {
        match node.inner() {
            LayoutNodeInner::Group(xs) => xs
                .get(n)
                .cloned()
                .ok_or_else(|| (format!("couldn't get {}th Group element", n), node.clone())),
            _ => Err(("expected Group".to_string(), node.clone())),
        }
    }

    pub fn fused(node: &LayoutNode) -> MatchResult<Vec<LayoutNode>> {
        match node.inner() {
            LayoutNodeInner::Concat(x) => Ok(x.clone()),
            _ => Err(("expected Concat".to_string(), node.clone())),
        }
    }

    pub fn nth_fused(n: usize, node: &LayoutNode) -> MatchResult<LayoutNode> {
        match node.inner() {
            LayoutNodeInner::Concat(xs) => xs
                .get(n)
                .cloned()
                .ok_or_else(|| (format!("couldn't get {}th Concat element", n), node.clone())),
            _ => Err(("expected Concat".to_string(), node.clone())),
        }
    }

    pub fn atom(node: &LayoutNode) -> MatchResult<String> {
        match node.inner() {
            LayoutNodeInner::Atom(x) => Ok(x.clone()),
            _ => Err(("expected Atom".to_string(), node.clone())),
        }
    }

    // TODO: support matching break, inline, indent
    pub fn sequence(node: &LayoutNode) -> MatchResult<Vec<LayoutNode>> {
        match node.inner() {
            LayoutNodeInner::Sequence(_, x) => Ok(x.clone()),
            _ => Err(("expected Sequence".to_string(), node.clone())),
        }
    }

    pub fn nth_sequence(n: usize, node: &LayoutNode) -> MatchResult<LayoutNode> {
        match node.inner() {
            LayoutNodeInner::Sequence(_, xs) => xs
                .get(n)
                .cloned()
                .ok_or_else(|| (format!("couldn't get {}th Layout element", n), node.clone())),
            _ => Err(("expected Sequence".to_string(), node.clone())),
        }
    }

    pub fn pretty_space(node: &LayoutNode) -> MatchResult<()> {
        match node.inner() {
            LayoutNodeInner::IfPretty(left, right) => {
                if matches!(left.inner(), LayoutNodeInner::Atom(s) if s == " ")
                    && matches!(right.inner(), LayoutNodeInner::Empty)
                {
                    Ok(())
                } else {
                    Err(("expected pretty space".to_string(), node.clone()))
                }
            }
            _ => Err(("expected pretty space".to_string(), node.clone())),
        }
    }

    // higher level helpers
    pub fn body_of_function_declaration(
        ast: &flow_parser::ast::statement::Statement<Loc, Loc>,
    ) -> MatchResult<LayoutNode> {
        let opts = js_layout_generator::default_opts();
        let layout = js_layout_generator::statement(&opts, false, ast);
        let layout = loc(&layout)?;
        let layout = nth_fused(5, &layout)?; // skip `function`, space, name, space, params
        let layout = loc(&layout)?;
        let layout = nth_group(1, &layout)?; // skip opening {
        let layout = indent(&layout)?; // body is indented
        let items = fused(&layout)?;
        // skip newline after {
        match items.as_slice() {
            [] => Ok(LayoutNode::concat(vec![])),
            [_newline, rest @ ..] => Ok(LayoutNode::concat(rest.to_vec())),
        }
    }
}

pub fn assert_layout(expected: LayoutNode, actual: LayoutNode) {
    pretty_assertions::assert_eq!(
        layout_builder::printer(&expected),
        layout_builder::printer(&actual),
    );
}

pub fn assert_layout_result(expected: LayoutNode, actual: layout_matcher::MatchResult<LayoutNode>) {
    match actual {
        Ok(layout) => assert_layout(expected, layout),
        Err((msg, layout)) => {
            panic!(
                "Unable to decode {}:\n{}",
                layout_builder::printer(&layout),
                msg
            );
        }
    }
}

pub fn assert_layout_of_expression(
    expr_ctxt: Option<&js_layout_generator::ExpressionContext>,
    opts: Option<&js_layout_generator::Opts>,
    expected: LayoutNode,
    ast: &flow_parser::ast::expression::Expression<Loc, Loc>,
) {
    let default_opts = js_layout_generator::default_opts();
    let opts = opts.unwrap_or(&default_opts);
    let actual = js_layout_generator::expression(opts, expr_ctxt, ast);
    assert_layout(expected, actual);
}

pub fn assert_layout_of_statement(
    opts: Option<&js_layout_generator::Opts>,
    expected: LayoutNode,
    ast: &flow_parser::ast::statement::Statement<Loc, Loc>,
) {
    let default_opts = js_layout_generator::default_opts();
    let opts = opts.unwrap_or(&default_opts);
    let actual = js_layout_generator::statement(opts, false, ast);
    assert_layout(expected, actual);
}

pub fn assert_layout_of_statement_string(expected: LayoutNode, source: &str) {
    let ast = flow_parser_utils::ast_builder::test_statement_of_string(source);
    assert_layout_of_statement(None, expected, &ast);
}
