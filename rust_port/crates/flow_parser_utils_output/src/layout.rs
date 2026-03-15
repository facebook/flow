/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::rc::Rc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::source::Source;

#[derive(Debug, Clone, Dupe, PartialEq)]
pub struct LayoutNode(Rc<LayoutNodeInner>);

#[derive(Debug, Clone, PartialEq)]
pub enum LayoutNodeInner {
    /// A layout with location information
    SourceLocation(ALoc, LayoutNode),
    /// A list of nodes that don't break
    Concat(Vec<LayoutNode>),
    /// A list of nodes to try to fit on one line
    Group(Vec<LayoutNode>),
    /// Join elements, allow for breaking over over lines
    Sequence(ListConfig, Vec<LayoutNode>),
    /// Increase the indentation
    Indent(LayoutNode),
    /// Force a line break
    Newline,
    /// Print a string
    Atom(String),
    /// Print an identifier, useful for source map name mappings
    Identifier(ALoc, FlowSmolStr),
    /// Only print left for pretty output else right
    IfPretty(LayoutNode, LayoutNode),
    /// Print left if break else right
    IfBreak(LayoutNode, LayoutNode),
    /// Print nothing
    Empty,
}

impl LayoutNode {
    pub fn source_location(loc: ALoc, node: LayoutNode) -> Self {
        Self(Rc::new(LayoutNodeInner::SourceLocation(loc, node)))
    }
    pub fn concat(nodes: Vec<LayoutNode>) -> Self {
        Self(Rc::new(LayoutNodeInner::Concat(nodes)))
    }
    pub fn group(nodes: Vec<LayoutNode>) -> Self {
        Self(Rc::new(LayoutNodeInner::Group(nodes)))
    }
    pub fn sequence(config: ListConfig, nodes: Vec<LayoutNode>) -> Self {
        Self(Rc::new(LayoutNodeInner::Sequence(config, nodes)))
    }
    pub fn indent(node: LayoutNode) -> Self {
        Self(Rc::new(LayoutNodeInner::Indent(node)))
    }
    pub fn newline() -> Self {
        Self(Rc::new(LayoutNodeInner::Newline))
    }
    pub fn atom(s: String) -> Self {
        Self(Rc::new(LayoutNodeInner::Atom(s)))
    }
    pub fn identifier(loc: ALoc, s: FlowSmolStr) -> Self {
        Self(Rc::new(LayoutNodeInner::Identifier(loc, s)))
    }
    pub fn if_pretty(if_: LayoutNode, else_: LayoutNode) -> Self {
        Self(Rc::new(LayoutNodeInner::IfPretty(if_, else_)))
    }
    pub fn if_break(if_: LayoutNode, else_: LayoutNode) -> Self {
        Self(Rc::new(LayoutNodeInner::IfBreak(if_, else_)))
    }
    pub fn empty() -> Self {
        Self(Rc::new(LayoutNodeInner::Empty))
    }
    pub fn inner(&self) -> &LayoutNodeInner {
        &self.0
    }
    pub fn is_empty(&self) -> bool {
        matches!(self.inner(), LayoutNodeInner::Empty)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WhenToBreak {
    BreakIfNeeded,
    BreakIfPretty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ListConfig {
    pub break_mode: WhenToBreak,
    /// Whether a break should be placed at the start and end of a sequence when
    /// splitting over lines
    pub inline: (bool, bool),
    pub indent: i32,
}

impl ListConfig {
    pub fn seq() -> Self {
        ListConfig {
            break_mode: WhenToBreak::BreakIfNeeded,
            inline: (false, false),
            indent: 2,
        }
    }
}

// Whitespace utils
pub fn space() -> LayoutNode {
    LayoutNode::atom(" ".to_string())
}

pub fn pretty_space() -> LayoutNode {
    LayoutNode::if_pretty(space(), LayoutNode::empty())
}

pub fn ugly_space() -> LayoutNode {
    LayoutNode::if_pretty(LayoutNode::empty(), space())
}

pub fn flat_ugly_space() -> LayoutNode {
    LayoutNode::if_break(LayoutNode::empty(), ugly_space())
}

pub fn hardline() -> LayoutNode {
    LayoutNode::newline()
}

/// Force a line break (`\n`) in pretty mode
pub fn pretty_hardline() -> LayoutNode {
    LayoutNode::if_pretty(LayoutNode::newline(), LayoutNode::empty())
}

/// Inserts a line break (`\n`) if the code doesn't fit on one line, otherwise a space
pub fn line() -> LayoutNode {
    LayoutNode::if_break(LayoutNode::newline(), space())
}

/// Inserts a line break (`\n`) if the code doesn't fit on one line, otherwise a pretty space
pub fn pretty_line() -> LayoutNode {
    LayoutNode::if_break(LayoutNode::newline(), pretty_space())
}

/// Inserts a line break (`\n`) if the code doesn't fit on one line, otherwise nothing
pub fn softline() -> LayoutNode {
    LayoutNode::if_break(LayoutNode::newline(), LayoutNode::empty())
}

pub fn if_pretty(if_: LayoutNode, else_: LayoutNode) -> LayoutNode {
    if if_.is_empty() && else_.is_empty() {
        LayoutNode::empty()
    } else {
        LayoutNode::if_pretty(if_, else_)
    }
}

pub fn if_break(if_: LayoutNode, else_: LayoutNode) -> LayoutNode {
    if if_.is_empty() && else_.is_empty() {
        LayoutNode::empty()
    } else {
        LayoutNode::if_break(if_, else_)
    }
}

pub fn group(items: Vec<LayoutNode>) -> LayoutNode {
    let items = flatten_items(items);
    match items.as_slice() {
        [item] if matches!(item.inner(), LayoutNodeInner::Group(_)) => {
            items.into_iter().next().unwrap()
        }
        _ => LayoutNode::group(items),
    }
}

/// Fuse a list of items together, no spaces or breaks will be inserted
pub fn fuse(items: Vec<LayoutNode>) -> LayoutNode {
    let items = flatten_items(items);
    match items.as_slice() {
        [] => LayoutNode::empty(),
        [item] => item.dupe(),
        _ => LayoutNode::concat(items),
    }
}

pub fn join(sep: LayoutNode, nodes: Vec<LayoutNode>) -> LayoutNode {
    fn helper(sep: &LayoutNode, acc: Vec<LayoutNode>, nodes: &[LayoutNode]) -> Vec<LayoutNode> {
        match nodes {
            [] => acc,
            [hd, tl @ ..] => {
                let mut acc = acc;
                if acc.is_empty() {
                    acc.push(hd.dupe());
                } else {
                    acc.push(sep.dupe());
                    acc.push(hd.dupe());
                }
                helper(sep, acc, tl)
            }
        }
    }
    fuse(helper(&sep, vec![], &nodes))
}

fn flatten_items(items: Vec<LayoutNode>) -> Vec<LayoutNode> {
    let mut result = Vec::new();
    for item in items.into_iter().rev() {
        match item.inner() {
            LayoutNodeInner::Empty => continue,
            LayoutNodeInner::Concat(more) => {
                result.extend(more.iter().rev().map(|n| n.dupe()));
            }
            _ => result.push(item.dupe()),
        }
    }
    result.reverse();
    result
}

pub fn fuse_list(
    sep: Option<LayoutNode>,
    wrap: Option<(LayoutNode, LayoutNode)>,
    items: Vec<LayoutNode>,
) -> LayoutNode {
    fn helper(sep: &LayoutNode, acc: Vec<LayoutNode>, items: &[LayoutNode]) -> Vec<LayoutNode> {
        match items {
            [] => acc,
            [item] => helper(
                sep,
                {
                    let mut acc = acc;
                    acc.push(item.dupe());
                    acc
                },
                &[],
            ),
            [item, rest @ ..] => helper(
                sep,
                {
                    let mut acc = acc;
                    acc.push(item.dupe());
                    acc.push(sep.dupe());
                    acc.push(pretty_space());
                    acc
                },
                rest,
            ),
        }
    }

    let sep = sep.unwrap_or_else(LayoutNode::empty);
    let wrap = wrap.unwrap_or_else(|| (LayoutNode::empty(), LayoutNode::empty()));
    fuse(vec![wrap.0, fuse(helper(&sep, vec![], &items)), wrap.1])
}

pub fn wrap_and_indent(
    wrap: (LayoutNode, LayoutNode),
    break_opt: Option<LayoutNode>,
    items: Vec<LayoutNode>,
) -> LayoutNode {
    let break_node = break_opt.unwrap_or_else(softline);
    let layout = if items.is_empty() {
        LayoutNode::empty()
    } else {
        let mut content = vec![break_node.dupe()];
        content.extend(items);
        fuse(vec![LayoutNode::indent(fuse(content)), break_node])
    };
    fuse(vec![wrap.0, layout, wrap.1])
}

pub fn new_list(
    wrap: Option<(LayoutNode, LayoutNode)>,
    sep: Option<LayoutNode>,
    wrap_spaces: bool,
    trailing_sep: bool,
    items: Vec<LayoutNode>,
) -> LayoutNode {
    let sep = sep.unwrap_or_else(LayoutNode::empty);
    let wrap = wrap.unwrap_or_else(|| (LayoutNode::empty(), LayoutNode::empty()));

    let items_layout = if items.is_empty() {
        vec![]
    } else {
        vec![
            join(fuse(vec![sep.dupe(), pretty_line()]), items),
            if trailing_sep {
                if_break(LayoutNode::atom(",".to_string()), LayoutNode::empty())
            } else {
                LayoutNode::empty()
            },
        ]
    };

    let break_opt = if wrap_spaces {
        Some(pretty_line())
    } else {
        None
    };

    wrap_and_indent(wrap, break_opt, items_layout)
}

/// All purpose list
pub fn list(
    break_mode: Option<WhenToBreak>,
    wrap: Option<(LayoutNode, LayoutNode)>,
    sep: Option<LayoutNode>,
    trailing: bool,
    inline: Option<(bool, bool)>,
    indent: Option<i32>,
    items: Vec<LayoutNode>,
) -> LayoutNode {
    let break_mode = break_mode.unwrap_or(WhenToBreak::BreakIfNeeded);
    let wrap = wrap.unwrap_or_else(|| (LayoutNode::empty(), LayoutNode::empty()));
    let sep = sep.unwrap_or_else(LayoutNode::empty);
    let inline = inline.unwrap_or((false, false));
    let indent = indent.unwrap_or(2);

    let add_separator = |is_last: bool, item: LayoutNode| {
        fuse(vec![
            item,
            if_break(
                if is_last && trailing {
                    if_pretty(sep.dupe(), LayoutNode::empty())
                } else if !is_last {
                    sep.dupe()
                } else {
                    LayoutNode::empty()
                },
                if is_last {
                    LayoutNode::empty()
                } else {
                    fuse(vec![sep.dupe(), pretty_space()])
                },
            ),
        ])
    };

    let items_count = items.len();
    let layout_items: Vec<LayoutNode> = items
        .into_iter()
        .enumerate()
        .map(|(i, item)| add_separator(i == items_count - 1, item))
        .collect();

    let layout_items = fuse(vec![
        wrap.0,
        LayoutNode::sequence(
            ListConfig {
                break_mode,
                inline,
                indent,
            },
            layout_items,
        ),
        wrap.1,
    ]);

    // Wrap items in additional sequence so `IfBreak`s within wrap are
    // not triggered by adjacent lists.
    LayoutNode::sequence(
        ListConfig {
            break_mode: WhenToBreak::BreakIfNeeded,
            inline: (true, true),
            indent: 0,
        },
        vec![layout_items],
    )
}

/// Takes a list of layout nodes and intersperses spaces: a `space` if a space is necessary
/// to separate two tokens, or a `pretty_space` if it's only needed for aesthetics. Generally a
/// space is required, except if the last char of one node or the first char of the next node is
/// a punctuator, then spaces are only for aesthetics (e.g. `new Foo` vs `new(Foo)`)
pub fn fuse_with_space(nodes: Vec<LayoutNode>) -> LayoutNode {
    fn is_punctuator(c: char) -> bool {
        matches!(
            c,
            '{' | '}'
                | '('
                | ')'
                | '['
                | ']'
                | '.'
                | ';'
                | ','
                | '<'
                | '>'
                | '='
                | '!'
                | '+'
                | '-'
                | '*'
                | '%'
                | '^'
                | '&'
                | '~'
                | '|'
                | '?'
                | ':'
                | '/'
                | '"'
                | '\''
        )
    }

    fn ugly_char(mode: CharMode, node: &LayoutNode) -> Option<char> {
        match node.inner() {
            LayoutNodeInner::Atom(s) => {
                let s: &str = s;
                if s.is_empty() {
                    None
                } else {
                    match mode {
                        CharMode::First => s.chars().next(),
                        CharMode::Last => s.chars().last(),
                    }
                }
            }
            LayoutNodeInner::Identifier(_, s) => {
                let s: &str = s;
                if s.is_empty() {
                    None
                } else {
                    match mode {
                        CharMode::First => s.chars().next(),
                        CharMode::Last => s.chars().last(),
                    }
                }
            }
            LayoutNodeInner::Empty => None,
            LayoutNodeInner::Indent(node) => ugly_char(mode, node),
            LayoutNodeInner::Newline => None,
            LayoutNodeInner::SourceLocation(_, node)
            | LayoutNodeInner::IfPretty(_, node)
            | LayoutNodeInner::IfBreak(_, node) => ugly_char(mode, node),
            LayoutNodeInner::Concat(nodes)
            | LayoutNodeInner::Group(nodes)
            | LayoutNodeInner::Sequence(_, nodes) => {
                let iter: Box<dyn Iterator<Item = &LayoutNode>> = match mode {
                    CharMode::First => Box::new(nodes.iter()),
                    CharMode::Last => Box::new(nodes.iter().rev()),
                };
                iter.fold(None, |acc, node| acc.or_else(|| ugly_char(mode, node)))
            }
        }
    }

    fn opt_punctuator(c: Option<char>) -> bool {
        c.is_some_and(is_punctuator)
    }

    fn helper(acc: Vec<LayoutNode>, nodes: &[LayoutNode]) -> Vec<LayoutNode> {
        match nodes {
            [] => acc,
            [first, rest @ ..] if first.is_empty() => helper(acc, rest),
            [a, second, rest @ ..] if second.is_empty() => {
                let mut combined = Vec::new();
                combined.push(a.dupe());
                combined.extend_from_slice(rest);
                helper(acc, &combined)
            }
            [a, b, rest @ ..] => {
                let prev = opt_punctuator(ugly_char(CharMode::Last, a));
                let next = opt_punctuator(ugly_char(CharMode::First, b));
                let sp = if prev || next {
                    pretty_space()
                } else {
                    space()
                };
                let mut new_acc = acc;
                new_acc.push(a.dupe());
                new_acc.push(sp);
                let mut combined = Vec::new();
                combined.push(b.dupe());
                combined.extend_from_slice(rest);
                helper(new_acc, &combined)
            }
            [a] => helper(
                {
                    let mut new_acc = acc;
                    new_acc.push(a.dupe());
                    new_acc
                },
                &[],
            ),
        }
    }

    #[derive(Clone, Copy)]
    enum CharMode {
        First,
        Last,
    }

    fuse(helper(vec![], &nodes))
}

const MAX_WIDTH: usize = 80;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BreakMode {
    Break,
    Flat,
}

type Item<'a> = (BreakMode, &'a LayoutNode);

fn fits(width: i32, rest: &[Item<'_>], items: &[Item<'_>]) -> bool {
    if width < 0 {
        return false;
    }

    match (items, rest) {
        ([], []) => true,
        ([], _) => fits(width, &[], rest),
        ([(mode, hd), tl @ ..], _) => match hd.inner() {
            LayoutNodeInner::Empty => fits(width, rest, tl),
            LayoutNodeInner::SourceLocation(_, hd) => fits(
                width,
                rest,
                &[(*mode, hd)][..]
                    .iter()
                    .chain(tl)
                    .copied()
                    .collect::<Vec<_>>(),
            ),
            LayoutNodeInner::IfPretty(hd, _) => fits(
                width,
                rest,
                &[(*mode, hd)][..]
                    .iter()
                    .chain(tl)
                    .copied()
                    .collect::<Vec<_>>(),
            ),
            LayoutNodeInner::IfBreak(if_, else_) => {
                let node = match mode {
                    BreakMode::Break => if_,
                    BreakMode::Flat => else_,
                };
                fits(
                    width,
                    rest,
                    &[(*mode, node)][..]
                        .iter()
                        .chain(tl)
                        .copied()
                        .collect::<Vec<_>>(),
                )
            }
            LayoutNodeInner::Group(nodes) | LayoutNodeInner::Concat(nodes) => {
                let new_items: Vec<Item> = nodes.iter().rev().map(|n| (*mode, n)).rev().collect();
                fits(width, rest, &[new_items.as_slice(), tl].concat())
            }
            LayoutNodeInner::Indent(node) => fits(
                width,
                rest,
                &[(*mode, node)][..]
                    .iter()
                    .chain(tl)
                    .copied()
                    .collect::<Vec<_>>(),
            ),
            LayoutNodeInner::Newline => match mode {
                BreakMode::Break => true,
                BreakMode::Flat => false,
            },
            LayoutNodeInner::Sequence(config, nodes) => match config.break_mode {
                WhenToBreak::BreakIfPretty => false,
                WhenToBreak::BreakIfNeeded => {
                    let (before, _) = config.inline;
                    (!before && *mode == BreakMode::Break)
                        || fits(
                            width,
                            rest,
                            &[(*mode, &LayoutNode::concat(nodes.clone()))][..]
                                .iter()
                                .chain(tl)
                                .copied()
                                .collect::<Vec<_>>(),
                        )
                }
            },
            LayoutNodeInner::Identifier(_, x) => fits(width - x.len() as i32, rest, tl),
            LayoutNodeInner::Atom(x) => fits(width - x.len() as i32, rest, tl),
        },
    }
}

fn print_node<'a>(
    indent: usize,
    source: &mut Source,
    rest: &[Item<'a>],
    mode: BreakMode,
    node: &'a LayoutNode,
) {
    match node.inner() {
        LayoutNodeInner::SourceLocation(_, node) => {
            print_node(indent, source, rest, mode, node);
        }
        LayoutNodeInner::Concat(nodes) => {
            for node in nodes.iter() {
                print_node(indent, source, rest, mode, node);
            }
        }
        LayoutNodeInner::Newline => {
            source.add_newline();
            source.add_space(indent);
        }
        LayoutNodeInner::Indent(node) => {
            print_node(indent + 2, source, rest, mode, node);
        }
        LayoutNodeInner::Sequence(config, nodes) => match config.break_mode {
            WhenToBreak::BreakIfPretty => {
                let inner_indent = indent + config.indent as usize;
                let (left, right) = config.inline;
                if !left {
                    source.add_newline();
                    source.add_space(inner_indent);
                }
                for (i, node) in nodes.iter().enumerate() {
                    print_node(inner_indent, source, rest, mode, node);
                    if i < nodes.len() - 1 {
                        source.add_newline();
                        source.add_space(inner_indent);
                    }
                }
                if !right {
                    source.add_newline();
                    source.add_space(indent);
                }
            }
            WhenToBreak::BreakIfNeeded => {
                let layout = LayoutNode::sequence(config.clone(), nodes.clone());
                let items = vec![(BreakMode::Flat, &layout)];
                if fits(MAX_WIDTH as i32, rest, &items) {
                    print_node(
                        indent,
                        source,
                        rest,
                        BreakMode::Flat,
                        &LayoutNode::concat(nodes.clone()),
                    );
                } else {
                    let new_config = ListConfig {
                        break_mode: WhenToBreak::BreakIfPretty,
                        ..*config
                    };
                    print_node(
                        indent,
                        source,
                        rest,
                        BreakMode::Break,
                        &LayoutNode::sequence(new_config, nodes.clone()),
                    );
                }
            }
        },
        LayoutNodeInner::Group(nodes) => {
            let layout = LayoutNode::group(nodes.clone());
            let items = vec![(BreakMode::Flat, &layout)];
            let new_mode = if fits(MAX_WIDTH as i32, rest, &items) {
                BreakMode::Flat
            } else {
                BreakMode::Break
            };
            print_node(
                indent,
                source,
                rest,
                new_mode,
                &LayoutNode::concat(nodes.clone()),
            );
        }
        LayoutNodeInner::Atom(s) => {
            source.add_string(s);
        }
        LayoutNodeInner::Identifier(_, s) => {
            source.add_string(s);
        }
        LayoutNodeInner::IfPretty(node, _) => {
            print_node(indent, source, rest, mode, node);
        }
        LayoutNodeInner::IfBreak(on_break, otherwise) => {
            let node = match mode {
                BreakMode::Break => on_break,
                BreakMode::Flat => otherwise,
            };
            print_node(indent, source, rest, mode, node);
        }
        LayoutNodeInner::Empty => {}
    }
}

pub fn print_pretty(node: &LayoutNode) -> String {
    let mut source = Source::default();
    print_node(0, &mut source, &[], BreakMode::Break, node);
    source.contents().to_string()
}

// Compact printer that does not output locations
pub fn print_single_line(node: &LayoutNode) -> String {
    let mut source = Source::default();

    fn print_node_compact(source: &mut Source, node: &LayoutNode) {
        match node.inner() {
            LayoutNodeInner::SourceLocation(_, node) => {
                // this printer does not output locations
                print_node_compact(source, node);
            }
            LayoutNodeInner::Indent(node) => {
                print_node_compact(source, node);
            }
            LayoutNodeInner::IfPretty(node, _) => {
                print_node_compact(source, node);
            }
            LayoutNodeInner::Concat(nodes)
            | LayoutNodeInner::Group(nodes)
            | LayoutNodeInner::Sequence(_, nodes) => {
                for node in nodes.iter() {
                    print_node_compact(source, node);
                }
            }
            LayoutNodeInner::IfBreak(_, no_break) => {
                print_node_compact(source, no_break);
            }
            LayoutNodeInner::Atom(s) => {
                source.add_string(s);
            }
            LayoutNodeInner::Identifier(_, s) => {
                source.add_string(s);
            }
            LayoutNodeInner::Newline => {
                source.add_space(1);
            }
            LayoutNodeInner::Empty => {}
        }
    }

    print_node_compact(&mut source, node);
    source.contents().to_string()
}
