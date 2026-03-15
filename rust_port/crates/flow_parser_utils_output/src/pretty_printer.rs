/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_data_structure_wrapper::vector::FlowVector;

use crate::layout::LayoutNode;
use crate::layout::LayoutNodeInner;
use crate::layout::ListConfig;
use crate::layout::WhenToBreak;
use crate::source::Source;

// TODO: Make this configurable
const MAX_WIDTH: i32 = 80;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BreakMode {
    Break,
    Flat,
}

struct Writer {
    src: Source,
    pos: i32,
}

type Item = (BreakMode, LayoutNode);

// fits(width, rest, items) determines whether items @ rest can be printed in width
// characters. items and rest are separate as an optimization to avoid list concats.
fn fits(width: i32, rest: &[Item], items: FlowVector<Item>) -> bool {
    if width < 0 {
        return false;
    }

    if items.is_empty() {
        return if rest.is_empty() {
            true
        } else {
            fits(width, &[], rest.iter().cloned().collect())
        };
    }

    let mut items = items;
    let (mode, hd) = items.pop_front().unwrap();
    match hd.inner() {
        LayoutNodeInner::Empty => fits(width, rest, items),
        LayoutNodeInner::SourceLocation(_, hd) => {
            items.push_front((mode, hd.dupe()));
            fits(width, rest, items)
        }
        LayoutNodeInner::IfPretty(hd, _) => {
            items.push_front((mode, hd.dupe()));
            fits(width, rest, items)
        }
        LayoutNodeInner::IfBreak(if_, else_) => {
            let node = match mode {
                BreakMode::Break => if_,
                BreakMode::Flat => else_,
            };
            items.push_front((mode, node.dupe()));
            fits(width, rest, items)
        }
        LayoutNodeInner::Group(nodes) | LayoutNodeInner::Concat(nodes) => {
            let mut new_items: FlowVector<Item> = nodes.iter().map(|n| (mode, n.dupe())).collect();
            new_items.append(items.into_inner());
            fits(width, rest, new_items)
        }
        LayoutNodeInner::Indent(node) => {
            items.push_front((mode, node.dupe()));
            fits(width, rest, items)
        }
        // Respect forced breaks
        LayoutNodeInner::Newline => match mode {
            BreakMode::Break => true,
            BreakMode::Flat => false,
        },
        LayoutNodeInner::Sequence(config, nodes) => match config.break_mode {
            WhenToBreak::BreakIfPretty => false,
            _ => {
                // TODO: need to consider `after`. and indent?
                let (before, _) = config.inline;
                (!before && mode == BreakMode::Break) || {
                    items.push_front((mode, LayoutNode::concat(nodes.clone())));
                    fits(width, rest, items)
                }
            }
        },
        LayoutNodeInner::Identifier(_, x) => fits(width - x.len() as i32, rest, items),
        LayoutNodeInner::Atom(x) => fits(width - x.len() as i32, rest, items),
    }
}

pub fn print(skip_endline: bool, node: &LayoutNode) -> Source {
    fn break_and_indent(ind: i32, w: &mut Writer) {
        w.src.add_newline();
        w.src.add_space(ind as usize);
        w.pos = ind;
        // Reset indentation to our inset
    }

    // folds a non-empty list
    fn fold<F>(f: &F, w: &mut Writer, nodes: &[Item])
    where
        F: Fn(&mut Writer, &[Item], &Item),
    {
        match nodes {
            [node, rest @ ..] => {
                f(w, rest, node);
                fold(f, w, rest);
            }
            [] => {}
        }
    }

    fn print_node(indent: i32, w: &mut Writer, rest: &[Item], item: &Item) {
        let (mode, node) = item;
        match node.inner() {
            LayoutNodeInner::SourceLocation(_loc, node) => {
                print_node(indent, w, rest, &(*mode, node.dupe()));
            }
            LayoutNodeInner::Concat(nodes) => {
                let items: Vec<Item> = nodes.iter().map(|node| (*mode, node.dupe())).collect();
                fold(
                    &|w: &mut Writer, rest: &[Item], item: &Item| {
                        print_node(indent, w, rest, item);
                    },
                    w,
                    &items,
                );
            }
            LayoutNodeInner::Newline => {
                break_and_indent(indent, w);
            }
            LayoutNodeInner::Indent(node) => {
                print_node(indent + 2, w, rest, &(*mode, node.dupe()));
            }
            LayoutNodeInner::Sequence(
                ListConfig {
                    break_mode: WhenToBreak::BreakIfPretty,
                    inline: (left, right),
                    indent: extra_indent,
                },
                nodes,
            ) => {
                let inner_indent = indent + extra_indent;
                if !left {
                    break_and_indent(inner_indent, w);
                }
                let items: Vec<Item> = nodes.iter().map(|node| (*mode, node.dupe())).collect();
                let mut i = items.len() as i32 - 1;
                for (idx, item) in items.iter().enumerate() {
                    print_node(inner_indent, w, &items[idx + 1..], item);
                    if i > 0 {
                        break_and_indent(inner_indent, w);
                        i -= 1;
                    } else {
                        i = 0;
                    }
                }
                if !right {
                    break_and_indent(indent, w);
                }
            }
            LayoutNodeInner::Group(_nodes) => {
                let new_mode = if fits(
                    MAX_WIDTH - w.pos,
                    rest,
                    FlowVector::unit((BreakMode::Flat, node.dupe())),
                ) {
                    BreakMode::Flat
                } else {
                    BreakMode::Break
                };
                let concat = LayoutNode::concat(_nodes.clone());
                print_node(indent, w, rest, &(new_mode, concat));
            }
            LayoutNodeInner::Sequence(
                config @ ListConfig {
                    break_mode: WhenToBreak::BreakIfNeeded,
                    ..
                },
                nodes,
            ) => {
                if fits(
                    MAX_WIDTH - w.pos,
                    rest,
                    FlowVector::unit((BreakMode::Flat, node.dupe())),
                ) {
                    let item = LayoutNode::concat(nodes.clone());
                    print_node(indent, w, rest, &(BreakMode::Flat, item));
                } else {
                    let new_config = ListConfig {
                        break_mode: WhenToBreak::BreakIfPretty,
                        ..config.clone()
                    };
                    let item = LayoutNode::sequence(new_config, nodes.clone());
                    print_node(indent, w, rest, &(BreakMode::Break, item));
                }
            }
            LayoutNodeInner::Atom(s) => {
                w.src.add_string(s);
                w.pos += s.len() as i32;
            }
            LayoutNodeInner::Identifier(_loc, s) => {
                w.src.add_identifier(s);
                w.pos += s.len() as i32;
            }
            LayoutNodeInner::IfPretty(node, _) => {
                print_node(indent, w, rest, &(*mode, node.dupe()));
            }
            LayoutNodeInner::IfBreak(on_break, otherwise) => {
                let node = match mode {
                    BreakMode::Break => on_break,
                    BreakMode::Flat => otherwise,
                };
                print_node(indent, w, rest, &(*mode, node.dupe()));
            }
            LayoutNodeInner::Empty => {}
        }
    }

    let mut w = Writer {
        src: Source::default(),
        pos: 0,
    };
    print_node(0, &mut w, &[], &(BreakMode::Break, node.dupe()));
    if !skip_endline {
        w.src.add_newline();
    }
    w.src
}
