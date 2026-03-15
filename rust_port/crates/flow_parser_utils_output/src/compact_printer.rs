/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::layout::LayoutNode;
use crate::layout::LayoutNodeInner;
use crate::source::Source;

pub fn print(node: &LayoutNode) -> Source {
    fn print_node(src: &mut Source, node: &LayoutNode) {
        match node.inner() {
            LayoutNodeInner::SourceLocation(_loc, node) => print_node(src, node),
            LayoutNodeInner::Concat(nodes)
            | LayoutNodeInner::Group(nodes)
            | LayoutNodeInner::Sequence(_, nodes) => {
                for node in nodes.iter() {
                    print_node(src, node);
                }
            }
            LayoutNodeInner::Indent(node) => print_node(src, node),
            LayoutNodeInner::Newline => {
                src.add_newline();
            }
            LayoutNodeInner::Atom(s) => {
                src.add_string(s);
            }
            LayoutNodeInner::Identifier(_loc, s) => {
                src.add_identifier(s);
            }
            LayoutNodeInner::IfPretty(_, node) => print_node(src, node),
            LayoutNodeInner::IfBreak(_, otherwise) => print_node(src, otherwise),
            LayoutNodeInner::Empty => {}
        }
    }
    let mut src = Source::default();
    print_node(&mut src, node);
    src.add_newline();
    src
}
