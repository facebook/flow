/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::layout::LayoutNode;
use crate::layout::fuse_list;
use crate::layout_test_utils::assert_layout;
use crate::layout_test_utils::layout_builder::pretty_space;

#[test]
fn test_fuse_list() {
    let a = LayoutNode::atom("a".to_string());
    let b = LayoutNode::atom("b".to_string());
    let c = LayoutNode::atom("c".to_string());
    let d = LayoutNode::atom("d".to_string());
    let sep = LayoutNode::atom(",".to_string());

    let actual = fuse_list(None, None, vec![a.clone(), b.clone()]);
    let expected = LayoutNode::concat(vec![a.clone(), pretty_space(), b.clone()]);
    assert_layout(expected, actual);

    let actual = fuse_list(None, None, vec![a.clone(), b.clone(), c.clone()]);
    let expected = LayoutNode::concat(vec![
        a.clone(),
        pretty_space(),
        b.clone(),
        pretty_space(),
        c.clone(),
    ]);
    assert_layout(expected, actual);

    let actual = fuse_list(Some(sep.clone()), None, vec![a.clone(), b.clone()]);
    let expected = LayoutNode::concat(vec![a.clone(), sep.clone(), pretty_space(), b.clone()]);
    assert_layout(expected, actual);

    let actual = fuse_list(
        Some(sep.clone()),
        None,
        vec![a.clone(), b.clone(), c.clone()],
    );
    let expected = LayoutNode::concat(vec![
        a.clone(),
        sep.clone(),
        pretty_space(),
        b.clone(),
        sep.clone(),
        pretty_space(),
        c.clone(),
    ]);
    assert_layout(expected, actual);

    let actual = fuse_list(
        None,
        Some((c.clone(), d.clone())),
        vec![a.clone(), b.clone()],
    );
    let expected = LayoutNode::concat(vec![
        c.clone(),
        a.clone(),
        pretty_space(),
        b.clone(),
        d.clone(),
    ]);
    assert_layout(expected, actual);

    let actual = fuse_list(
        Some(sep.clone()),
        Some((c.clone(), d.clone())),
        vec![a.clone(), b.clone()],
    );
    let expected = LayoutNode::concat(vec![
        c.clone(),
        a.clone(),
        sep.clone(),
        pretty_space(),
        b.clone(),
        d.clone(),
    ]);
    assert_layout(expected, actual);
}
