/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use serde_json::Value;

use crate::flow_result::FlowLoc;
use crate::flow_result::FlowPos;

#[derive(Clone)]
pub(crate) struct PathNode<'a> {
    pub(crate) key: String,
    pub(crate) ast: &'a Value,
}

impl<'a> PathNode<'a> {
    fn new(key: impl Into<String>, ast: &'a Value) -> Self {
        Self {
            key: key.into(),
            ast,
        }
    }
}

fn get_child_node_containing_location<'a>(
    node: &'a Value,
    is_location_within_node: &impl Fn(&Value) -> bool,
) -> Option<Vec<PathNode<'a>>> {
    let object = node.as_object()?;
    // Assuming node siblings are non overlapping we only need to find the
    // first node that has contains the location.
    for (key, child_node) in object {
        if let Some(children) = child_node.as_array() {
            // Assume list items only contain ASTNode's (no nested lists).
            for (list_key, list_child_node) in children.iter().enumerate() {
                if is_location_within_node(list_child_node) {
                    return Some(vec![
                        PathNode::new(key.as_str(), child_node),
                        PathNode::new(list_key.to_string(), list_child_node),
                    ]);
                }
            }
            continue;
        }

        if matches!(key.as_str(), "range" | "comments" | "loc") {
            continue;
        }
        if is_location_within_node(child_node) {
            return Some(vec![PathNode::new(key.as_str(), child_node)]);
        }
    }

    None
}

/**
 * Find path to a location within an AST.
 *
 * Note: This function assumes children nodes are always equal to or within
 *       the location of its parent and no siblings are overlapping. This
 *       allows much faster traversal but will fail to find nodes if these
 *       constraints are not maintained.
 */
fn find_path_to_location<'a>(
    root_node: &'a Value,
    filter_node_by_location: impl Fn(&Value) -> bool,
) -> Vec<PathNode<'a>> {
    let mut path = vec![PathNode::new("root", root_node)];
    loop {
        let current = path
            .last()
            .expect("path should always contain the root node")
            .ast;
        let Some(found_path_nodes) = get_child_node_containing_location(current, &|node| {
            node_type(node).is_some() && node.get("loc").is_some() && filter_node_by_location(node)
        }) else {
            return path;
        };
        path.extend(found_path_nodes);
    }
}

pub(crate) fn node_type(value: &Value) -> Option<&str> {
    value.get("type").and_then(Value::as_str)
}

// Returns true if position a comes before or is equal to position b
fn is_before_or_equal(left: &FlowPos, right: &FlowPos) -> bool {
    if left.line == right.line {
        return left.column <= right.column;
    }
    left.line <= right.line
}

/* Given a location and an AST, find the ast node whose location falls within
 * the given location. Then return the path to that node. */
pub(crate) fn get_path_to_loc<'a>(
    error_loc: &FlowLoc,
    ast_root: &'a Value,
) -> Option<Vec<PathNode<'a>>> {
    Some(find_path_to_location(ast_root, |node| {
        let Some(loc) = node.get("loc") else {
            return false;
        };
        let Some(start) = loc.get("start") else {
            return false;
        };
        let Some(end) = loc.get("end") else {
            return false;
        };
        let Some(start_line) = start.get("line").and_then(Value::as_i64) else {
            return false;
        };
        let Some(start_column) = start.get("column").and_then(Value::as_i64) else {
            return false;
        };
        let Some(end_line) = end.get("line").and_then(Value::as_i64) else {
            return false;
        };
        let Some(end_column) = end.get("column").and_then(Value::as_i64) else {
            return false;
        };
        let start = FlowPos {
            line: start_line,
            column: start_column,
            offset: None,
        };
        let end = FlowPos {
            line: end_line,
            column: end_column,
            offset: None,
        };

        is_before_or_equal(&start, &error_loc.start) && is_before_or_equal(&error_loc.end, &end)
    }))
}

pub(crate) fn get_node_at_range(range: (usize, usize), ast_root: &Value) -> Option<&Value> {
    let mut path = find_path_to_location(ast_root, |node| {
        let Some(node_range) = node.get("range").and_then(Value::as_array) else {
            return false;
        };
        let Some(start) = node_range.first().and_then(Value::as_u64) else {
            return false;
        };
        let Some(end) = node_range.get(1).and_then(Value::as_u64) else {
            return false;
        };
        start as usize <= range.0 && range.1 <= end as usize
    });
    let last_path_item = path.pop()?;
    // We only care about exact matches
    let last_range = last_path_item.ast.get("range")?.as_array()?;
    let start = last_range.first()?.as_u64()? as usize;
    let end = last_range.get(1)?.as_u64()? as usize;
    ((start, end) == range).then_some(last_path_item.ast)
}
