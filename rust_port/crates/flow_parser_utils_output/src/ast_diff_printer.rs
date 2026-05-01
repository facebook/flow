/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;

use dupe::Dupe;
use flow_common_utils::list_utils;
use flow_parser::ast;
use flow_parser::loc::Loc;
use flow_parser_utils::flow_ast_differ::Change;
use flow_parser_utils::flow_ast_differ::ExpressionNodeParent;
use flow_parser_utils::flow_ast_differ::Node;
use flow_parser_utils::flow_ast_differ::NodeChange;
use flow_parser_utils::flow_ast_differ::NodeChanges;
use flow_parser_utils::flow_ast_differ::StatementNodeParent;

use crate::js_layout_generator;
use crate::js_layout_generator::Opts;
use crate::layout;
use crate::layout::LayoutNode;
use crate::layout::fuse;
use crate::pretty_printer;
use crate::replacement_printer::LocPatch;

fn infer_indentation_count(parent: &StatementNodeParent) -> usize {
    match parent {
        StatementNodeParent::StatementBlockParentOfStatement(loc) => {
            // We use the end column since start might be at anywhere.
            // e.g.
            // function foo() {
            //   stmt
            // } // <- end indentation + 1 is the stmt indentation.
            (loc.end.column as usize / 2) + 1
        }
        StatementNodeParent::ExportParentOfStatement(loc)
        | StatementNodeParent::LabeledStatementParentOfStatement(loc) => {
            // We need to use the parent of parent statement, since the nested statement inside export and
            // label should have the same indentation level as parent.
            loc.start.column as usize / 2
        }
        StatementNodeParent::IfParentOfStatement(loc)
        | StatementNodeParent::LoopParentOfStatement(loc)
        | StatementNodeParent::WithStatementParentOfStatement(loc)
        | StatementNodeParent::SwitchCaseParentOfStatement(loc)
        | StatementNodeParent::MatchCaseParentOfStatement(loc) => {
            // These cases introduces one more level of nesting.
            (loc.start.column as usize / 2) + 1
        }
        StatementNodeParent::TopLevelParentOfStatement => 0,
    }
}

fn wrap_with_inferred_indentation(
    statement_node_parent: &StatementNodeParent,
    node: LayoutNode,
) -> LayoutNode {
    let indentation_count = infer_indentation_count(statement_node_parent);
    let mut result = node;
    for _ in 0..indentation_count {
        result = LayoutNode::indent(result);
    }
    result
}

fn layout_of_node(opts: &Opts, node: &Node) -> LayoutNode {
    match node {
        Node::Raw(str) => LayoutNode::atom(str.clone()),
        Node::Comment(c) => js_layout_generator::comment(false, c),
        Node::StringLiteral(loc, lit) => js_layout_generator::string_literal(opts, loc, lit),
        Node::NumberLiteral(loc, lit) => js_layout_generator::number_literal(opts, loc, lit),
        Node::BigIntLiteral(loc, lit) => js_layout_generator::bigint_literal(loc, lit),
        Node::BooleanLiteral(loc, lit) => js_layout_generator::boolean_literal(loc, lit),
        Node::RegExpLiteral(loc, lit) => js_layout_generator::regexp_literal(opts, loc, lit),
        Node::ModuleRefLiteral(loc, lit) => js_layout_generator::module_ref_literal(loc, lit),
        Node::Statement(stmt, parent) => {
            let layout = js_layout_generator::statement(opts, false, stmt);
            wrap_with_inferred_indentation(parent, layout)
        }
        Node::Program(prog) => js_layout_generator::program(opts, true, None, prog),
        // Do not wrap expression in parentheses for cases where we know parentheses are not needed.
        Node::Expression(expr, ExpressionNodeParent::StatementParentOfExpression(_))
        | Node::Expression(expr, ExpressionNodeParent::SlotParentOfExpression) => {
            js_layout_generator::expression(opts, None, expr)
        }
        Node::Expression(expr, ExpressionNodeParent::MatchExpressionCaseBodyParentOfExpression) => {
            match expr.deref() {
                ast::expression::ExpressionInner::Sequence { .. } => fuse(vec![
                    LayoutNode::atom("(".to_string()),
                    js_layout_generator::expression(opts, None, expr),
                    LayoutNode::atom(")".to_string()),
                ]),
                _ => js_layout_generator::expression(opts, None, expr),
            }
        }
        Node::Expression(
            expr,
            ExpressionNodeParent::ExpressionParentOfExpression(_)
            | ExpressionNodeParent::ClassExtends
            | ExpressionNodeParent::SpreadParentOfExpression,
        ) => {
            // TODO use expression context for printing to insert parens when actually needed.
            fuse(vec![
                LayoutNode::atom("(".to_string()),
                js_layout_generator::expression(opts, None, expr),
                LayoutNode::atom(")".to_string()),
            ])
        }
        Node::Pattern(pat) => js_layout_generator::pattern(opts, None, pat),
        Node::Params(params) => js_layout_generator::function_params(opts, params),
        Node::Variance(var) => js_layout_generator::variance(var),
        Node::Type(typ) => js_layout_generator::type_(opts, typ),
        Node::TypeParam(t_param) => js_layout_generator::type_param(opts, t_param),
        Node::TypeAnnotation(annot) => js_layout_generator::type_annotation(opts, false, annot),
        Node::TypeGuard(guard) => js_layout_generator::type_guard(opts, true, guard),
        Node::TypeGuardAnnotation(guard) => {
            js_layout_generator::type_guard_annotation(opts, true, guard)
        }
        Node::FunctionTypeAnnotation(annot) => {
            js_layout_generator::type_annotation(opts, true, annot)
        }
        Node::ClassProperty(prop) => js_layout_generator::class_property(opts, prop),
        Node::ClassPrivateField(field) => js_layout_generator::class_private_field(opts, field),
        Node::ObjectProperty(prop) => js_layout_generator::object_property(opts, prop),
        Node::TemplateLiteral(_, t_lit) => js_layout_generator::template_literal(opts, t_lit),
        Node::JSXChild(child) => match js_layout_generator::jsx_child(opts, child) {
            Some((_, layout_node)) => layout_node,
            // This case shouldn't happen, so return Empty
            None => LayoutNode::empty(),
        },
        Node::JSXIdentifier(id) => js_layout_generator::jsx_identifier(id),
        Node::MatchPattern(pat) => js_layout_generator::match_pattern(opts, pat),
        Node::MatchObjectPatternProperty(prop) => {
            js_layout_generator::match_object_pattern_property(opts, prop)
        }
    }
}

pub fn text_of_layout(layout: LayoutNode) -> String {
    // wrap these layout fragments in a group so they try to fit on one line
    let layout = layout::group(vec![layout]);
    pretty_printer::print(true, &layout).contents()
}

fn text_of_node(opts: &Opts, node: &Node) -> String {
    text_of_layout(layout_of_node(opts, node))
}

fn is_statement_list(nodes: &[Node]) -> bool {
    nodes
        .iter()
        .all(|node| matches!(node, Node::Statement(_, _)))
}

fn text_of_statement_list(opts: &Opts, nodes: &[Node]) -> String {
    let statements_with_parents: Vec<_> = nodes
        .iter()
        .filter_map(|node| match node {
            Node::Statement(stmt, parent) => Some((stmt, parent)),
            _ => None,
        })
        .collect();
    let parent = match statements_with_parents.first() {
        None => &StatementNodeParent::TopLevelParentOfStatement,
        Some((_, parent)) => parent,
    };
    let stmts: Vec<_> = statements_with_parents
        .iter()
        .map(|(stmt, _)| (*stmt).clone())
        .collect();
    let layout_nodes = js_layout_generator::statement_list(opts, false, &stmts);
    let fused = fuse(layout_nodes);
    let wrapped = wrap_with_inferred_indentation(parent, fused);
    text_of_layout(wrapped)
}

fn text_of_nodes(
    opts: &Opts,
    separator: &Option<String>,
    leading_separator: bool,
    nodes: &[Node],
) -> String {
    let sep = match separator {
        Some(str) => str.as_str(),
        None => "\n",
    };
    let text = list_utils::to_string(sep, |node| text_of_node(opts, node), nodes);
    if leading_separator {
        format!("{}{}", sep, text)
    } else {
        text
    }
}

fn edit_of_change(opts: &Opts, change: &NodeChange) -> (Loc, String) {
    let (loc, change_inner) = change;
    match change_inner {
        Change::Replace(Node::Comment(old_comment), Node::Comment(new_comment))
            if old_comment.kind == new_comment.kind =>
        {
            (
                loc.dupe(),
                text_of_layout(js_layout_generator::comment(true, new_comment)),
            )
        }
        Change::Replace(_, new_node) => (loc.dupe(), text_of_node(opts, new_node)),
        Change::Insert {
            items,
            separator,
            leading_separator,
        } if is_statement_list(items) => {
            let text = text_of_statement_list(opts, items);
            if *leading_separator {
                let sep = match separator {
                    Some(str) => str.as_str(),
                    None => "\n",
                };
                (loc.dupe(), format!("{}{}", sep, text))
            } else {
                (loc.dupe(), text)
            }
        }
        Change::Insert {
            items,
            separator,
            leading_separator,
        } => (
            loc.dupe(),
            text_of_nodes(opts, separator, *leading_separator, items),
        ),
        Change::Delete(_) => (loc.dupe(), String::new()),
    }
}

pub fn edits_of_changes(opts: &Opts, changes: &NodeChanges) -> LocPatch {
    edits_of_changes_rec(opts, changes, 0)
}

fn edits_of_changes_rec(opts: &Opts, changes: &NodeChanges, idx: usize) -> LocPatch {
    if idx >= changes.len() {
        return Vec::new();
    }

    if idx + 1 < changes.len() {
        let (loc1, change1) = &changes[idx];
        let (loc2, change2) = &changes[idx + 1];

        // Detect the case when a statement list was broken up into a Replace with a statement
        // followed by an Insert containing a statement list. Reconstruct the original statement
        // list to print so that whitespace can be preserved between the first and second statements.
        if let (Change::Replace(_, Node::Statement(stmt, parent)), Change::Insert { items, .. }) =
            (change1, change2)
        {
            if loc1.end_loc() == *loc2 && is_statement_list(items) {
                let mut combined = vec![Node::Statement(stmt.clone(), parent.clone())];
                combined.extend(items.iter().cloned());
                let text = text_of_statement_list(opts, &combined);
                let mut result = vec![(loc1.dupe(), text)];
                result.extend(edits_of_changes_rec(opts, changes, idx + 2));
                return result;
            }
        }

        // Detect the case when we want to replace a list of statements with a single statement.
        // The AST diffing algorithm will translate this into a replace followed by a delete.
        // We should coalesce this into a single replace spanning both the replace and delete with
        // the replacement, so that we can avoid an empty line being printed in the place of deleted
        // statements.
        if let (Change::Replace(old_node, new_node @ Node::Statement(stmt, _)), Change::Delete(_)) =
            (change1, change2)
        {
            let new_loc = stmt.loc().dupe();
            let btwn = Loc::between(loc1, loc2);
            if new_loc.contains(&btwn) {
                let mut new_changes =
                    vec![(new_loc, Change::Replace(old_node.clone(), new_node.clone()))];
                new_changes.extend(changes[idx + 2..].iter().cloned());
                return edits_of_changes_rec(opts, &new_changes, 0);
            }
        }

        // Similar to the case above, but sometimes we have diff patterns like:
        // expr1; // expr replaced by expr2, not the entire statement
        // s2;    // deleted
        // s3;    // deleted
        // // ...
        //
        // This case changes the diff on (expr1, expr2) to be (expr1;, expr2;), so it can be coalesced
        // with later deleted statements like above.
        if let (
            Change::Replace(
                old_node,
                Node::Expression(
                    expr,
                    ExpressionNodeParent::StatementParentOfExpression(parent_stmt),
                ),
            ),
            Change::Delete(_),
        ) = (change1, change2)
        {
            let new_loc = expr.loc().dupe();
            if let ast::statement::StatementInner::Expression { inner, .. } = parent_stmt.deref() {
                let btwn = Loc::between(loc1, loc2);
                if new_loc.contains(&btwn) {
                    let expression_statement = ast::statement::StatementInner::Expression {
                        loc: new_loc.dupe(),
                        inner: inner.clone(),
                    };
                    let new_stmt = ast::statement::Statement::new(expression_statement);
                    let mut new_changes = vec![(
                        new_loc,
                        Change::Replace(
                            old_node.clone(),
                            Node::Statement(
                                new_stmt,
                                StatementNodeParent::TopLevelParentOfStatement,
                            ),
                        ),
                    )];
                    new_changes.extend(changes[idx + 2..].iter().cloned());
                    return edits_of_changes_rec(opts, &new_changes, 0);
                }
            }
        }
    }

    let mut result = vec![edit_of_change(opts, &changes[idx])];
    result.extend(edits_of_changes_rec(opts, changes, idx + 1));
    result
}
