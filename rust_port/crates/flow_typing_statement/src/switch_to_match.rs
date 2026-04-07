/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Switch-to-match conversion utilities.
//!
//! Ported from flow/src/typing/switch_to_match.ml

use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::VariableKind;
use flow_parser::ast::expression;
use flow_parser::ast::match_pattern;
use flow_parser::ast::statement;

struct UnableToConvertSwitch;

fn pattern_of_expression<M: Dupe>(
    placeholder_loc: &M,
    expr: &expression::Expression<M, M>,
) -> Result<match_pattern::MatchPattern<M, M>, UnableToConvertSwitch> {
    match expr.deref() {
        expression::ExpressionInner::NumberLiteral { loc, inner } => {
            Ok(match_pattern::MatchPattern::NumberPattern {
                loc: loc.dupe(),
                inner: Box::new(inner.as_ref().clone()),
            })
        }
        expression::ExpressionInner::BigIntLiteral { loc, inner } => {
            Ok(match_pattern::MatchPattern::BigIntPattern {
                loc: loc.dupe(),
                inner: Box::new(inner.as_ref().clone()),
            })
        }
        expression::ExpressionInner::StringLiteral { loc, inner } => {
            Ok(match_pattern::MatchPattern::StringPattern {
                loc: loc.dupe(),
                inner: Box::new(inner.as_ref().clone()),
            })
        }
        expression::ExpressionInner::BooleanLiteral { loc, inner } => {
            Ok(match_pattern::MatchPattern::BooleanPattern {
                loc: loc.dupe(),
                inner: Box::new(inner.as_ref().clone()),
            })
        }
        expression::ExpressionInner::NullLiteral { loc, inner } => {
            Ok(match_pattern::MatchPattern::NullPattern {
                loc: loc.dupe(),
                inner: Box::new((**inner).clone()),
            })
        }
        expression::ExpressionInner::Identifier { loc, inner } => {
            Ok(match_pattern::MatchPattern::IdentifierPattern {
                loc: loc.dupe(),
                inner: Box::new(inner.dupe()),
            })
        }
        expression::ExpressionInner::Unary { inner, .. } => {
            let operator = &inner.operator;
            let argument = &inner.argument;
            let comments = &inner.comments;
            match (operator, argument.deref()) {
                (
                    expression::UnaryOperator::Plus,
                    expression::ExpressionInner::NumberLiteral { loc, inner: lit },
                ) => Ok(match_pattern::MatchPattern::UnaryPattern {
                    loc: placeholder_loc.dupe(),
                    inner: Arc::new(match_pattern::UnaryPattern {
                        operator: match_pattern::unary_pattern::Operator::Plus,
                        argument: (
                            loc.dupe(),
                            match_pattern::unary_pattern::Argument::NumberLiteral(
                                lit.as_ref().clone(),
                            ),
                        ),
                        comments: comments.dupe(),
                    }),
                }),
                (
                    expression::UnaryOperator::Minus,
                    expression::ExpressionInner::NumberLiteral { loc, inner: lit },
                ) => Ok(match_pattern::MatchPattern::UnaryPattern {
                    loc: placeholder_loc.dupe(),
                    inner: Arc::new(match_pattern::UnaryPattern {
                        operator: match_pattern::unary_pattern::Operator::Minus,
                        argument: (
                            loc.dupe(),
                            match_pattern::unary_pattern::Argument::NumberLiteral(
                                lit.as_ref().clone(),
                            ),
                        ),
                        comments: comments.dupe(),
                    }),
                }),
                (
                    expression::UnaryOperator::Minus,
                    expression::ExpressionInner::BigIntLiteral { loc, inner: lit },
                ) => Ok(match_pattern::MatchPattern::UnaryPattern {
                    loc: placeholder_loc.dupe(),
                    inner: Arc::new(match_pattern::UnaryPattern {
                        operator: match_pattern::unary_pattern::Operator::Minus,
                        argument: (
                            loc.dupe(),
                            match_pattern::unary_pattern::Argument::BigIntLiteral(
                                lit.as_ref().clone(),
                            ),
                        ),
                        comments: comments.dupe(),
                    }),
                }),
                _ => Err(UnableToConvertSwitch),
            }
        }
        expression::ExpressionInner::Member { inner, .. } => {
            fn convert_member<M: Dupe>(
                placeholder_loc: &M,
                member: &expression::Member<M, M>,
            ) -> Result<match_pattern::MemberPattern<M, M>, UnableToConvertSwitch> {
                let object = &member.object;
                let property = &member.property;
                let comments = &member.comments;
                let match_property = match property {
                    expression::member::Property::PropertyIdentifier(id) => {
                        match_pattern::member_pattern::Property::PropertyIdentifier(id.dupe())
                    }
                    expression::member::Property::PropertyExpression(expr)
                        if let expression::ExpressionInner::StringLiteral { loc, inner: lit } =
                            expr.deref() =>
                    {
                        match_pattern::member_pattern::Property::PropertyString {
                            loc: loc.dupe(),
                            literal: lit.as_ref().clone(),
                        }
                    }
                    expression::member::Property::PropertyExpression(expr)
                        if let expression::ExpressionInner::NumberLiteral { loc, inner: lit } =
                            expr.deref() =>
                    {
                        match_pattern::member_pattern::Property::PropertyNumber {
                            loc: loc.dupe(),
                            literal: lit.as_ref().clone(),
                        }
                    }
                    expression::member::Property::PropertyExpression(expr)
                        if let expression::ExpressionInner::BigIntLiteral { loc, inner: lit } =
                            expr.deref() =>
                    {
                        match_pattern::member_pattern::Property::PropertyBigInt {
                            loc: loc.dupe(),
                            literal: lit.as_ref().clone(),
                        }
                    }
                    _ => return Err(UnableToConvertSwitch),
                };

                let base = match object.deref() {
                    expression::ExpressionInner::Identifier { inner: id, .. } => {
                        match_pattern::member_pattern::Base::BaseIdentifier(id.dupe())
                    }
                    expression::ExpressionInner::Member { inner: member, .. } => {
                        match_pattern::member_pattern::Base::BaseMember(Arc::new(convert_member(
                            placeholder_loc,
                            member,
                        )?))
                    }
                    _ => return Err(UnableToConvertSwitch),
                };
                Ok(match_pattern::MemberPattern {
                    loc: placeholder_loc.dupe(),
                    base,
                    property: match_property,
                    comments: comments.dupe(),
                })
            }
            let mem = convert_member(placeholder_loc, inner)?;
            Ok(match_pattern::MatchPattern::MemberPattern {
                loc: placeholder_loc.dupe(),
                inner: Arc::new(mem),
            })
        }
        _ => Err(UnableToConvertSwitch),
    }
}

fn is_block_scope_variable<M: Dupe>(stmt: &statement::Statement<M, M>) -> bool {
    match stmt.deref() {
        statement::StatementInner::VariableDeclaration { inner, .. } => {
            matches!(inner.kind, VariableKind::Let | VariableKind::Const)
        }
        _ => false,
    }
}

#[derive(Clone)]
enum OutputKind {
    StatementOutput,
    ReturnOutput,
    AssignOutput { name: FlowSmolStr },
    UnknownOutput,
}

fn assign_output_kind(name: FlowSmolStr, kind: &OutputKind) -> OutputKind {
    match kind {
        OutputKind::UnknownOutput => OutputKind::AssignOutput { name },
        OutputKind::AssignOutput {
            name: existing_name,
        } if *existing_name == name => OutputKind::AssignOutput { name },
        _ => OutputKind::StatementOutput,
    }
}

fn convert_switch_inner<M: Dupe>(
    placeholder_loc: &M,
    loc: M,
    switch: &statement::Switch<M, M>,
) -> Result<statement::StatementInner<M, M>, UnableToConvertSwitch> {
    let discriminant = &switch.discriminant;
    let cases = &switch.cases;
    let comments = &switch.comments;

    let num_cases = cases.len();

    let mut cases_acc: Vec<ast::match_::Case<M, M, statement::StatementInner<M, M>>> = Vec::new();
    let mut patterns_acc: Vec<match_pattern::MatchPattern<M, M>> = Vec::new();
    let mut output_kind = OutputKind::UnknownOutput;

    for (i, case) in cases.iter().enumerate() {
        let last_case = i + 1 == num_cases;
        let test = &case.test;
        let consequent = &case.consequent;
        let case_comments = &case.comments;
        // We only allow the wildcard at the end.
        let pattern = match test {
            None if !last_case => return Err(UnableToConvertSwitch),
            None => match_pattern::MatchPattern::WildcardPattern {
                loc: placeholder_loc.dupe(),
                inner: Box::new(match_pattern::WildcardPattern {
                    comments: case_comments.dupe(),
                    invalid_syntax_default_keyword: false,
                }),
            },
            Some(expr) => pattern_of_expression(placeholder_loc, expr)?,
        };
        let is_empty_consequent = consequent.is_empty()
            || (consequent.len() == 1
                && matches!(&*consequent[0], statement::StatementInner::Empty { .. }));
        if is_empty_consequent && !last_case {
            // There was no `case` body, so we accumulate patterns to build up a future "or" pattern.
            patterns_acc.push(pattern);
            continue;
        }
        let stmts = consequent;
        let body_stmts: &[statement::Statement<M, M>] = if stmts.len() == 1
            && let statement::StatementInner::Block { inner, .. } = &*stmts[0]
        {
            &inner.body
        } else {
            if stmts.iter().any(is_block_scope_variable) {
                return Err(UnableToConvertSwitch);
            }
            stmts
        };
        let body_stmts_vec: Vec<&statement::Statement<M, M>> = body_stmts.iter().collect();
        let (final_stmts, new_output_kind): (Vec<statement::Statement<M, M>>, OutputKind) =
            if body_stmts.is_empty() {
                if last_case {
                    // We allow empty last cases.
                    (stmts.to_vec(), OutputKind::StatementOutput)
                } else {
                    // Valid empty cases already handled above, we don't handle the rest.
                    return Err(UnableToConvertSwitch);
                }
            } else {
                let last_stmt = &body_stmts_vec[body_stmts_vec.len() - 1];
                match last_stmt.deref().deref() {
                    // `return expr;`
                    statement::StatementInner::Return { inner, .. }
                        if inner.argument.is_some() && body_stmts_vec.len() == 1 =>
                    {
                        let new_ok = match output_kind {
                            OutputKind::UnknownOutput | OutputKind::ReturnOutput => {
                                OutputKind::ReturnOutput
                            }
                            _ => OutputKind::StatementOutput,
                        };
                        // Single return-with-arg statement body.
                        // This could be turned into expression that is returned.
                        (body_stmts.to_vec(), new_ok)
                    }
                    // `name = expr; break;`
                    statement::StatementInner::Break { inner, .. }
                        if inner.label.is_none()
                            && body_stmts_vec.len() == 2
                            && let assignment_stmt = &body_stmts_vec[body_stmts_vec.len() - 2]
                            && let statement::StatementInner::Expression {
                                inner: expr_stmt,
                                ..
                            } = assignment_stmt.deref().deref()
                            && let expression::ExpressionInner::Assignment {
                                inner: assign,
                                ..
                            } = expr_stmt.expression.deref()
                            && assign.operator.is_none()
                            && let ast::pattern::Pattern::Identifier { inner: id, .. } =
                                &assign.left =>
                    {
                        // Single assignment followed by break as the statement body.
                        // This could be turned into expression that is assigned.
                        let name = id.name.name.dupe();
                        let new_ok = assign_output_kind(name, &output_kind);
                        (vec![(**assignment_stmt).dupe()], new_ok)
                    }
                    // (* `name = expr;` when the last case *)
                    // Check for single assignment without break (last case only)
                    _ if body_stmts_vec.len() == 1
                        && last_case
                        && let statement::StatementInner::Expression {
                            inner: expr_stmt, ..
                        } = last_stmt.deref().deref()
                        && let expression::ExpressionInner::Assignment {
                            inner: assign, ..
                        } = expr_stmt.expression.deref()
                        && assign.operator.is_none()
                        && let ast::pattern::Pattern::Identifier { inner: id, .. } =
                            &assign.left =>
                    {
                        // Single assignment statement body, no break but it's the last case.
                        // This could be turned into expression that is assigned.
                        let name = id.name.name.dupe();
                        let new_ok = assign_output_kind(name, &output_kind);
                        (body_stmts.to_vec(), new_ok)
                    }
                    statement::StatementInner::Break { inner, .. } if inner.label.is_none() => {
                        // Drop that last `break`.
                        let without_break: Vec<_> = body_stmts[..body_stmts.len() - 1].to_vec();
                        (without_break, OutputKind::StatementOutput)
                    }
                    // `return` | `throw`
                    statement::StatementInner::Return { .. }
                    | statement::StatementInner::Throw { .. } => {
                        (body_stmts.to_vec(), OutputKind::StatementOutput)
                    }
                    _ if last_case => {
                        // It is OK if the last case doesn't exit.
                        (body_stmts.to_vec(), OutputKind::StatementOutput)
                    }
                    _ => {
                        // We don't support cases that don't end with an exit otherwise.
                        return Err(UnableToConvertSwitch);
                    }
                }
            };
        output_kind = new_output_kind;
        let body = statement::StatementInner::Block {
            loc: placeholder_loc.dupe(),
            inner: Arc::new(statement::Block {
                body: final_stmts.into(),
                comments: None,
            }),
        };
        let final_pattern = if patterns_acc.is_empty() {
            pattern
        } else {
            patterns_acc.push(pattern);
            let all_patterns = std::mem::take(&mut patterns_acc);
            match_pattern::MatchPattern::OrPattern {
                loc: placeholder_loc.dupe(),
                inner: Arc::new(match_pattern::OrPattern {
                    patterns: all_patterns.into(),
                    comments: None,
                }),
            }
        };
        let match_case = ast::match_::Case {
            loc: placeholder_loc.dupe(),
            pattern: final_pattern,
            body,
            guard: None,
            comments: case_comments.dupe(),
            invalid_syntax: ast::match_::InvalidSyntax {
                invalid_prefix_case: None,
                invalid_infix_colon: None,
                invalid_suffix_semicolon: None,
            },
            case_match_root_loc: placeholder_loc.dupe(),
        };
        cases_acc.push(match_case);
    }
    if !patterns_acc.is_empty() {
        // If we ended with patterns that were never added to a case, give up.
        return Err(UnableToConvertSwitch);
    }
    fn stmts_of_block<M: Dupe>(
        stmt: &statement::StatementInner<M, M>,
    ) -> Result<&[statement::Statement<M, M>], UnableToConvertSwitch> {
        match stmt {
            statement::StatementInner::Block { inner, .. } => Ok(&inner.body),
            _ => Err(UnableToConvertSwitch),
        }
    }
    match output_kind {
        OutputKind::ReturnOutput => {
            // We can turn this into a return of a match expression.
            let match_cases: Result<Vec<_>, _> = cases_acc
                .into_iter()
                .map(|case| {
                    let body_stmts = stmts_of_block(&case.body)?;
                    let body_expr = if body_stmts.len() == 1
                        && let statement::StatementInner::Return { inner, .. } =
                            body_stmts[0].deref()
                        && let Some(ref expr) = inner.argument
                    {
                        expr.dupe()
                    } else {
                        return Err(UnableToConvertSwitch);
                    };
                    Ok(ast::match_::Case {
                        loc: case.loc,
                        pattern: case.pattern,
                        body: body_expr,
                        guard: case.guard,
                        comments: case.comments,
                        invalid_syntax: case.invalid_syntax,
                        case_match_root_loc: case.case_match_root_loc,
                    })
                })
                .collect();
            let match_cases = match_cases?;
            let match_expr = expression::Expression::new(expression::ExpressionInner::Match {
                loc: placeholder_loc.dupe(),
                inner: Arc::new(ast::match_::Match {
                    arg: discriminant.dupe(),
                    cases: match_cases.into(),
                    match_keyword_loc: placeholder_loc.dupe(),
                    comments: None,
                }),
            });
            Ok(statement::StatementInner::Return {
                loc,
                inner: Arc::new(statement::Return {
                    argument: Some(match_expr),
                    comments: comments.dupe(),
                    return_out: placeholder_loc.dupe(),
                }),
            })
        }
        OutputKind::AssignOutput { name } => {
            // We can turn this into an assign of a match expression.
            let match_cases: Result<Vec<_>, _> = cases_acc
                .into_iter()
                .map(|case| {
                    let body_stmts = stmts_of_block(&case.body)?;
                    let body_expr = if body_stmts.len() == 1
                        && let statement::StatementInner::Expression { inner, .. } =
                            body_stmts[0].deref()
                        && let expression::ExpressionInner::Assignment { inner: assign, .. } =
                            inner.expression.deref()
                        && assign.operator.is_none()
                    {
                        assign.right.dupe()
                    } else {
                        return Err(UnableToConvertSwitch);
                    };
                    Ok(ast::match_::Case {
                        loc: case.loc,
                        pattern: case.pattern,
                        body: body_expr,
                        guard: case.guard,
                        comments: case.comments,
                        invalid_syntax: case.invalid_syntax,
                        case_match_root_loc: case.case_match_root_loc,
                    })
                })
                .collect();
            let match_cases = match_cases?;
            let match_expr = expression::Expression::new(expression::ExpressionInner::Match {
                loc: placeholder_loc.dupe(),
                inner: Arc::new(ast::match_::Match {
                    arg: discriminant.dupe(),
                    cases: match_cases.into(),
                    match_keyword_loc: placeholder_loc.dupe(),
                    comments: None,
                }),
            });
            Ok(statement::StatementInner::Expression {
                loc,
                inner: Arc::new(statement::Expression {
                    expression: expression::Expression::new(
                        expression::ExpressionInner::Assignment {
                            loc: placeholder_loc.dupe(),
                            inner: Arc::new(expression::Assignment {
                                operator: None,
                                left: ast::pattern::Pattern::Identifier {
                                    loc: placeholder_loc.dupe(),
                                    inner: Arc::new(ast::pattern::Identifier {
                                        name: ast::Identifier::new(ast::IdentifierInner {
                                            loc: placeholder_loc.dupe(),
                                            name,
                                            comments: None,
                                        }),
                                        annot: ast::types::AnnotationOrHint::Missing(
                                            placeholder_loc.dupe(),
                                        ),
                                        optional: false,
                                    }),
                                },
                                right: match_expr,
                                comments: None,
                            }),
                        },
                    ),
                    directive: None,
                    comments: comments.dupe(),
                }),
            })
        }
        OutputKind::UnknownOutput | OutputKind::StatementOutput => {
            let match_cases: Vec<ast::match_::Case<M, M, statement::Statement<M, M>>> = cases_acc
                .into_iter()
                .map(|case| ast::match_::Case {
                    loc: case.loc,
                    pattern: case.pattern,
                    body: statement::Statement::new(case.body),
                    guard: case.guard,
                    comments: case.comments,
                    invalid_syntax: case.invalid_syntax,
                    case_match_root_loc: case.case_match_root_loc,
                })
                .collect();
            Ok(statement::StatementInner::Match {
                loc,
                inner: Arc::new(ast::match_::Match {
                    arg: discriminant.dupe(),
                    cases: match_cases.into(),
                    match_keyword_loc: placeholder_loc.dupe(),
                    comments: comments.dupe(),
                }),
            })
        }
    }
}

pub fn convert_switch<M: Dupe>(
    placeholder_loc: &M,
    loc: M,
    switch: &statement::Switch<M, M>,
) -> Option<statement::StatementInner<M, M>> {
    convert_switch_inner(placeholder_loc, loc, switch).ok()
}
