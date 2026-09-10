/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast::expression;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::pattern;
use flow_parser::loc::Loc;
use flow_parser_utils::ast_builder;

/// Placeholder for synthesized nodes that have not yet been given a real source position.
///
/// Line 1 column 0 is a *reachable* position, so a node carrying this is indistinguishable from
/// one genuinely at the top of the file — which is why it is being migrated out. `enum_lowering`
/// no longer uses it; `record_lowering` and `match_lowering` still do, and each call site that
/// passes it marks work remaining. `rg generated_loc` lists what is left.
pub fn generated_loc() -> Loc {
    Loc::mk(None, 1, 0, 1, 0)
}

pub fn identifier(loc: &Loc, name: &str) -> expression::Expression<Loc, Loc> {
    ast_builder::expressions::identifier(Some(loc.dupe()), None, name)
}

pub fn identifier_pattern(id: &ast::Identifier<Loc, Loc>) -> pattern::Pattern<Loc, Loc> {
    pattern::Pattern::Identifier {
        loc: id.loc.dupe(),
        inner: Arc::new(pattern::Identifier {
            name: id.dupe(),
            annot: ast::types::AnnotationOrHint::Missing(id.loc.dupe()),
            optional: false,
        }),
    }
}

pub fn string_literal(loc: &Loc, value: &str) -> expression::Expression<Loc, Loc> {
    ast_builder::string_literal_expression(Some(loc.dupe()), None, value)
}

pub fn call(
    loc: &Loc,
    callee: expression::Expression<Loc, Loc>,
    arguments: Vec<expression::Expression<Loc, Loc>>,
) -> expression::Expression<Loc, Loc> {
    let arguments = arguments
        .into_iter()
        .map(ast_builder::expressions::expression_or_spread)
        .collect();
    ast_builder::expressions::call(
        Some(loc.dupe()),
        Some(ast_builder::expressions::arg_list(
            Some(loc.dupe()),
            None,
            arguments,
        )),
        callee,
    )
}

pub fn member(
    loc: &Loc,
    object: expression::Expression<Loc, Loc>,
    property: &str,
) -> expression::Expression<Loc, Loc> {
    ast_builder::expressions::member(
        Some(loc.dupe()),
        ast_builder::expressions::members::identifier_by_name(None, property, object),
    )
}

pub fn expression_from_enum_member(
    member: &ast::statement::enum_declaration::Member<Loc>,
) -> Option<expression::Expression<Loc, Loc>> {
    use ast::statement::enum_declaration::Member;

    match member {
        Member::BooleanMember(member) => Some(expression::Expression::new(
            ExpressionInner::BooleanLiteral {
                loc: member.init.0.dupe(),
                inner: Arc::new(member.init.1.clone()),
            },
        )),
        Member::NumberMember(member) => Some(expression::Expression::new(
            ExpressionInner::NumberLiteral {
                loc: member.init.0.dupe(),
                inner: Arc::new(member.init.1.clone()),
            },
        )),
        Member::StringMember(member) => Some(expression::Expression::new(
            ExpressionInner::StringLiteral {
                loc: member.init.0.dupe(),
                inner: Arc::new(member.init.1.clone()),
            },
        )),
        Member::BigIntMember(member) => Some(expression::Expression::new(
            ExpressionInner::BigIntLiteral {
                loc: member.init.0.dupe(),
                inner: Arc::new(member.init.1.clone()),
            },
        )),
        Member::DefaultedMember(_) => None,
    }
}
