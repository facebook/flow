/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;

use dupe::Dupe;

use crate::ast::expression::ExpressionInner;
use crate::ast::*;
use crate::expression_parser;
use crate::loc::Loc;
use crate::parse_error::ParseError;
use crate::parser_env::try_parse::Rollback;
use crate::parser_env::*;
use crate::pattern_parser;

#[derive(Debug, Clone)]
pub(super) enum PatternCover {
    CoverExpr(expression::Expression<Loc, Loc>),
    CoverPatt(expression::Expression<Loc, Loc>, PatternCoverErrors),
}

#[derive(Debug, Clone)]
pub(super) struct PatternCoverErrors {
    pub(super) if_expr: Vec<(Loc, ParseError)>,
    pub(super) if_patt: Vec<(Loc, ParseError)>,
}

const EMPTY_ERRORS: PatternCoverErrors = PatternCoverErrors {
    if_expr: Vec::new(),
    if_patt: Vec::new(),
};

impl PatternCoverErrors {
    pub(super) fn empty() -> Self {
        EMPTY_ERRORS
    }

    pub(super) fn append(&mut self, other: Self) {
        let Self {
            if_expr: mut other_if_expr,
            if_patt: mut other_if_patt,
        } = other;
        self.if_expr.append(&mut other_if_expr);
        self.if_patt.append(&mut other_if_patt);
    }
}

pub(super) fn as_expression(
    env: &mut ParserEnv,
    cover: PatternCover,
) -> Result<expression::Expression<Loc, Loc>, Rollback> {
    match cover {
        PatternCover::CoverExpr(expression) => Ok(expression),
        PatternCover::CoverPatt(expression, pattern_errors) => {
            for (loc, error) in pattern_errors.if_expr {
                env.error_at(loc, error)?;
            }
            Ok(expression)
        }
    }
}

pub(super) fn as_pattern(
    env: &mut ParserEnv,
    error: Option<ParseError>,
    cover: PatternCover,
) -> Result<pattern::Pattern<Loc, Loc>, Rollback> {
    let expr = match cover {
        PatternCover::CoverExpr(expression) => expression,
        PatternCover::CoverPatt(expression, pattern_errors) => {
            for (loc, error) in pattern_errors.if_patt {
                env.error_at(loc, error)?;
            }
            expression
        }
    };
    if !expression_parser::is_assignable_lhs(&expr) {
        env.error_at(
            expr.loc().dupe(),
            error.unwrap_or(ParseError::InvalidLHSInAssignment),
        )?;
    }

    match expr.deref() {
        ExpressionInner::Identifier { loc, inner } if is_reserved(&inner.name) => {
            env.strict_error_at((loc.dupe(), ParseError::StrictLHSAssignment))?
        }
        _ => {}
    }

    pattern_parser::from_expr(env, expr)
}
