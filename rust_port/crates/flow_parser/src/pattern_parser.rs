/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;

use crate::ast::expression::ExpressionInner;
use crate::ast::*;
use crate::ast_utils;
use crate::expression_parser;
use crate::loc::Loc;
use crate::main_parser;
use crate::object_parser;
use crate::parse_error::ParseError;
use crate::parser_common::with_loc;
use crate::parser_env::ParserEnv;
use crate::parser_env::eat;
use crate::parser_env::expect;
use crate::parser_env::is_reserved;
use crate::parser_env::is_restricted;
use crate::parser_env::is_strict_reserved;
use crate::parser_env::peek;
use crate::parser_env::try_parse::Rollback;
use crate::token::TokenKind;
use crate::type_parser;

fn missing_annot(env: &ParserEnv) -> types::AnnotationOrHint<Loc, Loc> {
    types::AnnotationOrHint::Missing(peek::loc_skip_lookahead(env))
}

///  Reinterpret various expressions as patterns.
/// This is not the correct thing to do and is only used for assignment
/// expressions. This should be removed and replaced ASAP.
fn object_from_expr(
    env: &mut ParserEnv,
    loc: Loc,
    obj: expression::Object<Loc, Loc>,
) -> Result<pattern::Pattern<Loc, Loc>, Rollback> {
    fn object_properties_from_expr(
        env: &mut ParserEnv,
        properties: Vec<expression::object::Property<Loc, Loc>>,
    ) -> Result<Vec<pattern::object::Property<Loc, Loc>>, Rollback> {
        let len = properties.len();
        let mut result = Vec::with_capacity(len);
        for (i, prop) in properties.into_iter().enumerate() {
            match prop {
                expression::object::Property::NormalProperty(normal_property) => {
                    match normal_property {
                        expression::object::NormalProperty::Init {
                            loc,
                            key,
                            value,
                            shorthand,
                        } => {
                            let key = match key {
                                expression::object::Key::StringLiteral(lit) => {
                                    pattern::object::Key::StringLiteral(lit)
                                }
                                expression::object::Key::NumberLiteral(lit) => {
                                    pattern::object::Key::NumberLiteral(lit)
                                }
                                expression::object::Key::BigIntLiteral(lit) => {
                                    pattern::object::Key::BigIntLiteral(lit)
                                }
                                expression::object::Key::Identifier(id) => {
                                    pattern::object::Key::Identifier(id)
                                }
                                expression::object::Key::PrivateName(_) => {
                                    unreachable!("Internal Error: Found object private prop")
                                }
                                expression::object::Key::Computed(key) => {
                                    pattern::object::Key::Computed(key)
                                }
                            };

                            let (pattern, default) = match value.deref() {
                                ExpressionInner::Assignment { loc: _, inner }
                                    if inner.operator.is_none() =>
                                {
                                    (inner.left.clone(), Some(inner.right.clone()))
                                }
                                _ => (from_expr(env, value)?, None),
                            };

                            result.push(pattern::object::Property::NormalProperty(
                                pattern::object::NormalProperty {
                                    loc,
                                    key,
                                    pattern,
                                    default,
                                    shorthand,
                                },
                            ));
                        }
                        expression::object::NormalProperty::Method {
                            value: (loc, _), ..
                        } => {
                            env.error_at(loc.dupe(), ParseError::MethodInDestructuring)?;
                        }
                        expression::object::NormalProperty::Get {
                            loc: _,
                            key: _,
                            value: (loc, _),
                            comments: _,
                        }
                        | expression::object::NormalProperty::Set {
                            loc: _,
                            key: _,
                            value: (loc, _),
                            comments: _,
                        } => {
                            // these should never happen
                            env.error_at(
                                loc.dupe(),
                                ParseError::Unexpected("identifier".to_string()),
                            )?;
                        }
                    }
                }
                expression::object::Property::SpreadProperty(
                    expression::object::SpreadProperty {
                        loc,
                        argument,
                        comments,
                    },
                ) => {
                    if i + 1 == len {
                        result.push(pattern::object::Property::RestElement(
                            pattern::RestElement {
                                loc,
                                argument: from_expr(env, argument)?,
                                comments,
                            },
                        ));
                    } else {
                        env.error_at(loc, ParseError::PropertyAfterRestElement)?;
                    }
                }
            }
        }

        Ok(result)
    }

    let properties = object_properties_from_expr(env, obj.properties.to_vec())?;
    Ok(pattern::Pattern::Object {
        loc,
        inner: Arc::new(pattern::Object {
            properties: properties.into(),
            annot: missing_annot(env),
            comments: obj.comments,
        }),
    })
}

fn array_from_expr(
    env: &mut ParserEnv,
    loc: Loc,
    arr: expression::Array<Loc, Loc>,
) -> Result<pattern::Pattern<Loc, Loc>, Rollback> {
    // Convert an Expression to a Pattern if it is a valid
    // DestructuringAssignmentTarget, which must be an Object, Array or
    // IsValidSimpleAssignmentTarget.
    // #sec-destructuring-assignment-static-semantics-early-errors
    fn assignment_target(
        env: &mut ParserEnv,
        expr: expression::Expression<Loc, Loc>,
    ) -> Result<Option<pattern::Pattern<Loc, Loc>>, Rollback> {
        if expression_parser::is_assignable_lhs(&expr) {
            Ok(Some(from_expr(env, expr)?))
        } else {
            env.error_at(expr.loc().dupe(), ParseError::InvalidLHSInAssignment)?;
            Ok(None)
        }
    }

    fn array_elements_from_expr(
        env: &mut ParserEnv,
        elems: Vec<expression::ArrayElement<Loc, Loc>>,
    ) -> Result<Vec<pattern::array::Element<Loc, Loc>>, Rollback> {
        let len = elems.len();
        let mut result = Vec::with_capacity(len);

        for (i, elem) in elems.into_iter().enumerate() {
            match elem {
                expression::ArrayElement::Spread(expression::SpreadElement {
                    loc,
                    argument,
                    comments,
                }) => {
                    if i + 1 == len {
                        if let Some(argument) = assignment_target(env, argument)? {
                            result.push(pattern::array::Element::RestElement(
                                pattern::RestElement {
                                    loc,
                                    argument,
                                    comments,
                                },
                            ));
                        }
                    } else {
                        env.error_at(loc, ParseError::ElementAfterRestElement)?;
                    }
                }
                expression::ArrayElement::Expression(expr) => {
                    match expr.deref() {
                        ExpressionInner::Assignment { loc, inner, .. }
                            if inner.operator.is_none() =>
                        {
                            // AssignmentElement is a `DestructuringAssignmentTarget Initializer`
                            // see #prod-AssignmentElement
                            result.push(pattern::array::Element::NormalElement(
                                pattern::array::NormalElement {
                                    loc: loc.dupe(),
                                    argument: inner.left.clone(),
                                    default: Some(inner.right.clone()),
                                },
                            ));
                        }
                        _ => {
                            let elem_loc = expr.loc().dupe();
                            // AssignmentElement is a DestructuringAssignmentTarget.
                            // See #prod-AssignmentElement
                            if let Some(pattern) = assignment_target(env, expr.dupe())? {
                                result.push(pattern::array::Element::NormalElement(
                                    pattern::array::NormalElement {
                                        loc: elem_loc,
                                        argument: pattern,
                                        default: None,
                                    },
                                ));
                            }
                        }
                    }
                }
                expression::ArrayElement::Hole(loc) => {
                    result.push(pattern::array::Element::Hole(loc));
                }
            }
        }

        Ok(result)
    }

    let elements = array_elements_from_expr(env, arr.elements.to_vec())?;
    Ok(pattern::Pattern::Array {
        loc,
        inner: Arc::new(pattern::Array {
            elements: elements.into(),
            annot: missing_annot(env),
            comments: arr.comments,
        }),
    })
}

/// Reinterpret various expressions as patterns.
/// This is not the correct thing to do and is only used for assignment expressions.
pub(super) fn from_expr(
    env: &mut ParserEnv,
    expr: expression::Expression<Loc, Loc>,
) -> Result<pattern::Pattern<Loc, Loc>, Rollback> {
    let loc = expr.loc().dupe();
    match expr.deref() {
        ExpressionInner::Object { inner, .. } => object_from_expr(env, loc, (**inner).clone()),
        ExpressionInner::Array { inner, .. } => array_from_expr(env, loc, (**inner).clone()),
        ExpressionInner::Identifier { loc: _, inner } => {
            let IdentifierInner {
                loc: id_loc,
                name,
                comments,
            } = inner.deref();
            // per #sec-destructuring-assignment-static-semantics-early-errors,
            // it is a syntax error if IsValidSimpleAssignmentTarget of this
            // IdentifierReference is false. That happens when `name` is
            // "eval" or "arguments" in strict mode.
            if env.in_strict_mode() && is_restricted(name) {
                env.error_at(id_loc.dupe(), ParseError::StrictLHSAssignment)?;
            }
            // per #prod-IdentifierReference, yield is only a valid
            // IdentifierReference when [~Yield], and await is only valid
            // when [~Await]. but per #sec-identifiers-static-semantics-early-errors,
            // they are already invalid in strict mode, which we should have
            // already errored about when parsing the expression that we're now
            // converting into a pattern.
            else if !env.in_strict_mode() {
                if env.allow_yield() && name == "yield" {
                    env.error_at(id_loc.dupe(), ParseError::YieldAsIdentifierReference)?;
                } else if env.allow_await() && name == "await" {
                    env.error_at(id_loc.dupe(), ParseError::AwaitAsIdentifierReference)?;
                }
            }

            Ok(pattern::Pattern::Identifier {
                loc,
                inner: Arc::new(pattern::Identifier {
                    name: Identifier::new(IdentifierInner {
                        loc: id_loc.dupe(),
                        name: name.dupe(),
                        comments: comments.clone(),
                    }),
                    annot: missing_annot(env),
                    optional: false,
                }),
            })
        }
        _ => Ok(pattern::Pattern::Expression {
            loc,
            inner: Arc::new(expr),
        }),
    }
}

fn parse_object(
    env: &mut ParserEnv,
    restricted_error: ParseError,
) -> Result<pattern::Pattern<Loc, Loc>, Rollback> {
    fn parse_rest_property(
        env: &mut ParserEnv,
        restricted_error: ParseError,
    ) -> Result<pattern::object::Property<Loc, Loc>, Rollback> {
        let leading = peek::comments(env);
        let (loc, argument) = with_loc(None, env, |env| {
            expect::token(env, TokenKind::TEllipsis)?;
            pattern(env, false, restricted_error)
        })?;

        Ok(pattern::object::Property::RestElement(
            pattern::RestElement {
                loc,
                argument,
                comments: ast_utils::mk_comments_opt(Some(leading.into()), None),
            },
        ))
    }

    fn parse_property_default(
        env: &mut ParserEnv,
    ) -> Result<Option<expression::Expression<Loc, Loc>>, Rollback> {
        if peek::token(env) == &TokenKind::TAssign {
            expect::token(env, TokenKind::TAssign)?;
            Ok(Some(expression_parser::assignment(env)?))
        } else {
            Ok(None)
        }
    }

    fn parse_object_property(
        env: &mut ParserEnv,
        restricted_error: ParseError,
    ) -> Result<Option<pattern::object::Property<Loc, Loc>>, Rollback> {
        if peek::token(env) == &TokenKind::TEllipsis {
            return Ok(Some(parse_rest_property(env, restricted_error)?));
        }

        let start_loc = peek::loc(env).dupe();
        let raw_key = object_parser::key(env, false)?;

        match peek::token(env) {
            TokenKind::TColon => {
                expect::token(env, TokenKind::TColon)?;
                let (loc, (pattern, default)) = with_loc(Some(start_loc), env, |env| {
                    let pattern = pattern(env, false, restricted_error)?;
                    let default = parse_property_default(env)?;
                    Ok((pattern, default))
                })?;

                let key = match raw_key.1 {
                    expression::object::Key::StringLiteral(lit) => {
                        pattern::object::Key::StringLiteral(lit)
                    }
                    expression::object::Key::NumberLiteral(lit) => {
                        pattern::object::Key::NumberLiteral(lit)
                    }
                    expression::object::Key::BigIntLiteral(lit) => {
                        pattern::object::Key::BigIntLiteral(lit)
                    }
                    expression::object::Key::Identifier(id) => pattern::object::Key::Identifier(id),
                    expression::object::Key::PrivateName(_) => {
                        unreachable!("Internal Error: Found object private prop")
                    }
                    expression::object::Key::Computed(key) => pattern::object::Key::Computed(key),
                };

                Ok(Some(pattern::object::Property::NormalProperty(
                    pattern::object::NormalProperty {
                        loc,
                        key,
                        pattern,
                        default,
                        shorthand: false,
                    },
                )))
            }
            _ => {
                // Shorthand property
                match raw_key.1 {
                    expression::object::Key::Identifier(id) => {
                        // #sec-identifiers-static-semantics-early-errors
                        if is_reserved(&id.name) {
                            // it is a syntax error if `name` is a reserved word other than await or yield
                            env.error_at(id.loc.dupe(), ParseError::UnexpectedReserved)?;
                        } else if is_strict_reserved(&id.name) {
                            // it is a syntax error if `name` is a strict reserved word, in strict mode
                            env.strict_error_at((id.loc.dupe(), ParseError::StrictReservedWord))?;
                        }
                        if peek::token(env) == &TokenKind::TPling {
                            env.error(ParseError::UnexpectedOptional)?;
                            eat::token(env)?;
                        }

                        let (loc, (pattern, default)) = with_loc(Some(start_loc), env, |env| {
                            let pattern = pattern::Pattern::Identifier {
                                loc: id.loc.dupe(),
                                inner: Arc::new(pattern::Identifier {
                                    name: id.dupe(),
                                    annot: missing_annot(env),
                                    optional: false,
                                }),
                            };
                            let default = parse_property_default(env)?;
                            Ok((pattern, default))
                        })?;

                        Ok(Some(pattern::object::Property::NormalProperty(
                            pattern::object::NormalProperty {
                                loc,
                                key: pattern::object::Key::Identifier(id),
                                pattern,
                                default,
                                shorthand: true,
                            },
                        )))
                    }
                    _ => {
                        env.error_unexpected(Some("an identifier".to_string()))?;
                        // invalid shorthand destructuring
                        Ok(None)
                    }
                }
            }
        }
    }

    fn parse_object_properties(
        env: &mut ParserEnv,
        restricted_error: ParseError,
    ) -> Result<Vec<pattern::object::Property<Loc, Loc>>, Rollback> {
        let mut properties = Vec::new();
        // seen_rest is true when we've seen a rest element. rest_trailing_comma is the location of
        // the rest element's trailing command
        let mut seen_rest = false;
        // Trailing comma: `let { ...rest, } = obj`
        // Still invalid, but not a trailing comma: `let { ...rest, x } = obj`
        let mut rest_trailing_comma: Option<Loc> = None;

        loop {
            match peek::token(env) {
                TokenKind::TEof | TokenKind::TRcurly => {
                    if let Some(loc) = rest_trailing_comma {
                        env.error_at(loc, ParseError::TrailingCommaAfterRestElement)?;
                    }
                    return Ok(properties);
                }
                _ => {
                    if let Some(prop) = parse_object_property(env, restricted_error.clone())? {
                        if seen_rest {
                            let prop_loc = match &prop {
                                pattern::object::Property::NormalProperty(p) => p.loc.dupe(),
                                pattern::object::Property::RestElement(p) => p.loc.dupe(),
                            };
                            env.error_at(prop_loc, ParseError::PropertyAfterRestElement)?;
                            rest_trailing_comma = None;
                        }

                        match &prop {
                            pattern::object::Property::RestElement(_) => {
                                seen_rest = true;
                                rest_trailing_comma = if peek::token(env) == &TokenKind::TComma {
                                    Some(peek::loc(env).dupe())
                                } else {
                                    None
                                };
                            }
                            _ => {}
                        }

                        if peek::token(env) != &TokenKind::TRcurly {
                            expect::token(env, TokenKind::TComma)?;
                        }

                        properties.push(prop);
                    }
                }
            }
        }
    }

    let (loc, obj_pattern) = with_loc(None, env, |env| {
        let leading = peek::comments(env);
        expect::token(env, TokenKind::TLcurly)?;

        let properties = parse_object_properties(env, restricted_error)?;

        let internal = peek::comments(env);
        expect::token(env, TokenKind::TRcurly)?;
        let trailing = eat::trailing_comments(env);

        let annot = if peek::token(env) == &TokenKind::TColon {
            types::AnnotationOrHint::Available(type_parser::parse_annotation(env)?)
        } else {
            missing_annot(env)
        };

        Ok(pattern::Object {
            properties: properties.into(),
            annot,
            comments: ast_utils::mk_comments_with_internal_opt(
                Some(leading.into()),
                Some(trailing.into()),
                Some(internal.into()),
            ),
        })
    })?;
    Ok(pattern::Pattern::Object {
        loc,
        inner: Arc::new(obj_pattern),
    })
}

fn parse_array(
    env: &mut ParserEnv,
    restricted_error: ParseError,
) -> Result<pattern::Pattern<Loc, Loc>, Rollback> {
    fn parse_array_elements(
        env: &mut ParserEnv,
        restricted_error: ParseError,
    ) -> Result<Vec<pattern::array::Element<Loc, Loc>>, Rollback> {
        let mut elements = Vec::new();

        loop {
            match peek::token(env) {
                TokenKind::TEof | TokenKind::TRbracket => {
                    return Ok(elements);
                }
                TokenKind::TComma => {
                    let loc = peek::loc(env).dupe();
                    expect::token(env, TokenKind::TComma)?;
                    elements.push(pattern::array::Element::Hole(loc));
                }
                TokenKind::TEllipsis => {
                    let leading = peek::comments(env);
                    let (loc, argument) = with_loc(None, env, |env| {
                        expect::token(env, TokenKind::TEllipsis)?;
                        pattern(env, false, restricted_error.clone())
                    })?;

                    let element = pattern::array::Element::RestElement(pattern::RestElement {
                        loc: loc.dupe(),
                        argument,
                        comments: ast_utils::mk_comments_opt(Some(leading.into()), None),
                    });

                    // rest elements are always last, the closing ] should be next. but if not,
                    // error and keep going so we recover gracefully by parsing the rest of the
                    // elements.
                    if peek::token(env) != &TokenKind::TRbracket {
                        env.error_at(loc, ParseError::ElementAfterRestElement)?;
                        if peek::token(env) == &TokenKind::TComma {
                            eat::token(env)?;
                        }
                    }

                    elements.push(element);
                }
                _ => {
                    let (loc, (pattern, default)) = with_loc(None, env, |env| {
                        let pattern = pattern(env, false, restricted_error.clone())?;
                        let default = if peek::token(env) == &TokenKind::TAssign {
                            expect::token(env, TokenKind::TAssign)?;
                            Some(expression_parser::assignment(env)?)
                        } else {
                            None
                        };
                        Ok((pattern, default))
                    })?;

                    let element =
                        pattern::array::Element::NormalElement(pattern::array::NormalElement {
                            loc,
                            argument: pattern,
                            default,
                        });

                    if peek::token(env) != &TokenKind::TRbracket {
                        expect::token(env, TokenKind::TComma)?;
                    }

                    elements.push(element);
                }
            }
        }
    }

    let (loc, arr_pattern) = with_loc(None, env, |env| {
        let leading = peek::comments(env);
        expect::token(env, TokenKind::TLbracket)?;

        let elements = parse_array_elements(env, restricted_error)?;

        let internal = peek::comments(env);
        expect::token(env, TokenKind::TRbracket)?;

        let annot = if peek::token(env) == &TokenKind::TColon {
            types::AnnotationOrHint::Available(type_parser::parse_annotation(env)?)
        } else {
            missing_annot(env)
        };

        let trailing = eat::trailing_comments(env);
        let comments = ast_utils::mk_comments_with_internal_opt(
            Some(leading.into()),
            Some(trailing.into()),
            Some(internal.into()),
        );

        Ok(pattern::Array {
            elements: elements.into(),
            annot,
            comments,
        })
    })?;
    Ok(pattern::Pattern::Array {
        loc,
        inner: Arc::new(arr_pattern),
    })
}

pub(super) fn pattern(
    env: &mut ParserEnv,
    allow_optional: bool,
    restricted_error: ParseError,
) -> Result<pattern::Pattern<Loc, Loc>, Rollback> {
    match peek::token(env) {
        TokenKind::TLcurly => parse_object(env, restricted_error),
        TokenKind::TLbracket => parse_array(env, restricted_error),
        _ => {
            let (loc, id) = main_parser::parse_identifier_with_type(
                env,
                allow_optional,
                Some(restricted_error),
            )?;
            Ok(pattern::Pattern::Identifier {
                loc,
                inner: Arc::new(id),
            })
        }
    }
}
