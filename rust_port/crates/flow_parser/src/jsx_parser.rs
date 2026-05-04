/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use dupe::OptionDupedExt;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::ast::StringLiteral;
use crate::ast::*;
use crate::ast_utils;
use crate::expression_parser;
use crate::loc::LOC_NONE;
use crate::loc::Loc;
use crate::main_parser;
use crate::parse_error::ParseError;
use crate::parser_common::with_loc;
use crate::parser_env;
use crate::parser_env::LexMode;
use crate::parser_env::ParserEnv;
use crate::parser_env::eat;
use crate::parser_env::expect;
use crate::parser_env::peek;
use crate::parser_env::try_parse::Rollback;
use crate::token::TokenKind;

/// Consumes and returns the trailing comments after the end of a JSX tag name,
/// attribute, or spread attribute.
///
/// If the component is followed by the end of the JSX tag, then all trailing
/// comments are returned. If the component is instead followed by another tag
/// component on another line, only trailing comments on the same line are
/// returned. If the component is followed by another tag component on the same
/// line, all trailing comments will instead be leading the next component.
fn tag_component_trailing_comments(env: &mut ParserEnv) -> Vec<Comment<Loc>> {
    if matches!(
        peek::token(env),
        TokenKind::TEof | TokenKind::TDiv | TokenKind::TGreaterThan
    ) {
        eat::trailing_comments(env)
    } else if peek::is_line_terminator(env) {
        eat::comments_until_next_line(env)
    } else {
        Vec::new()
    }
}

fn spread_attribute(env: &mut ParserEnv) -> Result<jsx::SpreadAttribute<Loc, Loc>, Rollback> {
    let leading = peek::comments(env);
    eat::push_lex_mode(env, LexMode::Normal);
    let (loc, argument) = with_loc(None, env, |env| {
        expect::token(env, TokenKind::TLcurly)?;
        expect::token(env, TokenKind::TEllipsis)?;
        let argument = expression_parser::assignment(env)?;
        expect::token(env, TokenKind::TRcurly)?;
        Ok(argument)
    })?;
    eat::pop_lex_mode(env);
    let trailing = tag_component_trailing_comments(env);
    Ok(jsx::SpreadAttribute {
        loc,
        argument,
        comments: ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into())),
    })
}

fn expression_container_contents(
    env: &mut ParserEnv,
) -> Result<jsx::expression_container::Expression<Loc, Loc>, Rollback> {
    if peek::token(env) == &TokenKind::TRcurly {
        Ok(jsx::expression_container::Expression::EmptyExpression)
    } else {
        let expr = main_parser::parse_expression(env)?;
        Ok(jsx::expression_container::Expression::Expression(expr))
    }
}

fn expression_container(
    env: &mut ParserEnv,
) -> Result<(Loc, jsx::ExpressionContainer<Loc, Loc>), Rollback> {
    let leading = peek::comments(env);
    eat::push_lex_mode(env, LexMode::Normal);
    let (loc, expression) = with_loc(None, env, |env| {
        expect::token(env, TokenKind::TLcurly)?;
        let expression = expression_container_contents(env)?;
        expect::token(env, TokenKind::TRcurly)?;
        Ok(expression)
    })?;
    eat::pop_lex_mode(env);
    let trailing = tag_component_trailing_comments(env);
    Ok((
        loc,
        jsx::ExpressionContainer {
            expression,
            comments: ast_utils::mk_comments_with_internal_opt(
                Some(leading.into()),
                Some(trailing.into()),
                Some(Arc::from([])),
            ),
        },
    ))
}

fn expression_container_or_spread_child(
    env: &mut ParserEnv,
) -> Result<jsx::Child<Loc, Loc>, Rollback> {
    eat::push_lex_mode(env, LexMode::Normal);
    let (loc, mut result) = with_loc(None, env, |env| {
        expect::token(env, TokenKind::TLcurly)?;
        let result = if peek::token(env) == &TokenKind::TEllipsis {
            let leading = peek::comments(env);
            expect::token(env, TokenKind::TEllipsis)?;
            let expression = expression_parser::assignment(env)?;
            jsx::Child::SpreadChild {
                loc: LOC_NONE,
                inner: jsx::SpreadChild {
                    expression,
                    comments: ast_utils::mk_comments_opt(Some(leading.into()), None),
                },
            }
        } else {
            let expression = expression_container_contents(env)?;
            let internal = match expression {
                jsx::expression_container::Expression::EmptyExpression => peek::comments(env),
                _ => Vec::new(),
            };
            jsx::Child::ExpressionContainer {
                loc: LOC_NONE,
                inner: jsx::ExpressionContainer {
                    expression,
                    comments: ast_utils::mk_comments_with_internal_opt(
                        None,
                        None,
                        Some(internal.into()),
                    ),
                },
            }
        };
        expect::token(env, TokenKind::TRcurly)?;
        Ok(result)
    })?;
    *result.loc_mut() = loc;
    eat::pop_lex_mode(env);
    Ok(result)
}

fn identifier(env: &mut ParserEnv) -> Result<jsx::Identifier<Loc, Loc>, Rollback> {
    let loc = peek::loc(env).dupe();
    let name = match peek::token(env) {
        TokenKind::TJsxIdentifier { raw, .. } => raw.to_owned(),
        _ => {
            env.error_unexpected(Some("an identifier".to_owned()))?;
            FlowSmolStr::new_inline("")
        }
    };
    let leading = peek::comments(env);
    eat::token(env)?;
    // Unless this identifier is the first part of a namespaced name, member
    // expression, or attribute name, it is the end of a tag component.
    let trailing = match peek::token(env) {
        // Namespaced name
        TokenKind::TColon
        // Member expression
        | TokenKind::TPeriod
        // Attribute name
        | TokenKind::TAssign => eat::trailing_comments(env),
        _ => tag_component_trailing_comments(env),
    };
    Ok(jsx::Identifier {
        loc,
        name,
        comments: ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into())),
    })
}

fn name(env: &mut ParserEnv) -> Result<jsx::Name<Loc, Loc>, Rollback> {
    fn member_expression(
        env: &mut ParserEnv,
        mut member: jsx::MemberExpression<Loc, Loc>,
    ) -> Result<jsx::MemberExpression<Loc, Loc>, Rollback> {
        loop {
            if peek::token(env) == &TokenKind::TPeriod {
                let start_loc = member.loc.dupe();
                let (loc, mut new_member) = with_loc(Some(start_loc), env, |env| {
                    expect::token(env, TokenKind::TPeriod)?;
                    let property = identifier(env)?;
                    Ok(jsx::MemberExpression {
                        loc: LOC_NONE,
                        object: jsx::member_expression::Object::MemberExpression(Arc::new(member)),
                        property,
                    })
                })?;
                new_member.loc = loc;
                member = new_member;
            } else {
                return Ok(member);
            }
        }
    }

    match peek::ith_token(env, 1) {
        TokenKind::TColon => {
            let (loc, mut name) = with_loc(None, env, |env| {
                let namespace = identifier(env)?;
                expect::token(env, TokenKind::TColon)?;
                let name = identifier(env)?;
                Ok(jsx::NamespacedName {
                    loc: LOC_NONE,
                    namespace,
                    name,
                })
            })?;
            name.loc = loc;
            Ok(jsx::Name::NamespacedName(name))
        }
        TokenKind::TPeriod => {
            let (loc, mut member) = with_loc(None, env, |env| {
                let object = jsx::member_expression::Object::Identifier(identifier(env)?);
                expect::token(env, TokenKind::TPeriod)?;
                let property = identifier(env)?;
                Ok(jsx::MemberExpression {
                    loc: LOC_NONE,
                    object,
                    property,
                })
            })?;
            member.loc = loc;
            Ok(jsx::Name::MemberExpression(member_expression(env, member)?))
        }
        _ => {
            let name = identifier(env)?;
            Ok(jsx::Name::Identifier(name))
        }
    }
}

fn names_are_equal(a: &jsx::Name<Loc, Loc>, b: &jsx::Name<Loc, Loc>) -> bool {
    fn identifiers_are_equal(a: &jsx::Identifier<Loc, Loc>, b: &jsx::Identifier<Loc, Loc>) -> bool {
        a.name == b.name
    }

    fn member_expressions_are_equal(
        a: &jsx::MemberExpression<Loc, Loc>,
        b: &jsx::MemberExpression<Loc, Loc>,
    ) -> bool {
        let objs_equal = match (&a.object, &b.object) {
            (
                jsx::member_expression::Object::Identifier(a),
                jsx::member_expression::Object::Identifier(b),
            ) => identifiers_are_equal(a, b),
            (
                jsx::member_expression::Object::MemberExpression(a),
                jsx::member_expression::Object::MemberExpression(b),
            ) => member_expressions_are_equal(a, b),
            _ => false,
        };
        objs_equal && identifiers_are_equal(&a.property, &b.property)
    }

    fn namespaced_names_are_equal(
        a: &jsx::NamespacedName<Loc, Loc>,
        b: &jsx::NamespacedName<Loc, Loc>,
    ) -> bool {
        identifiers_are_equal(&a.namespace, &b.namespace) && identifiers_are_equal(&a.name, &b.name)
    }

    match (a, b) {
        (jsx::Name::Identifier(a), jsx::Name::Identifier(b)) => identifiers_are_equal(a, b),
        (jsx::Name::MemberExpression(a), jsx::Name::MemberExpression(b)) => {
            member_expressions_are_equal(a, b)
        }
        (jsx::Name::NamespacedName(a), jsx::Name::NamespacedName(b)) => {
            namespaced_names_are_equal(a, b)
        }
        _ => false,
    }
}

fn attribute(env: &mut ParserEnv) -> Result<(Loc, jsx::Attribute<Loc, Loc>), Rollback> {
    with_loc(None, env, |env| {
        let name = match peek::ith_token(env, 1) {
            TokenKind::TColon => {
                let (loc, mut namespaced_name) = with_loc(None, env, |env| {
                    let namespace = identifier(env)?;
                    expect::token(env, TokenKind::TColon)?;
                    let name = identifier(env)?;
                    Ok(jsx::NamespacedName {
                        loc: LOC_NONE,
                        namespace,
                        name,
                    })
                })?;
                namespaced_name.loc = loc;
                jsx::attribute::Name::NamespacedName(namespaced_name)
            }
            _ => {
                let name = identifier(env)?;
                jsx::attribute::Name::Identifier(name)
            }
        };
        let value = if peek::token(env) == &TokenKind::TAssign {
            expect::token(env, TokenKind::TAssign)?;
            let leading = peek::comments(env);
            match peek::token(env) {
                TokenKind::TLcurly => {
                    let (loc, expression_container) = expression_container(env)?;
                    match expression_container.expression {
                        jsx::expression_container::Expression::EmptyExpression => {
                            env.error_at(loc.dupe(), ParseError::JSXAttributeValueEmptyExpression)?;
                        }
                        _ => {}
                    }
                    Some(jsx::attribute::Value::ExpressionContainer((
                        loc,
                        expression_container,
                    )))
                }
                TokenKind::TJsxQuoteText(loc, value, raw) => {
                    let loc = loc.dupe();
                    let value = value.dupe();
                    let raw = raw.dupe();
                    expect::token(
                        env,
                        TokenKind::TJsxQuoteText(loc.dupe(), value.dupe(), raw.dupe()),
                    )?;
                    let trailing = tag_component_trailing_comments(env);
                    let comments =
                        ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
                    Some(jsx::attribute::Value::StringLiteral((
                        loc,
                        StringLiteral {
                            value,
                            raw,
                            comments,
                        },
                    )))
                }
                _ => {
                    env.error(ParseError::InvalidJSXAttributeValue)?;
                    let loc = peek::loc(env).dupe();
                    Some(jsx::attribute::Value::StringLiteral((
                        loc,
                        StringLiteral {
                            value: FlowSmolStr::new_inline(""),
                            raw: FlowSmolStr::new_inline(""),
                            comments: None,
                        },
                    )))
                }
            }
        } else {
            None
        };
        Ok(jsx::Attribute {
            loc: LOC_NONE,
            name,
            value,
        })
    })
}

enum OpeningElementResult {
    Fragment(Loc),
    Element(jsx::Opening<Loc, Loc>),
}

impl OpeningElementResult {
    fn loc(&self) -> &Loc {
        match self {
            OpeningElementResult::Fragment(loc) => loc,
            OpeningElementResult::Element(opening) => &opening.loc,
        }
    }

    fn mod_loc(&mut self, new_loc: Loc) {
        match self {
            OpeningElementResult::Fragment(loc) => {
                *loc = new_loc;
            }
            OpeningElementResult::Element(opening) => {
                opening.loc = new_loc;
            }
        }
    }
}

fn opening_element(
    env: &mut ParserEnv,
) -> Result<Result<OpeningElementResult, OpeningElementResult>, Rollback> {
    fn attributes(
        env: &mut ParserEnv,
        mut acc: Vec<jsx::OpeningAttribute<Loc, Loc>>,
    ) -> Result<Vec<jsx::OpeningAttribute<Loc, Loc>>, Rollback> {
        loop {
            match peek::token(env) {
                TokenKind::TJsxIdentifier { .. } => {
                    let (loc, mut attr) = attribute(env)?;
                    attr.loc = loc;
                    acc.push(jsx::OpeningAttribute::Attribute(attr));
                }
                TokenKind::TLcurly => {
                    let spread_attr = spread_attribute(env)?;
                    acc.push(jsx::OpeningAttribute::SpreadAttribute(spread_attr));
                }
                _ => break,
            }
        }
        Ok(acc)
    }

    let (loc, mut result) = with_loc(None, env, |env| {
        expect::token(env, TokenKind::TLessThan)?;
        match peek::token(env) {
            TokenKind::TGreaterThan => {
                eat::token(env)?;
                Ok(Ok(OpeningElementResult::Fragment(LOC_NONE)))
            }
            TokenKind::TJsxIdentifier { .. } => {
                let jsx_name = name(env)?;
                let targs = if env.should_parse_types()
                    && peek::token(env) == &TokenKind::TLessThan
                    && peek::ith_token(env, 1) != &TokenKind::TDiv
                {
                    parser_env::try_parse::or_else(env, expression_parser::call_type_args, None)
                } else {
                    None
                };
                let attributes = attributes(env, Vec::new())?;
                let self_closing = eat::maybe(env, TokenKind::TDiv)?;
                let element = OpeningElementResult::Element(jsx::Opening {
                    loc: LOC_NONE,
                    name: jsx_name,
                    targs,
                    self_closing,
                    attributes: attributes.into(),
                });
                if eat::maybe(env, TokenKind::TGreaterThan)? {
                    Ok(Ok(element))
                } else {
                    expect::error(env, &TokenKind::TGreaterThan)?;
                    Ok(Err(element))
                }
            }
            _ => {
                expect::error(env, &TokenKind::TGreaterThan)?;
                Ok(Err(OpeningElementResult::Fragment(LOC_NONE)))
            }
        }
    })?;
    match &mut result {
        Ok(r) | Err(r) => r.mod_loc(loc),
    }
    Ok(result)
}

enum ClosingElementResult {
    Fragment(Loc),
    Element(jsx::Closing<Loc, Loc>),
}

fn closing_element(env: &mut ParserEnv) -> Result<ClosingElementResult, Rollback> {
    let (loc, name_opt) = with_loc(None, env, |env| {
        expect::token(env, TokenKind::TLessThan)?;
        expect::token(env, TokenKind::TDiv)?;
        match peek::token(env) {
            TokenKind::TGreaterThan => {
                eat::token(env)?;
                Ok(None)
            }
            TokenKind::TJsxIdentifier { .. } => {
                let jsx_name = name(env)?;
                expect::token_opt(env, TokenKind::TGreaterThan)?;
                Ok(Some(jsx_name))
            }
            _ => {
                expect::error(env, &TokenKind::TGreaterThan)?;
                Ok(None)
            }
        }
    })?;
    Ok(match name_opt {
        Some(jsx_name) => ClosingElementResult::Element(jsx::Closing {
            loc,
            name: jsx_name,
        }),
        None => ClosingElementResult::Fragment(loc),
    })
}

fn child_is_unpaired(opening_name: &jsx::Name<Loc, Loc>, child: &jsx::Child<Loc, Loc>) -> bool {
    match child {
        jsx::Child::Element {
            loc: _,
            inner:
                jsx::Element {
                    opening_element,
                    closing_element: Some(closing_element),
                    ..
                },
        } => {
            let child_opening_name = &opening_element.name;
            let child_closing_name = &closing_element.name;
            !names_are_equal(child_opening_name, child_closing_name)
                && names_are_equal(opening_name, child_closing_name)
        }
        _ => false,
    }
}

fn child(
    env: &mut ParserEnv,
    parent_opening_name: Option<&jsx::Name<Loc, Loc>>,
) -> Result<jsx::Child<Loc, Loc>, Rollback> {
    match peek::token(env) {
        TokenKind::TLcurly => expression_container_or_spread_child(env),
        TokenKind::TJsxChildText(loc, value, raw) => {
            let loc = loc.dupe();
            let value = value.to_owned();
            let raw = raw.to_owned();
            eat::token(env)?;
            Ok(jsx::Child::Text {
                loc,
                inner: jsx::Text { value, raw },
            })
        }
        _ => {
            let (loc, result) = element_or_fragment(env, parent_opening_name)?;
            match result {
                Ok(element) => Ok(jsx::Child::Element {
                    loc,
                    inner: element,
                }),
                Err(fragment) => Ok(jsx::Child::Fragment {
                    loc,
                    inner: fragment,
                }),
            }
        }
    }
}

enum ClosingResult {
    Element(jsx::Closing<Loc, Loc>),
    Fragment(Loc),
    None,
}

fn element(
    env: &mut ParserEnv,
    parent_opening_name: Option<&jsx::Name<Loc, Loc>>,
) -> Result<(Loc, Result<jsx::Element<Loc, Loc>, jsx::Fragment<Loc, Loc>>), Rollback> {
    fn children_and_closing_helper(
        env: &mut ParserEnv,
        opening_name: Option<&jsx::Name<Loc, Loc>>,
    ) -> Result<(Vec<jsx::Child<Loc, Loc>>, Option<Loc>, ClosingResult), Rollback> {
        let mut acc = Vec::new();
        loop {
            let previous_loc = env.last_loc().duped();

            if let Some(opening_name) = opening_name
                && let Some(last_child) =
                    acc.pop_if(|last_child| child_is_unpaired(opening_name, last_child))
            {
                // (* if the last child's opening and closing tags don't match, and the
                // child's closing tag matches ours, then we're in a situation like
                // <a><b></b><c></a>, where opening_name = a and the child has opening
                // tag c and closing tag a.
                //
                // steal the closing tag from the last child, so that <c> has no
                // closing tag, but <a>...</a> is properly paired.
                let closing = match last_child {
                    jsx::Child::Element {
                        loc,
                        inner:
                            jsx::Element {
                                opening_element,
                                closing_element: Some(closing),
                                children,
                                comments,
                            },
                    } => {
                        let new_loc = Loc::between(&loc, &children.0);
                        let last_child = jsx::Child::Element {
                            loc: new_loc,
                            inner: jsx::Element {
                                opening_element,
                                closing_element: None,
                                children,
                                comments,
                            },
                        };
                        acc.push(last_child);
                        ClosingResult::Element(closing)
                    }
                    _ => {
                        acc.push(last_child);
                        ClosingResult::None
                    }
                };
                eat::pop_lex_mode(env);
                return Ok((acc, previous_loc, closing));
            }

            match peek::token(env) {
                TokenKind::TLessThan => {
                    eat::push_lex_mode(env, LexMode::JsxTag);
                    let next_token = peek::ith_token(env, 1).clone();
                    match (peek::token(env), &next_token) {
                        (TokenKind::TLessThan, TokenKind::TEof)
                        | (TokenKind::TLessThan, TokenKind::TDiv) => {
                            let closing = match closing_element(env)? {
                                ClosingElementResult::Element(c) => ClosingResult::Element(c),
                                ClosingElementResult::Fragment(loc) => ClosingResult::Fragment(loc),
                            };
                            eat::double_pop_lex_mode(env);
                            return Ok((acc, previous_loc, closing));
                        }
                        _ => {
                            let (loc, result) = element(env, opening_name)?;
                            let child = match result {
                                Ok(element) => jsx::Child::Element {
                                    loc,
                                    inner: element,
                                },
                                Err(fragment) => jsx::Child::Fragment {
                                    loc,
                                    inner: fragment,
                                },
                            };
                            acc.push(child);
                        }
                    }
                }
                TokenKind::TEof => {
                    let _ = env.error_unexpected(None);
                    return Ok((acc, previous_loc, ClosingResult::None));
                }
                _ => {
                    let child_result = child(env, opening_name)?;
                    acc.push(child_result);
                }
            }
        }
    }

    fn children_and_closing(
        env: &mut ParserEnv,
        opening_name: Option<&jsx::Name<Loc, Loc>>,
    ) -> Result<((Loc, Vec<jsx::Child<Loc, Loc>>), ClosingResult), Rollback> {
        let start_loc = peek::loc(env).dupe();
        let (children, last_child_loc, closing) = children_and_closing_helper(env, opening_name)?;
        // It's a little bit tricky to untangle the parsing of the child elements from the parsing
        // of the closing element, so we can't easily use `with_loc` here. Instead, we'll use the
        // same logic that `with_loc` uses, but manipulate the locations explicitly.
        let children_loc = match last_child_loc {
            Some(l) => Loc::between(&start_loc, &l),
            None => start_loc,
        };
        Ok(((children_loc, children), closing))
    }

    fn normalize(name: &jsx::Name<Loc, Loc>) -> String {
        match name {
            jsx::Name::Identifier(id) => id.name.as_str().to_owned(),
            jsx::Name::NamespacedName(ns) => {
                format!("{}:{}", ns.namespace.name, ns.name.name)
            }
            jsx::Name::MemberExpression(me) => {
                fn normalize_object(obj: &jsx::member_expression::Object<Loc, Loc>) -> String {
                    match obj {
                        jsx::member_expression::Object::Identifier(id) => {
                            id.name.as_str().to_owned()
                        }
                        jsx::member_expression::Object::MemberExpression(me) => {
                            normalize(&jsx::Name::MemberExpression((**me).clone()))
                        }
                    }
                }
                format!("{}.{}", normalize_object(&me.object), me.property.name)
            }
        }
    }

    let leading = peek::comments(env);
    let opening_element = opening_element(env)?;
    eat::pop_lex_mode(env);
    let (children, closing_element) = if match &opening_element {
        Ok(OpeningElementResult::Element(e)) => e.self_closing,
        Ok(OpeningElementResult::Fragment(_)) => false,
        Err(_) => true,
    } {
        // If it's self closing,
        let (loc, _) = with_loc(None, env, |_| Ok(()))?;
        ((loc, Vec::new()), ClosingResult::None)
    } else {
        eat::push_lex_mode(env, LexMode::JsxChild);
        let opening_name = match &opening_element {
            Ok(OpeningElementResult::Element(e)) | Err(OpeningElementResult::Element(e)) => {
                Some(&e.name)
            }
            Ok(OpeningElementResult::Fragment(_)) | Err(OpeningElementResult::Fragment(_)) => None,
        };
        children_and_closing(env, opening_name)?
    };
    let trailing = eat::trailing_comments(env);
    let end_loc = match &closing_element {
        ClosingResult::Element(closing) => {
            match &opening_element {
                Ok(OpeningElementResult::Element(e)) => {
                    if !names_are_equal(&closing.name, &e.name) {
                        match parent_opening_name {
                            Some(parent_name) if names_are_equal(parent_name, &closing.name) => {
                                // the opening and closing tags don't match, but the closing
                                // tag matches the parent's opening tag. the parent is going
                                // to steal the closing tag away from this tag, so error on
                                // the opening tag instead.
                                env.error_at(
                                    e.name.loc().dupe(),
                                    ParseError::MissingJSXClosingTag(normalize(&e.name)),
                                )?;
                            }
                            _ => {
                                env.error_at(
                                    closing.name.loc().dupe(),
                                    ParseError::ExpectedJSXClosingTag(normalize(&e.name)),
                                )?;
                            }
                        }
                    }
                }
                Ok(OpeningElementResult::Fragment(_)) => {
                    env.error_at(
                        closing.name.loc().dupe(),
                        ParseError::ExpectedJSXClosingTag("JSX fragment".to_string()),
                    )?;
                }
                Err(_) => {}
            }
            closing.loc.dupe()
        }
        ClosingResult::Fragment(loc) => {
            match &opening_element {
                Ok(OpeningElementResult::Element(e)) => {
                    env.error_at(
                        loc.dupe(),
                        ParseError::ExpectedJSXClosingTag(normalize(&e.name)),
                    )?;
                }
                Ok(OpeningElementResult::Fragment(_)) | Err(_) => {}
            }
            loc.dupe()
        }
        ClosingResult::None => match &opening_element {
            Ok(r) | Err(r) => r.loc().dupe(),
        },
    };

    let result = match opening_element {
        Ok(OpeningElementResult::Element(e)) | Err(OpeningElementResult::Element(e)) => {
            Ok(jsx::Element {
                opening_element: e,
                closing_element: match closing_element {
                    ClosingResult::Element(c) => Some(c),
                    _ => None,
                },
                children,
                comments: ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into())),
            })
        }
        Ok(OpeningElementResult::Fragment(frag_loc))
        | Err(OpeningElementResult::Fragment(frag_loc)) => Err(jsx::Fragment {
            frag_opening_element: frag_loc,
            frag_closing_element: match closing_element {
                ClosingResult::Fragment(loc) => loc,
                ClosingResult::Element(c) => c.loc.dupe(),
                ClosingResult::None => end_loc.dupe(),
            },
            frag_children: children,
            frag_comments: ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into())),
        }),
    };

    let final_loc = Loc::between(
        match &result {
            Ok(e) => &e.opening_element.loc,
            Err(f) => &f.frag_opening_element,
        },
        &end_loc,
    );
    Ok((final_loc, result))
}

fn element_or_fragment(
    env: &mut ParserEnv,
    parent_opening_name: Option<&jsx::Name<Loc, Loc>>,
) -> Result<(Loc, Result<jsx::Element<Loc, Loc>, jsx::Fragment<Loc, Loc>>), Rollback> {
    eat::push_lex_mode(env, LexMode::JsxTag);
    element(env, parent_opening_name)
}

pub(super) fn parse_jsx_element_or_fragment(
    env: &mut ParserEnv,
) -> Result<(Loc, Result<jsx::Element<Loc, Loc>, jsx::Fragment<Loc, Loc>>), Rollback> {
    element_or_fragment(env, None)
}
