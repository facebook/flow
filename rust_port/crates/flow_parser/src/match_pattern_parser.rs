/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::ast::BigIntLiteral;
use crate::ast::BooleanLiteral;
use crate::ast::Comment;
use crate::ast::Identifier;
use crate::ast::IdentifierInner;
use crate::ast::NumberLiteral;
use crate::ast::StringLiteral;
use crate::ast::VariableKind;
use crate::ast::match_pattern;
use crate::ast_utils;
use crate::expression_parser;
use crate::loc::LOC_NONE;
use crate::loc::Loc;
use crate::main_parser;
use crate::parse_error::MatchNonLastRestKind;
use crate::parse_error::ParseError;
use crate::parser_common;
use crate::parser_common::with_loc;
use crate::parser_env::ParserEnv;
use crate::parser_env::eat;
use crate::parser_env::expect;
use crate::parser_env::peek;
use crate::parser_env::try_parse::Rollback;
use crate::token::TokenKind;

pub(super) fn parse_match_pattern(
    env: &mut ParserEnv,
) -> Result<match_pattern::MatchPattern<Loc, Loc>, Rollback> {
    let start_loc = peek::loc(env).dupe();
    eat::maybe(env, TokenKind::TBitOr)?;
    let pattern = subpattern(env)?;
    let pattern = match peek::token(env) {
        TokenKind::TBitOr => {
            let mut patterns = vec![pattern];
            while matches!(peek::token(env), TokenKind::TBitOr) {
                eat::token(env)?;
                patterns.push(subpattern(env)?);
            }
            let (or_loc, or_pattern) = with_loc(Some(start_loc.dupe()), env, |env| {
                let trailing = eat::trailing_comments(env);
                let comments = ast_utils::mk_comments_opt(None, Some(trailing.into()));
                Ok(match_pattern::OrPattern {
                    patterns: patterns.into(),
                    comments,
                })
            })?;
            match_pattern::MatchPattern::OrPattern {
                loc: or_loc,
                inner: Arc::new(or_pattern),
            }
        }
        _ => pattern,
    };

    match peek::token(env) {
        TokenKind::TIdentifier { raw, .. } if raw == "as" => {
            let (loc, as_pattern) = with_loc(Some(start_loc), env, |env| {
                eat::token(env)?;
                let target = match peek::token(env) {
                    TokenKind::TConst => {
                        let (loc, binding) = binding_pattern(env, VariableKind::Const)?;
                        match_pattern::as_pattern::Target::Binding {
                            loc,
                            pattern: binding,
                        }
                    }
                    TokenKind::TLet => {
                        let (loc, binding) = binding_pattern(env, VariableKind::Let)?;
                        match_pattern::as_pattern::Target::Binding {
                            loc,
                            pattern: binding,
                        }
                    }
                    TokenKind::TVar => {
                        let (loc, binding) = binding_pattern(env, VariableKind::Var)?;
                        match_pattern::as_pattern::Target::Binding {
                            loc,
                            pattern: binding,
                        }
                    }
                    _ => match_pattern::as_pattern::Target::Identifier(
                        main_parser::parse_identifier(env, None)?,
                    ),
                };
                let trailing = eat::trailing_comments(env);
                let comments = ast_utils::mk_comments_opt(None, Some(trailing.into()));
                Ok(match_pattern::AsPattern {
                    pattern,
                    target,
                    comments,
                })
            })?;
            Ok(match_pattern::MatchPattern::AsPattern {
                loc,
                inner: Arc::new(as_pattern),
            })
        }
        _ => Ok(pattern),
    }
}

fn subpattern(env: &mut ParserEnv) -> Result<match_pattern::MatchPattern<Loc, Loc>, Rollback> {
    match peek::token(env) {
        TokenKind::TIdentifier { raw, .. } if raw == "_" => {
            let leading = peek::comments(env);
            let loc = peek::loc(env).dupe();
            eat::token(env)?;
            let trailing = eat::trailing_comments(env);
            let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
            Ok(match_pattern::MatchPattern::WildcardPattern {
                loc,
                inner: match_pattern::WildcardPattern {
                    comments,
                    invalid_syntax_default_keyword: false,
                },
            })
        }
        TokenKind::TDefault => {
            let leading = peek::comments(env);
            let loc = peek::loc(env).dupe();
            eat::token(env)?;
            let trailing = eat::trailing_comments(env);
            let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
            Ok(match_pattern::MatchPattern::WildcardPattern {
                loc,
                inner: match_pattern::WildcardPattern {
                    comments,
                    invalid_syntax_default_keyword: true,
                },
            })
        }
        TokenKind::TLparen => {
            let leading = peek::comments(env);
            expect::token(env, TokenKind::TLparen)?;
            let pattern = parse_match_pattern(env)?;
            expect::token(env, TokenKind::TRparen)?;
            let trailing = eat::trailing_comments(env);
            let pattern = add_comments(pattern, Some(leading.into()), Some(trailing.into()));
            Ok(pattern)
        }
        TokenKind::TNumber { kind, raw } => {
            let kind = *kind;
            let raw = raw.to_owned();
            let leading = peek::comments(env);
            let loc = peek::loc(env).dupe();
            let value = expression_parser::number(env, kind, &raw)?;
            let trailing = eat::trailing_comments(env);
            let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
            Ok(match_pattern::MatchPattern::NumberPattern {
                loc,
                inner: NumberLiteral {
                    value,
                    raw,
                    comments,
                },
            })
        }
        TokenKind::TBigint { kind, raw } => {
            let kind = *kind;
            let raw = raw.to_owned();
            let leading = peek::comments(env);
            let loc = peek::loc(env).dupe();
            let value = expression_parser::bigint(env, kind, &raw)?;
            let trailing = eat::trailing_comments(env);
            let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
            Ok(match_pattern::MatchPattern::BigIntPattern {
                loc,
                inner: BigIntLiteral {
                    value,
                    raw,
                    comments,
                },
            })
        }
        TokenKind::TString(str_loc, value, raw, octal) => {
            let str_loc = str_loc.dupe();
            let value = value.to_owned();
            let raw = raw.to_owned();
            let octal = *octal;
            let leading = peek::comments(env);
            if octal {
                env.strict_error(ParseError::StrictOctalLiteral)?;
            }
            eat::token(env)?;
            let trailing = eat::trailing_comments(env);
            let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
            Ok(match_pattern::MatchPattern::StringPattern {
                loc: str_loc,
                inner: StringLiteral {
                    value,
                    raw,
                    comments,
                },
            })
        }
        TokenKind::TTrue | TokenKind::TFalse => {
            let leading = peek::comments(env);
            let loc = peek::loc(env).dupe();
            let value = matches!(peek::token(env), TokenKind::TTrue);
            eat::token(env)?;
            let trailing = eat::trailing_comments(env);
            let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
            Ok(match_pattern::MatchPattern::BooleanPattern {
                loc,
                inner: BooleanLiteral { value, comments },
            })
        }
        TokenKind::TNull => {
            let leading = peek::comments(env);
            let loc = peek::loc(env).dupe();
            eat::token(env)?;
            let trailing = eat::trailing_comments(env);
            let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
            Ok(match_pattern::MatchPattern::NullPattern {
                loc,
                inner: comments,
            })
        }
        TokenKind::TPlus => {
            let (loc, pattern) = with_loc(None, env, |env| {
                unary_pattern(env, match_pattern::unary_pattern::Operator::Plus)
            })?;
            Ok(match_pattern::MatchPattern::UnaryPattern {
                loc,
                inner: Arc::new(pattern),
            })
        }
        TokenKind::TMinus => {
            let (loc, pattern) = with_loc(None, env, |env| {
                unary_pattern(env, match_pattern::unary_pattern::Operator::Minus)
            })?;
            Ok(match_pattern::MatchPattern::UnaryPattern {
                loc,
                inner: Arc::new(pattern),
            })
        }
        TokenKind::TConst => {
            let (loc, binding) = binding_pattern(env, VariableKind::Const)?;
            Ok(match_pattern::MatchPattern::BindingPattern {
                loc,
                inner: Arc::new(binding),
            })
        }
        TokenKind::TLet => {
            let (loc, binding) = binding_pattern(env, VariableKind::Let)?;
            Ok(match_pattern::MatchPattern::BindingPattern {
                loc,
                inner: Arc::new(binding),
            })
        }
        TokenKind::TVar => {
            let (loc, binding) = binding_pattern(env, VariableKind::Var)?;
            Ok(match_pattern::MatchPattern::BindingPattern {
                loc,
                inner: Arc::new(binding),
            })
        }
        TokenKind::TLcurly => {
            let (loc, pattern) = with_loc(None, env, object_pattern)?;
            Ok(match_pattern::MatchPattern::ObjectPattern {
                loc,
                inner: Arc::new(pattern),
            })
        }
        TokenKind::TLbracket => {
            let (loc, pattern) = with_loc(None, env, array_pattern)?;
            Ok(match_pattern::MatchPattern::ArrayPattern {
                loc,
                inner: Arc::new(pattern),
            })
        }
        _ => {
            if peek::is_identifier(env) {
                let start_loc = peek::loc(env).dupe();
                let id = main_parser::parse_identifier(env, None)?;

                fn member(
                    env: &mut ParserEnv,
                    start_loc: Loc,
                    mut acc: match_pattern::member_pattern::Base<Loc, Loc>,
                ) -> Result<match_pattern::member_pattern::Base<Loc, Loc>, Rollback>
                {
                    loop {
                        match peek::token(env) {
                            TokenKind::TPeriod => {
                                let (loc, mut mem) =
                                    with_loc(Some(start_loc.dupe()), env, |env| {
                                        eat::token(env)?;
                                        let property =
                                    match_pattern::member_pattern::Property::PropertyIdentifier(
                                        parser_common::identifier_name(env)?,
                                    );
                                        let trailing = eat::trailing_comments(env);
                                        let comments =
                                            ast_utils::mk_comments_opt(None, Some(trailing.into()));
                                        Ok(match_pattern::MemberPattern {
                                            loc: LOC_NONE,
                                            base: acc,
                                            property,
                                            comments,
                                        })
                                    })?;
                                mem.loc = loc;
                                acc =
                                    match_pattern::member_pattern::Base::BaseMember(Arc::new(mem));
                            }
                            TokenKind::TLbracket => {
                                let (loc, mut mem) = with_loc(
                                    Some(start_loc.dupe()),
                                    env,
                                    |env| {
                                        expect::token(env, TokenKind::TLbracket)?;
                                        let leading = peek::comments(env);
                                        let property = match peek::token(env) {
                                            TokenKind::TString(loc, value, raw, octal) => {
                                                let loc = loc.dupe();
                                                let value = value.to_owned();
                                                let raw = raw.to_owned();
                                                if *octal {
                                                    env.strict_error(
                                                        ParseError::StrictOctalLiteral,
                                                    )?;
                                                }
                                                eat::token(env)?;
                                                let trailing = eat::trailing_comments(env);
                                                let comments = ast_utils::mk_comments_opt(
                                                    Some(leading.into()),
                                                    Some(trailing.into()),
                                                );
                                                match_pattern::member_pattern::Property::PropertyString {
                                            loc,
                                            literal: StringLiteral {
                                                value,
                                                raw,
                                                comments,
                                            },
                                        }
                                            }
                                            TokenKind::TNumber { kind, raw } => {
                                                let kind = *kind;
                                                let raw = raw.to_owned();
                                                let loc = peek::loc(env).dupe();
                                                let value =
                                                    expression_parser::number(env, kind, &raw)?;
                                                let trailing = eat::trailing_comments(env);
                                                let comments = ast_utils::mk_comments_opt(
                                                    Some(leading.into()),
                                                    Some(trailing.into()),
                                                );
                                                match_pattern::member_pattern::Property::PropertyNumber {
                                            loc,
                                            literal: NumberLiteral {
                                                value,
                                                raw,
                                                comments,
                                            },
                                        }
                                            }
                                            TokenKind::TBigint { kind, raw } => {
                                                let kind = *kind;
                                                let raw = raw.to_owned();
                                                let loc = peek::loc(env).dupe();
                                                let value =
                                                    expression_parser::bigint(env, kind, &raw)?;
                                                let trailing = eat::trailing_comments(env);
                                                let comments = ast_utils::mk_comments_opt(
                                                    Some(leading.into()),
                                                    Some(trailing.into()),
                                                );
                                                match_pattern::member_pattern::Property::PropertyBigInt {
                                            loc,
                                            literal: BigIntLiteral {
                                                value,
                                                raw,
                                                comments,
                                            },
                                        }
                                            }
                                            _ => {
                                                env.error_unexpected(Some(
                                                    "a numeric or string literal".to_owned(),
                                                ))?;
                                                let loc = peek::loc(env).dupe();
                                                match_pattern::member_pattern::Property::PropertyString {
                                            loc,
                                            literal: StringLiteral {
                                                value: FlowSmolStr::new_inline(""),
                                                raw: FlowSmolStr::new_inline("\"\""),
                                                comments: None,
                                            },
                                        }
                                            }
                                        };
                                        expect::token(env, TokenKind::TRbracket)?;
                                        let trailing = eat::trailing_comments(env);
                                        let comments =
                                            ast_utils::mk_comments_opt(None, Some(trailing.into()));
                                        Ok(match_pattern::MemberPattern {
                                            loc: LOC_NONE,
                                            base: acc,
                                            property,
                                            comments,
                                        })
                                    },
                                )?;
                                mem.loc = loc;
                                acc =
                                    match_pattern::member_pattern::Base::BaseMember(Arc::new(mem));
                            }
                            _ => return Ok(acc),
                        }
                    }
                }

                let base = member(
                    env,
                    start_loc.dupe(),
                    match_pattern::member_pattern::Base::BaseIdentifier(id),
                )?;

                match peek::token(env) {
                    TokenKind::TLcurly => {
                        let (loc, pattern) = with_loc(Some(start_loc), env, |env| {
                            let properties = with_loc(None, env, object_pattern)?;
                            let constructor = match base {
                                match_pattern::member_pattern::Base::BaseIdentifier(id) => {
                                    match_pattern::InstancePatternConstructor::IdentifierConstructor(
                                        id,
                                    )
                                }
                                match_pattern::member_pattern::Base::BaseMember(member) => {
                                    match_pattern::InstancePatternConstructor::MemberConstructor(
                                        (*member).clone(),
                                    )
                                }
                            };
                            let trailing = eat::trailing_comments(env);
                            let comments = ast_utils::mk_comments_opt(None, Some(trailing.into()));
                            Ok(match_pattern::InstancePattern {
                                constructor,
                                properties,
                                comments,
                            })
                        })?;
                        Ok(match_pattern::MatchPattern::InstancePattern {
                            loc,
                            inner: Arc::new(pattern),
                        })
                    }
                    _ => match base {
                        match_pattern::member_pattern::Base::BaseIdentifier(id) => {
                            let loc = id.loc.dupe();
                            Ok(match_pattern::MatchPattern::IdentifierPattern { loc, inner: id })
                        }
                        match_pattern::member_pattern::Base::BaseMember(member) => {
                            Ok(match_pattern::MatchPattern::MemberPattern {
                                loc: member.loc.dupe(),
                                inner: member,
                            })
                        }
                    },
                }
            } else {
                let leading = peek::comments(env);
                let loc = peek::loc(env).dupe();
                env.error_unexpected(None)?;
                // Let's get rid of the bad token
                if matches!(peek::token(env), TokenKind::TError(_)) {
                    eat::token(env)?;
                }
                let comments =
                    ast_utils::mk_comments_opt(Some(leading.into()), Some(Vec::new().into()));
                Ok(match_pattern::MatchPattern::WildcardPattern {
                    loc,
                    inner: match_pattern::WildcardPattern {
                        comments,
                        invalid_syntax_default_keyword: false,
                    },
                })
            }
        }
    }
}

fn unary_pattern(
    env: &mut ParserEnv,
    operator: match_pattern::unary_pattern::Operator,
) -> Result<match_pattern::UnaryPattern<Loc>, Rollback> {
    let leading = peek::comments(env);
    eat::token(env)?;
    let argument = match peek::token(env) {
        TokenKind::TNumber { kind, raw } => {
            let kind = *kind;
            let raw = raw.to_owned();
            let leading = peek::comments(env);
            let loc = peek::loc(env).dupe();
            let value = expression_parser::number(env, kind, &raw)?;
            let trailing = eat::trailing_comments(env);
            let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
            (
                loc,
                match_pattern::unary_pattern::Argument::NumberLiteral(NumberLiteral {
                    value,
                    raw,
                    comments,
                }),
            )
        }
        TokenKind::TBigint { kind, raw } => {
            let kind = *kind;
            let raw = raw.to_owned();
            let leading = peek::comments(env);
            let loc = peek::loc(env).dupe();
            let value = expression_parser::bigint(env, kind, &raw)?;
            let trailing = eat::trailing_comments(env);
            let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
            (
                loc,
                match_pattern::unary_pattern::Argument::BigIntLiteral(BigIntLiteral {
                    value,
                    raw,
                    comments,
                }),
            )
        }
        _ => {
            let loc = peek::loc(env).dupe();
            env.error_unexpected(Some("a number literal".to_owned()))?;
            (
                loc,
                match_pattern::unary_pattern::Argument::NumberLiteral(NumberLiteral {
                    value: 0.0,
                    raw: FlowSmolStr::new_inline("0"),
                    comments: None,
                }),
            )
        }
    };
    let trailing = eat::trailing_comments(env);
    let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
    Ok(match_pattern::UnaryPattern {
        operator,
        argument,
        comments,
    })
}

fn binding_pattern(
    env: &mut ParserEnv,
    kind: VariableKind,
) -> Result<(Loc, match_pattern::BindingPattern<Loc, Loc>), Rollback> {
    with_loc(None, env, |env| {
        let leading = peek::comments(env);
        eat::token(env)?;
        let id = main_parser::parse_identifier(env, Some(ParseError::StrictVarName))?;
        let trailing = eat::trailing_comments(env);
        let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
        Ok(match_pattern::BindingPattern { kind, id, comments })
    })
}

fn object_pattern(env: &mut ParserEnv) -> Result<match_pattern::ObjectPattern<Loc, Loc>, Rollback> {
    fn property_key(
        env: &mut ParserEnv,
    ) -> Result<match_pattern::object_pattern::Key<Loc, Loc>, Rollback> {
        let leading = peek::comments(env);
        match peek::token(env) {
            TokenKind::TString(loc, value, raw, octal) => {
                let loc = loc.dupe();
                let value = value.to_owned();
                let raw = raw.to_owned();
                if *octal {
                    env.strict_error(ParseError::StrictOctalLiteral)?;
                }
                eat::token(env)?;
                let trailing = eat::trailing_comments(env);
                let comments =
                    ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
                Ok(match_pattern::object_pattern::Key::StringLiteral((
                    loc,
                    StringLiteral {
                        value,
                        raw,
                        comments,
                    },
                )))
            }
            TokenKind::TNumber { kind, raw } => {
                let kind = *kind;
                let raw = raw.to_owned();
                let loc = peek::loc(env).dupe();
                let value = expression_parser::number(env, kind, &raw)?;
                let trailing = eat::trailing_comments(env);
                let comments =
                    ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
                Ok(match_pattern::object_pattern::Key::NumberLiteral((
                    loc,
                    NumberLiteral {
                        value,
                        raw,
                        comments,
                    },
                )))
            }
            TokenKind::TBigint { kind, raw } => {
                let kind = *kind;
                let raw = raw.to_owned();
                let loc = peek::loc(env).dupe();
                let value = expression_parser::bigint(env, kind, &raw)?;
                let trailing = eat::trailing_comments(env);
                let comments =
                    ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
                Ok(match_pattern::object_pattern::Key::BigIntLiteral((
                    loc,
                    BigIntLiteral {
                        value,
                        raw,
                        comments,
                    },
                )))
            }
            _ => {
                let id = parser_common::identifier_name(env)?;
                Ok(match_pattern::object_pattern::Key::Identifier(id))
            }
        }
    }

    fn property(
        env: &mut ParserEnv,
    ) -> Result<match_pattern::object_pattern::Property<Loc, Loc>, Rollback> {
        let (loc, mut prop) = with_loc(None, env, |env| {
            let leading = peek::comments(env);

            fn shorthand_prop(
                env: &mut ParserEnv,
                (loc, binding): (Loc, match_pattern::BindingPattern<Loc, Loc>),
                leading: Vec<Comment<Loc>>,
            ) -> Result<match_pattern::object_pattern::Property<Loc, Loc>, Rollback> {
                let id = Identifier::new(IdentifierInner {
                    loc: loc.dupe(),
                    name: binding.id.name.dupe(),
                    comments: binding.id.comments.dupe(),
                });
                let key = match_pattern::object_pattern::Key::Identifier(id);
                let pattern = match_pattern::MatchPattern::BindingPattern {
                    loc,
                    inner: Arc::new(binding),
                };
                let trailing = eat::trailing_comments(env);
                let comments =
                    ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
                Ok(match_pattern::object_pattern::Property::Valid {
                    loc: LOC_NONE,
                    property: match_pattern::object_pattern::PropertyStruct {
                        key,
                        pattern,
                        shorthand: true,
                        comments,
                    },
                })
            }

            match peek::token(env) {
                TokenKind::TConst => {
                    let binding = binding_pattern(env, VariableKind::Const)?;
                    shorthand_prop(env, binding, leading)
                }
                TokenKind::TLet => {
                    let binding = binding_pattern(env, VariableKind::Let)?;
                    shorthand_prop(env, binding, leading)
                }
                TokenKind::TVar => {
                    let binding = binding_pattern(env, VariableKind::Var)?;
                    shorthand_prop(env, binding, leading)
                }
                _ => {
                    if peek::is_identifier(env)
                        && matches!(
                            peek::ith_token(env, 1),
                            TokenKind::TComma | TokenKind::TRcurly
                        )
                    {
                        Ok(match_pattern::object_pattern::Property::InvalidShorthand {
                            loc: LOC_NONE,
                            identifier: parser_common::identifier_name(env)?,
                        })
                    } else {
                        let key = property_key(env)?;
                        expect::token(env, TokenKind::TColon)?;
                        let pattern = parse_match_pattern(env)?;
                        let trailing = eat::trailing_comments(env);
                        let comments =
                            ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
                        Ok(match_pattern::object_pattern::Property::Valid {
                            loc: LOC_NONE,
                            property: match_pattern::object_pattern::PropertyStruct {
                                key,
                                pattern,
                                shorthand: false,
                                comments,
                            },
                        })
                    }
                }
            }
        })?;
        *prop.loc_mut() = loc;
        Ok(prop)
    }

    fn properties(
        env: &mut ParserEnv,
    ) -> Result<
        (
            Vec<match_pattern::object_pattern::Property<Loc, Loc>>,
            Option<match_pattern::RestPattern<Loc, Loc>>,
        ),
        Rollback,
    > {
        let mut properties = Vec::new();
        loop {
            match peek::token(env) {
                TokenKind::TEof | TokenKind::TRcurly => {
                    return Ok((properties, None));
                }
                TokenKind::TEllipsis => {
                    let rest = rest_pattern(env)?;
                    if peek::token(env) == &TokenKind::TComma {
                        let loc = peek::loc(env).dupe();
                        env.error_at(
                            loc,
                            ParseError::MatchNonLastRest(MatchNonLastRestKind::Object),
                        )?;
                    }
                    return Ok((properties, Some(rest)));
                }
                _ => {
                    let prop = property(env)?;
                    if peek::token(env) != &TokenKind::TRcurly {
                        expect::token(env, TokenKind::TComma)?;
                    }
                    properties.push(prop);
                }
            }
        }
    }

    let leading = peek::comments(env);
    expect::token(env, TokenKind::TLcurly)?;
    let (props, rest) = properties(env)?;
    let internal = peek::comments(env);
    expect::token(env, TokenKind::TRcurly)?;
    let trailing = eat::trailing_comments(env);
    let comments = ast_utils::mk_comments_with_internal_opt(
        Some(leading.into()),
        Some(trailing.into()),
        Some(internal.into()),
    );
    Ok(match_pattern::ObjectPattern {
        properties: props.into(),
        rest,
        comments,
    })
}

fn array_pattern(env: &mut ParserEnv) -> Result<match_pattern::ArrayPattern<Loc, Loc>, Rollback> {
    fn elements(
        env: &mut ParserEnv,
        start_loc: Loc,
    ) -> Result<
        (
            Vec<match_pattern::array_pattern::Element<Loc, Loc>>,
            Option<match_pattern::RestPattern<Loc, Loc>>,
        ),
        Rollback,
    > {
        let mut elements = Vec::new();
        loop {
            match peek::token(env) {
                TokenKind::TEof | TokenKind::TRbracket => {
                    return Ok((elements, None));
                }
                TokenKind::TEllipsis => {
                    let rest = rest_pattern(env)?;
                    if peek::token(env) == &TokenKind::TComma {
                        let loc = peek::loc(env).dupe();
                        env.error_at(
                            loc,
                            ParseError::MatchNonLastRest(MatchNonLastRestKind::Array),
                        )?;
                    }
                    return Ok((elements, Some(rest)));
                }
                _ => {
                    let pattern = parse_match_pattern(env)?;
                    let index = Loc::between(&start_loc, peek::loc(env));
                    if peek::token(env) != &TokenKind::TRbracket {
                        expect::token(env, TokenKind::TComma)?;
                    }
                    let element = match_pattern::array_pattern::Element { index, pattern };
                    elements.push(element);
                }
            }
        }
    }

    let leading = peek::comments(env);
    let start_loc = peek::loc(env).dupe();
    expect::token(env, TokenKind::TLbracket)?;
    let (elems, rest) = elements(env, start_loc)?;
    let internal = peek::comments(env);
    expect::token(env, TokenKind::TRbracket)?;
    let trailing = eat::trailing_comments(env);
    let comments = ast_utils::mk_comments_with_internal_opt(
        Some(leading.into()),
        Some(trailing.into()),
        Some(internal.into()),
    );
    Ok(match_pattern::ArrayPattern {
        elements: elems.into(),
        rest,
        comments,
    })
}

fn rest_pattern(env: &mut ParserEnv) -> Result<match_pattern::RestPattern<Loc, Loc>, Rollback> {
    let (loc, mut rest) = with_loc(None, env, |env| {
        let leading = peek::comments(env);
        expect::token(env, TokenKind::TEllipsis)?;
        let argument = match peek::token(env) {
            TokenKind::TConst => Some(binding_pattern(env, VariableKind::Const)?),
            TokenKind::TLet => Some(binding_pattern(env, VariableKind::Let)?),
            TokenKind::TVar => Some(binding_pattern(env, VariableKind::Var)?),
            _ => None,
        };
        let trailing = eat::trailing_comments(env);
        let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
        Ok(match_pattern::RestPattern {
            loc: LOC_NONE,
            argument,
            comments,
        })
    })?;
    rest.loc = loc;
    Ok(rest)
}

fn add_comments(
    pattern: match_pattern::MatchPattern<Loc, Loc>,
    leading: Option<Arc<[Comment<Loc>]>>,
    trailing: Option<Arc<[Comment<Loc>]>>,
) -> match_pattern::MatchPattern<Loc, Loc> {
    match pattern {
        match_pattern::MatchPattern::WildcardPattern { loc, mut inner } => {
            inner.comments = ast_utils::merge_comments(
                std::mem::take(&mut inner.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::WildcardPattern { loc, inner }
        }
        match_pattern::MatchPattern::NumberPattern { loc, mut inner } => {
            inner.comments = ast_utils::merge_comments(
                std::mem::take(&mut inner.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::NumberPattern { loc, inner }
        }
        match_pattern::MatchPattern::BigIntPattern { loc, mut inner } => {
            inner.comments = ast_utils::merge_comments(
                std::mem::take(&mut inner.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::BigIntPattern { loc, inner }
        }
        match_pattern::MatchPattern::StringPattern { loc, mut inner } => {
            inner.comments = ast_utils::merge_comments(
                std::mem::take(&mut inner.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::StringPattern { loc, inner }
        }
        match_pattern::MatchPattern::BooleanPattern { loc, mut inner } => {
            inner.comments = ast_utils::merge_comments(
                std::mem::take(&mut inner.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::BooleanPattern { loc, inner }
        }
        match_pattern::MatchPattern::NullPattern { loc, inner } => {
            let new_inner =
                ast_utils::merge_comments(inner, ast_utils::mk_comments_opt(leading, trailing));
            match_pattern::MatchPattern::NullPattern {
                loc,
                inner: new_inner,
            }
        }
        match_pattern::MatchPattern::UnaryPattern { loc, mut inner } => {
            let content = Arc::get_mut(&mut inner).unwrap();
            content.comments = ast_utils::merge_comments(
                std::mem::take(&mut content.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::UnaryPattern { loc, inner }
        }
        match_pattern::MatchPattern::BindingPattern { loc, mut inner } => {
            let content = Arc::get_mut(&mut inner).unwrap();
            content.comments = ast_utils::merge_comments(
                std::mem::take(&mut content.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::BindingPattern { loc, inner }
        }
        match_pattern::MatchPattern::IdentifierPattern { loc, inner } => {
            let new_inner = Identifier::new(IdentifierInner {
                loc: inner.loc.dupe(),
                name: inner.name.dupe(),
                comments: ast_utils::merge_comments(
                    inner.comments.dupe(),
                    ast_utils::mk_comments_opt(leading, trailing),
                ),
            });
            match_pattern::MatchPattern::IdentifierPattern {
                loc,
                inner: new_inner,
            }
        }
        match_pattern::MatchPattern::MemberPattern { loc, mut inner } => {
            let content = Arc::get_mut(&mut inner).unwrap();
            content.comments = ast_utils::merge_comments(
                std::mem::take(&mut content.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::MemberPattern { loc, inner }
        }
        match_pattern::MatchPattern::ObjectPattern { loc, mut inner } => {
            let content = Arc::get_mut(&mut inner).unwrap();
            content.comments = ast_utils::merge_comments_with_internal(
                std::mem::take(&mut content.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::ObjectPattern { loc, inner }
        }
        match_pattern::MatchPattern::ArrayPattern { loc, mut inner } => {
            let content = Arc::get_mut(&mut inner).unwrap();
            content.comments = ast_utils::merge_comments_with_internal(
                std::mem::take(&mut content.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::ArrayPattern { loc, inner }
        }
        match_pattern::MatchPattern::InstancePattern { loc, mut inner } => {
            let content = Arc::get_mut(&mut inner).unwrap();
            content.comments = ast_utils::merge_comments(
                std::mem::take(&mut content.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::InstancePattern { loc, inner }
        }
        match_pattern::MatchPattern::OrPattern { loc, mut inner } => {
            let content = Arc::get_mut(&mut inner).unwrap();
            content.comments = ast_utils::merge_comments(
                std::mem::take(&mut content.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::OrPattern { loc, inner }
        }
        match_pattern::MatchPattern::AsPattern { loc, mut inner } => {
            let content = Arc::get_mut(&mut inner).unwrap();
            content.comments = ast_utils::merge_comments(
                std::mem::take(&mut content.comments),
                ast_utils::mk_comments_opt(leading, trailing),
            );
            match_pattern::MatchPattern::AsPattern { loc, inner }
        }
    }
}
