/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::ast::BigIntLiteral;
use crate::ast::BooleanLiteral;
use crate::ast::Comment;
use crate::ast::IdentifierInner;
use crate::ast::NumberLiteral;
use crate::ast::StringLiteral;
use crate::ast::statement;
use crate::ast_utils;
use crate::enum_common::ExplicitType;
use crate::expression_parser;
use crate::loc::LOC_NONE;
use crate::loc::Loc;
use crate::main_parser;
use crate::parse_error::ParseError;
use crate::parser_common;
use crate::parser_common::with_loc;
use crate::parser_env::LexMode;
use crate::parser_env::ParserEnv;
use crate::parser_env::eat;
use crate::parser_env::expect;
use crate::parser_env::peek;
use crate::parser_env::try_parse::Rollback;
use crate::token::BoolOrBoolean;
use crate::token::NumberType;
use crate::token::TokenKind;

struct Acc {
    members: Vec<statement::enum_declaration::Member<Loc>>,
    has_unknown_members: Option<Loc>,
    internal_comments: Vec<Comment<Loc>>,
}

enum Init {
    NoInit,
    InvalidInit(Loc),
    BooleanInit(Loc, BooleanLiteral<Loc>),
    NumberInit(Loc, NumberLiteral<Loc>),
    StringInit(Loc, StringLiteral<Loc>),
    BigIntInit(Loc, BigIntLiteral<Loc>),
}

fn empty_acc() -> Acc {
    Acc {
        members: Vec::new(),
        has_unknown_members: None,
        internal_comments: Vec::new(),
    }
}

fn end_of_member_init(env: &mut ParserEnv) -> bool {
    matches!(
        peek::token(env),
        TokenKind::TSemicolon | TokenKind::TComma | TokenKind::TRcurly
    )
}

fn number_init(
    env: &mut ParserEnv,
    loc: Loc,
    neg: bool,
    leading: Vec<Comment<Loc>>,
    kind: NumberType,
    raw: FlowSmolStr,
) -> Result<Init, Rollback> {
    let value = expression_parser::number(env, kind, &raw)?;
    let (value, raw) = if neg {
        (-value, FlowSmolStr::new(format!("-{}", raw)))
    } else {
        (value, raw)
    };
    let trailing = eat::trailing_comments(env);
    Ok(if end_of_member_init(env) {
        Init::NumberInit(
            loc,
            NumberLiteral {
                value,
                raw,
                comments: ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into())),
            },
        )
    } else {
        Init::InvalidInit(loc)
    })
}

fn member_init(env: &mut ParserEnv) -> Result<Init, Rollback> {
    let loc = peek::loc(env).dupe();
    let leading = peek::comments(env);
    match peek::token(env) {
        TokenKind::TMinus => {
            eat::token(env)?;
            match peek::token(env) {
                TokenKind::TNumber { kind, raw } => {
                    let kind = *kind;
                    let raw = raw.to_owned();
                    number_init(env, loc, true, leading, kind, raw)
                }
                _ => Ok(Init::InvalidInit(loc)),
            }
        }
        TokenKind::TNumber { kind, raw } => {
            let kind = *kind;
            let raw = raw.to_owned();
            number_init(env, loc, false, leading, kind, raw)
        }
        TokenKind::TString(str_loc, value, raw, octal) => {
            let str_loc = str_loc.dupe();
            let value = value.to_owned();
            let raw = raw.to_owned();
            if *octal {
                env.strict_error(ParseError::StrictOctalLiteral)?;
            }
            eat::token(env)?;
            let trailing = eat::trailing_comments(env);
            if end_of_member_init(env) {
                Ok(Init::StringInit(
                    str_loc,
                    StringLiteral {
                        value,
                        raw,
                        comments: ast_utils::mk_comments_opt(
                            Some(leading.into()),
                            Some(trailing.into()),
                        ),
                    },
                ))
            } else {
                Ok(Init::InvalidInit(loc))
            }
        }
        TokenKind::TTrue | TokenKind::TFalse => {
            let is_true = peek::token(env) == &TokenKind::TTrue;
            eat::token(env)?;
            let trailing = eat::trailing_comments(env);
            if end_of_member_init(env) {
                Ok(Init::BooleanInit(
                    loc.dupe(),
                    BooleanLiteral {
                        value: is_true,
                        comments: ast_utils::mk_comments_opt(
                            Some(leading.into()),
                            Some(trailing.into()),
                        ),
                    },
                ))
            } else {
                Ok(Init::InvalidInit(loc))
            }
        }
        TokenKind::TBigint { kind, raw } => {
            let kind = *kind;
            let raw = raw.to_owned();
            let value = expression_parser::bigint(env, kind, &raw)?;
            let trailing = eat::trailing_comments(env);
            if end_of_member_init(env) {
                Ok(Init::BigIntInit(
                    loc.dupe(),
                    BigIntLiteral {
                        value,
                        raw,
                        comments: ast_utils::mk_comments_opt(
                            Some(leading.into()),
                            Some(trailing.into()),
                        ),
                    },
                ))
            } else {
                Ok(Init::InvalidInit(loc))
            }
        }
        _ => {
            eat::token(env)?;
            Ok(Init::InvalidInit(loc))
        }
    }
}

fn member_raw(
    env: &mut ParserEnv,
) -> Result<(Loc, (statement::enum_declaration::MemberName<Loc>, Init)), Rollback> {
    with_loc(None, env, |env| {
        let id = match peek::token(env) {
            TokenKind::TString(str_loc, value, raw, octal) => {
                let str_loc = str_loc.dupe();
                let value = value.to_owned();
                let raw = raw.to_owned();
                if *octal {
                    env.strict_error(ParseError::StrictOctalLiteral)?;
                }
                eat::token(env)?;
                statement::enum_declaration::MemberName::StringLiteral(
                    str_loc,
                    StringLiteral {
                        value,
                        raw,
                        comments: None,
                    },
                )
            }
            _ => statement::enum_declaration::MemberName::Identifier(
                parser_common::identifier_name(env)?,
            ),
        };
        let init = match peek::token(env) {
            TokenKind::TAssign => {
                expect::token(env, TokenKind::TAssign)?;
                member_init(env)?
            }
            TokenKind::TColon => {
                let member_name = ast_utils::string_of_enum_member_name(&id).to_owned();
                env.error(ParseError::EnumInvalidInitializerSeparator { member_name })?;
                expect::token(env, TokenKind::TColon)?;
                member_init(env)?
            }
            _ => Init::NoInit,
        };
        Ok((id, init))
    })
}

fn enum_member(
    env: &mut ParserEnv,
    acc: &mut Acc,
    enum_name: &str,
    explicit_type: Option<ExplicitType>,
) -> Result<(), Rollback> {
    let (member_loc, (id, init)) = member_raw(env)?;
    let member_name = ast_utils::string_of_enum_member_name(&id);

    /* if we parsed an empty name, something has gone wrong and we should abort analysis */
    if member_name.is_empty() {
        return Ok(());
    }

    match init {
        Init::BooleanInit(loc, value) => {
            let member = statement::enum_declaration::InitializedMember {
                loc: member_loc,
                id,
                init: (loc, value),
            };
            acc.members
                .push(statement::enum_declaration::Member::BooleanMember(member));
        }
        Init::NumberInit(loc, value) => {
            let member = statement::enum_declaration::InitializedMember {
                loc: member_loc,
                id,
                init: (loc, value),
            };
            acc.members
                .push(statement::enum_declaration::Member::NumberMember(member));
        }
        Init::StringInit(loc, value) => {
            let member = statement::enum_declaration::InitializedMember {
                loc: member_loc,
                id,
                init: (loc, value),
            };
            acc.members
                .push(statement::enum_declaration::Member::StringMember(member));
        }
        Init::BigIntInit(loc, value) => {
            let member = statement::enum_declaration::InitializedMember {
                loc: member_loc,
                id,
                init: (loc, value),
            };
            acc.members
                .push(statement::enum_declaration::Member::BigIntMember(member));
        }
        Init::InvalidInit(loc) => {
            env.error_at(
                loc,
                ParseError::EnumInvalidMemberInitializer {
                    enum_name: enum_name.to_owned(),
                    explicit_type,
                    member_name: member_name.to_owned(),
                },
            )?;
        }
        Init::NoInit => {
            let member = statement::enum_declaration::DefaultedMember {
                loc: member_loc,
                id,
            };
            acc.members
                .push(statement::enum_declaration::Member::DefaultedMember(member));
        }
    }
    Ok(())
}

fn enum_members(
    env: &mut ParserEnv,
    acc: &mut Acc,
    enum_name: &str,
    explicit_type: Option<ExplicitType>,
) -> Result<(), Rollback> {
    loop {
        match peek::token(env) {
            TokenKind::TRcurly | TokenKind::TEof => return Ok(()),
            TokenKind::TEllipsis => {
                let loc = peek::loc(env).dupe();
                // Internal comments may appear before the ellipsis
                let internal_comments = peek::comments(env);
                acc.internal_comments.extend(internal_comments);
                eat::token(env)?;
                match peek::token(env) {
                    TokenKind::TRcurly | TokenKind::TEof => {}
                    TokenKind::TComma => {
                        expect::token(env, TokenKind::TComma)?;
                        let trailing_comma =
                            matches!(peek::token(env), TokenKind::TRcurly | TokenKind::TEof);
                        env.error_at(
                            loc.dupe(),
                            ParseError::EnumInvalidEllipsis { trailing_comma },
                        )?;
                    }
                    _ => {
                        env.error_at(
                            loc.dupe(),
                            ParseError::EnumInvalidEllipsis {
                                trailing_comma: false,
                            },
                        )?;
                    }
                }
                acc.has_unknown_members = Some(loc);
            }
            _ => {
                enum_member(env, acc, enum_name, explicit_type)?;
                match peek::token(env) {
                    TokenKind::TRcurly | TokenKind::TEof => {}
                    TokenKind::TSemicolon => {
                        env.error(ParseError::EnumInvalidMemberSeparator)?;
                        expect::token(env, TokenKind::TSemicolon)?;
                    }
                    _ => {
                        expect::token(env, TokenKind::TComma)?;
                    }
                }
            }
        }
    }
}

fn parse_explicit_type(
    env: &mut ParserEnv,
    enum_name: &str,
) -> Result<Option<(Loc, ExplicitType)>, Rollback> {
    if eat::maybe(env, TokenKind::TOf)? {
        eat::push_lex_mode(env, LexMode::Type);
        let type_loc = peek::loc(env).dupe();
        let result = match peek::token(env) {
            TokenKind::TBooleanType(BoolOrBoolean::Boolean) => {
                Some((type_loc, ExplicitType::Boolean))
            }
            TokenKind::TNumberType => Some((type_loc, ExplicitType::Number)),
            TokenKind::TStringType => Some((type_loc, ExplicitType::String)),
            TokenKind::TSymbolType => Some((type_loc, ExplicitType::Symbol)),
            TokenKind::TBigintType => Some((type_loc, ExplicitType::BigInt)),
            TokenKind::TIdentifier { raw, .. } => {
                let raw = raw.as_str().to_owned();
                env.error(ParseError::EnumInvalidExplicitType {
                    enum_name: enum_name.to_owned(),
                    supplied_type: Some(raw),
                })?;
                None
            }
            _ => {
                env.error(ParseError::EnumInvalidExplicitType {
                    enum_name: enum_name.to_owned(),
                    supplied_type: None,
                })?;
                None
            }
        };
        eat::token(env)?;
        eat::pop_lex_mode(env);
        Ok(result)
    } else {
        Ok(None)
    }
}

fn enum_body(
    env: &mut ParserEnv,
    enum_name: &str,
) -> Result<statement::enum_declaration::Body<Loc>, Rollback> {
    let (loc, mut body) = with_loc(None, env, |env| {
        let explicit_type = parse_explicit_type(env, enum_name)?;
        let leading = if explicit_type.is_some() {
            peek::comments(env)
        } else {
            Vec::new()
        };
        expect::token(env, TokenKind::TLcurly)?;
        let mut acc = empty_acc();
        let explicit_type_kind = explicit_type.as_ref().map(|(_, t)| *t);
        enum_members(env, &mut acc, enum_name, explicit_type_kind)?;
        let mut internal = acc.internal_comments;
        internal.extend(peek::comments(env));
        expect::token(env, TokenKind::TRcurly)?;
        let trailing = match peek::token(env) {
            TokenKind::TEof | TokenKind::TRcurly => eat::trailing_comments(env),
            _ => {
                if peek::is_line_terminator(env) {
                    eat::comments_until_next_line(env)
                } else {
                    Vec::new()
                }
            }
        };
        let comments = ast_utils::mk_comments_with_internal_opt(
            Some(leading.into()),
            Some(trailing.into()),
            Some(internal.into()),
        );
        let members = acc.members;
        let has_unknown_members = acc.has_unknown_members;

        let body = statement::enum_declaration::Body {
            loc: LOC_NONE,
            members: members.into(),
            explicit_type,
            has_unknown_members,
            comments,
        };

        Ok(body)
    })?;
    *body.loc_mut() = loc;
    Ok(body)
}

pub(super) fn declaration(
    leading: Vec<Comment<Loc>>,
    const_: bool,
    env: &mut ParserEnv,
) -> Result<statement::EnumDeclaration<Loc, Loc>, Rollback> {
    let leading = [leading, peek::comments(env)].concat();
    expect::token(env, TokenKind::TEnum)?;
    let id = main_parser::parse_identifier(env, None)?;
    let IdentifierInner {
        loc: _name_loc,
        name: enum_name,
        comments: _,
    } = id.deref();
    let body = enum_body(env, enum_name)?;
    let comments = ast_utils::mk_comments_opt(Some(leading.into()), None);
    Ok(statement::EnumDeclaration {
        id,
        body,
        const_,
        comments,
    })
}
