/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashSet;
use std::ops::Deref;
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
use crate::ast::Syntax;
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

#[derive(Default)]
struct Members {
    boolean_members: Vec<statement::enum_declaration::InitializedMember<BooleanLiteral<Loc>, Loc>>,
    number_members: Vec<statement::enum_declaration::InitializedMember<NumberLiteral<Loc>, Loc>>,
    string_members: Vec<statement::enum_declaration::InitializedMember<StringLiteral<Loc>, Loc>>,
    bigint_members: Vec<statement::enum_declaration::InitializedMember<BigIntLiteral<Loc>, Loc>>,
    defaulted_members: Vec<statement::enum_declaration::DefaultedMember<Loc>>,
}

#[derive(Default)]
struct Acc {
    members: Members,
    seen_names: HashSet<FlowSmolStr>,
    has_unknown_members: bool,
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
    Acc::default()
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

fn member_raw(env: &mut ParserEnv) -> Result<(Loc, (Identifier<Loc, Loc>, Init)), Rollback> {
    with_loc(None, env, |env| {
        let id = parser_common::identifier_name(env)?;
        let init = match peek::token(env) {
            TokenKind::TAssign => {
                expect::token(env, TokenKind::TAssign)?;
                member_init(env)?
            }
            TokenKind::TColon => {
                let IdentifierInner {
                    loc: _,
                    name: member_name,
                    comments: _,
                } = id.deref();
                env.error(ParseError::EnumInvalidInitializerSeparator {
                    member_name: member_name.as_str().to_owned(),
                })?;
                expect::token(env, TokenKind::TColon)?;
                member_init(env)?
            }
            _ => Init::NoInit,
        };
        Ok((id, init))
    })
}

fn check_explicit_type_mismatch(
    env: &mut ParserEnv,
    enum_name: &str,
    explicit_type: Option<ExplicitType>,
    member_name: &str,
    literal_type: ExplicitType,
    loc: Loc,
) -> Result<(), Rollback> {
    if let Some(enum_type) = explicit_type {
        if enum_type != literal_type {
            env.error_at(
                loc,
                ParseError::EnumInvalidMemberInitializer {
                    enum_name: enum_name.to_owned(),
                    explicit_type: Some(enum_type),
                    member_name: member_name.to_owned(),
                },
            )?;
        }
    }
    Ok(())
}

fn enum_member(
    env: &mut ParserEnv,
    acc: &mut Acc,
    enum_name: &str,
    explicit_type: Option<ExplicitType>,
) -> Result<(), Rollback> {
    let (member_loc, (id, init)) = member_raw(env)?;
    let IdentifierInner {
        loc: id_loc,
        name: member_name,
        comments: _,
    } = id.deref();

    /* if we parsed an empty name, something has gone wrong and we should abort analysis */
    if member_name.is_empty() {
        return Ok(());
    }

    if acc.seen_names.contains(member_name) {
        env.error_at(
            id_loc.dupe(),
            ParseError::EnumDuplicateMemberName {
                enum_name: enum_name.to_owned(),
                member_name: member_name.as_str().to_owned(),
            },
        )?;
    }
    acc.seen_names.insert(member_name.to_owned());

    match init {
        Init::BooleanInit(loc, value) => {
            check_explicit_type_mismatch(
                env,
                enum_name,
                explicit_type,
                member_name,
                ExplicitType::Boolean,
                loc.dupe(),
            )?;
            let member = statement::enum_declaration::InitializedMember {
                loc: member_loc,
                id,
                init: (loc, value),
            };
            acc.members.boolean_members.push(member);
        }
        Init::NumberInit(loc, value) => {
            check_explicit_type_mismatch(
                env,
                enum_name,
                explicit_type,
                member_name,
                ExplicitType::Number,
                loc.dupe(),
            )?;
            let member = statement::enum_declaration::InitializedMember {
                loc: member_loc,
                id,
                init: (loc, value),
            };
            acc.members.number_members.push(member);
        }
        Init::StringInit(loc, value) => {
            check_explicit_type_mismatch(
                env,
                enum_name,
                explicit_type,
                member_name,
                ExplicitType::String,
                loc.dupe(),
            )?;
            let member = statement::enum_declaration::InitializedMember {
                loc: member_loc,
                id,
                init: (loc, value),
            };
            acc.members.string_members.push(member);
        }
        Init::BigIntInit(loc, value) => {
            check_explicit_type_mismatch(
                env,
                enum_name,
                explicit_type,
                member_name,
                ExplicitType::BigInt,
                loc.dupe(),
            )?;
            let member = statement::enum_declaration::InitializedMember {
                loc: member_loc,
                id,
                init: (loc, value),
            };
            acc.members.bigint_members.push(member);
        }
        Init::InvalidInit(loc) => {
            env.error_at(
                loc,
                ParseError::EnumInvalidMemberInitializer {
                    enum_name: enum_name.to_owned(),
                    explicit_type,
                    member_name: member_name.as_str().to_owned(),
                },
            )?;
        }
        Init::NoInit => match explicit_type {
            Some(ExplicitType::Boolean) => {
                env.error_at(
                    member_loc,
                    ParseError::EnumBooleanMemberNotInitialized {
                        enum_name: enum_name.to_owned(),
                        member_name: member_name.as_str().to_owned(),
                    },
                )?;
            }
            Some(ExplicitType::Number) => {
                env.error_at(
                    member_loc,
                    ParseError::EnumNumberMemberNotInitialized {
                        enum_name: enum_name.to_owned(),
                        member_name: member_name.as_str().to_owned(),
                    },
                )?;
            }
            Some(ExplicitType::BigInt) => {
                env.error_at(
                    member_loc,
                    ParseError::EnumBigIntMemberNotInitialized {
                        enum_name: enum_name.to_owned(),
                        member_name: member_name.as_str().to_owned(),
                    },
                )?;
            }
            Some(ExplicitType::String) | Some(ExplicitType::Symbol) | None => {
                let member = statement::enum_declaration::DefaultedMember {
                    loc: member_loc,
                    id,
                };
                acc.members.defaulted_members.push(member);
            }
        },
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
                        env.error_at(loc, ParseError::EnumInvalidEllipsis { trailing_comma })?;
                    }
                    _ => {
                        env.error_at(
                            loc,
                            ParseError::EnumInvalidEllipsis {
                                trailing_comma: false,
                            },
                        )?;
                    }
                }
                acc.has_unknown_members = true;
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

fn string_body(
    env: &mut ParserEnv,
    enum_name: &str,
    is_explicit: bool,
    has_unknown_members: bool,
    string_members: Vec<statement::enum_declaration::InitializedMember<StringLiteral<Loc>, Loc>>,
    defaulted_members: Vec<statement::enum_declaration::DefaultedMember<Loc>>,
    comments: Option<Syntax<Loc, Arc<[Comment<Loc>]>>>,
    body_loc: Loc,
) -> Result<statement::enum_declaration::Body<Loc>, Rollback> {
    let initialized_len = string_members.len();
    let defaulted_len = defaulted_members.len();

    match (initialized_len, defaulted_len) {
        (0, 0) | (0, _) => Ok(statement::enum_declaration::Body::StringBody {
            loc: body_loc,
            body: statement::enum_declaration::StringBody {
                members: statement::enum_declaration::StringBodyMembers::Defaulted(
                    defaulted_members.into(),
                ),
                explicit_type: is_explicit,
                has_unknown_members,
                comments,
            },
        }),
        (_, 0) => Ok(statement::enum_declaration::Body::StringBody {
            loc: body_loc.dupe(),
            body: statement::enum_declaration::StringBody {
                members: statement::enum_declaration::StringBodyMembers::Initialized(
                    string_members.into(),
                ),
                explicit_type: is_explicit,
                has_unknown_members,
                comments,
            },
        }),
        _ => {
            if defaulted_len > initialized_len {
                for m in &string_members {
                    env.error_at(
                        m.loc.dupe(),
                        ParseError::EnumStringMemberInconsistentlyInitialized {
                            enum_name: enum_name.to_owned(),
                        },
                    )?;
                }
                Ok(statement::enum_declaration::Body::StringBody {
                    loc: body_loc,
                    body: statement::enum_declaration::StringBody {
                        members: statement::enum_declaration::StringBodyMembers::Defaulted(
                            defaulted_members.into(),
                        ),
                        explicit_type: is_explicit,
                        has_unknown_members,
                        comments,
                    },
                })
            } else {
                for m in &defaulted_members {
                    env.error_at(
                        m.loc.dupe(),
                        ParseError::EnumStringMemberInconsistentlyInitialized {
                            enum_name: enum_name.to_owned(),
                        },
                    )?;
                }
                Ok(statement::enum_declaration::Body::StringBody {
                    loc: body_loc.dupe(),
                    body: statement::enum_declaration::StringBody {
                        members: statement::enum_declaration::StringBodyMembers::Initialized(
                            string_members.into(),
                        ),
                        explicit_type: is_explicit,
                        has_unknown_members,
                        comments,
                    },
                })
            }
        }
    }
}

fn parse_explicit_type(
    env: &mut ParserEnv,
    enum_name: &str,
) -> Result<Option<ExplicitType>, Rollback> {
    if eat::maybe(env, TokenKind::TOf)? {
        eat::push_lex_mode(env, LexMode::Type);
        let result = match peek::token(env) {
            TokenKind::TBooleanType(BoolOrBoolean::Boolean) => Some(ExplicitType::Boolean),
            TokenKind::TNumberType => Some(ExplicitType::Number),
            TokenKind::TStringType => Some(ExplicitType::String),
            TokenKind::TSymbolType => Some(ExplicitType::Symbol),
            TokenKind::TBigintType => Some(ExplicitType::BigInt),
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
    name_loc: Loc,
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
        enum_members(env, &mut acc, enum_name, explicit_type)?;
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

        let body = match explicit_type {
            Some(ExplicitType::Boolean) => statement::enum_declaration::Body::BooleanBody {
                loc: LOC_NONE,
                body: statement::enum_declaration::BooleanBody {
                    members: members.boolean_members.into(),
                    explicit_type: true,
                    has_unknown_members,
                    comments,
                },
            },
            Some(ExplicitType::Number) => statement::enum_declaration::Body::NumberBody {
                loc: LOC_NONE,
                body: statement::enum_declaration::NumberBody {
                    members: members.number_members.into(),
                    explicit_type: true,
                    has_unknown_members,
                    comments,
                },
            },
            Some(ExplicitType::String) => string_body(
                env,
                enum_name,
                true,
                has_unknown_members,
                members.string_members,
                members.defaulted_members,
                comments,
                LOC_NONE,
            )?,
            Some(ExplicitType::Symbol) => statement::enum_declaration::Body::SymbolBody {
                loc: LOC_NONE,
                body: statement::enum_declaration::SymbolBody {
                    members: members.defaulted_members.into(),
                    has_unknown_members,
                    comments,
                },
            },
            Some(ExplicitType::BigInt) => statement::enum_declaration::Body::BigIntBody {
                loc: LOC_NONE,
                body: statement::enum_declaration::BigIntBody {
                    members: members.bigint_members.into(),
                    explicit_type: true,
                    has_unknown_members,
                    comments,
                },
            },
            None => {
                let bools_len = members.boolean_members.len();
                let nums_len = members.number_members.len();
                let bigints_len = members.bigint_members.len();
                let strs_len = members.string_members.len();
                let defaulted_len = members.defaulted_members.len();

                match (bools_len, nums_len, bigints_len, strs_len, defaulted_len) {
                    (0, 0, 0, 0, 0) => statement::enum_declaration::Body::StringBody {
                        loc: LOC_NONE,
                        body: statement::enum_declaration::StringBody {
                            members: statement::enum_declaration::StringBodyMembers::Defaulted(
                                Arc::from([]) as Arc<[_]>,
                            ),
                            explicit_type: false,
                            has_unknown_members,
                            comments,
                        },
                    },
                    (0, 0, 0, _, _) => string_body(
                        env,
                        enum_name,
                        false,
                        has_unknown_members,
                        members.string_members,
                        members.defaulted_members,
                        comments,
                        LOC_NONE,
                    )?,
                    (_, 0, 0, 0, _) if bools_len >= defaulted_len => {
                        for m in &members.defaulted_members {
                            let member_name = &m.id.name;
                            env.error_at(
                                m.loc.dupe(),
                                ParseError::EnumBooleanMemberNotInitialized {
                                    enum_name: enum_name.to_owned(),
                                    member_name: member_name.as_str().to_owned(),
                                },
                            )?;
                        }
                        statement::enum_declaration::Body::BooleanBody {
                            loc: LOC_NONE,
                            body: statement::enum_declaration::BooleanBody {
                                members: members.boolean_members.into(),
                                explicit_type: false,
                                has_unknown_members,
                                comments,
                            },
                        }
                    }
                    (0, _, 0, 0, _) if nums_len >= defaulted_len => {
                        for m in &members.defaulted_members {
                            let member_name = &m.id.name;
                            env.error_at(
                                m.loc.dupe(),
                                ParseError::EnumNumberMemberNotInitialized {
                                    enum_name: enum_name.to_owned(),
                                    member_name: member_name.as_str().to_owned(),
                                },
                            )?;
                        }
                        statement::enum_declaration::Body::NumberBody {
                            loc: LOC_NONE,
                            body: statement::enum_declaration::NumberBody {
                                members: members.number_members.into(),
                                explicit_type: false,
                                has_unknown_members,
                                comments,
                            },
                        }
                    }
                    (0, 0, _, 0, _) if bigints_len >= defaulted_len => {
                        for m in &members.defaulted_members {
                            let member_name = &m.id.name;
                            env.error_at(
                                m.loc.dupe(),
                                ParseError::EnumNumberMemberNotInitialized {
                                    enum_name: enum_name.to_owned(),
                                    member_name: member_name.as_str().to_owned(),
                                },
                            )?;
                        }
                        statement::enum_declaration::Body::BigIntBody {
                            loc: LOC_NONE,
                            body: statement::enum_declaration::BigIntBody {
                                members: members.bigint_members.into(),
                                explicit_type: false,
                                has_unknown_members,
                                comments,
                            },
                        }
                    }
                    _ => {
                        env.error_at(
                            name_loc,
                            ParseError::EnumInconsistentMemberValues {
                                enum_name: enum_name.to_owned(),
                            },
                        )?;
                        statement::enum_declaration::Body::StringBody {
                            loc: LOC_NONE,
                            body: statement::enum_declaration::StringBody {
                                members: statement::enum_declaration::StringBodyMembers::Defaulted(
                                    Arc::from([]) as Arc<[_]>,
                                ),
                                explicit_type: false,
                                has_unknown_members,
                                comments,
                            },
                        }
                    }
                }
            }
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
        loc: name_loc,
        name: enum_name,
        comments: _,
    } = id.deref();
    let body = enum_body(env, enum_name, name_loc.dupe())?;
    let comments = ast_utils::mk_comments_opt(Some(leading.into()), None);
    Ok(statement::EnumDeclaration {
        id,
        body,
        const_,
        comments,
    })
}
