/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::ast::expression::ExpressionInner;
use crate::ast::*;
use crate::ast_utils;
use crate::loc::Loc;
use crate::parse_error::ParseError;
use crate::parser_env::try_parse::Rollback;
use crate::parser_env::*;
use crate::token::BoolOrBoolean;
use crate::token::TokenKind;

fn identifier_name_raw<'a>(env: &mut ParserEnv<'a>) -> Result<FlowSmolStr, Rollback> {
    let name = match peek::token(env) {
        /* obviously, Identifier is a valid IdentifierName */
        TokenKind::TIdentifier { value, .. } => value.dupe(),
        /* keywords are also IdentifierNames */
        TokenKind::TAwait => FlowSmolStr::new_inline("await"),
        TokenKind::TBreak => FlowSmolStr::new_inline("break"),
        TokenKind::TCase => FlowSmolStr::new_inline("case"),
        TokenKind::TCatch => FlowSmolStr::new_inline("catch"),
        TokenKind::TClass => FlowSmolStr::new_inline("class"),
        TokenKind::TConst => FlowSmolStr::new_inline("const"),
        TokenKind::TContinue => FlowSmolStr::new_inline("continue"),
        TokenKind::TDebugger => FlowSmolStr::new_inline("debugger"),
        TokenKind::TDefault => FlowSmolStr::new_inline("default"),
        TokenKind::TDelete => FlowSmolStr::new_inline("delete"),
        TokenKind::TDo => FlowSmolStr::new_inline("do"),
        TokenKind::TElse => FlowSmolStr::new_inline("else"),
        TokenKind::TExport => FlowSmolStr::new_inline("export"),
        TokenKind::TExtends => FlowSmolStr::new_inline("extends"),
        TokenKind::TFinally => FlowSmolStr::new_inline("finally"),
        TokenKind::TFor => FlowSmolStr::new_inline("for"),
        TokenKind::TFunction => FlowSmolStr::new_inline("function"),
        TokenKind::TIf => FlowSmolStr::new_inline("if"),
        TokenKind::TImport => FlowSmolStr::new_inline("import"),
        TokenKind::TIn => FlowSmolStr::new_inline("in"),
        TokenKind::TInstanceof => FlowSmolStr::new_inline("instanceof"),
        TokenKind::TNew => FlowSmolStr::new_inline("new"),
        TokenKind::TReturn => FlowSmolStr::new_inline("return"),
        TokenKind::TSuper => FlowSmolStr::new_inline("super"),
        TokenKind::TSwitch => FlowSmolStr::new_inline("switch"),
        TokenKind::TThis => FlowSmolStr::new_inline("this"),
        TokenKind::TThrow => FlowSmolStr::new_inline("throw"),
        TokenKind::TTry => FlowSmolStr::new_inline("try"),
        TokenKind::TTypeof => FlowSmolStr::new_inline("typeof"),
        TokenKind::TVar => FlowSmolStr::new_inline("var"),
        TokenKind::TVoid => FlowSmolStr::new_inline("void"),
        TokenKind::TWhile => FlowSmolStr::new_inline("while"),
        TokenKind::TWith => FlowSmolStr::new_inline("with"),
        TokenKind::TYield => FlowSmolStr::new_inline("yield"),
        /* FutureReservedWord */
        TokenKind::TEnum => FlowSmolStr::new_inline("enum"),
        TokenKind::TLet => FlowSmolStr::new_inline("let"),
        TokenKind::TStatic => FlowSmolStr::new_inline("static"),
        TokenKind::TInterface => FlowSmolStr::new_inline("interface"),
        TokenKind::TImplements => FlowSmolStr::new_inline("implements"),
        TokenKind::TPackage => FlowSmolStr::new_inline("package"),
        TokenKind::TPrivate => FlowSmolStr::new_inline("private"),
        TokenKind::TProtected => FlowSmolStr::new_inline("protected"),
        TokenKind::TPublic => FlowSmolStr::new_inline("public"),
        /* NullLiteral */
        TokenKind::TNull => FlowSmolStr::new_inline("null"),
        /* BooleanLiteral */
        TokenKind::TTrue => FlowSmolStr::new_inline("true"),
        TokenKind::TFalse => FlowSmolStr::new_inline("false"),
        /* Flow-specific stuff */
        TokenKind::TAsserts => FlowSmolStr::new_inline("asserts"),
        TokenKind::TImplies => FlowSmolStr::new_inline("implies"),
        TokenKind::TIs => FlowSmolStr::new_inline("is"),
        TokenKind::TDeclare => FlowSmolStr::new_inline("declare"),
        TokenKind::TType => FlowSmolStr::new_inline("type"),
        TokenKind::TOpaque => FlowSmolStr::new_inline("opaque"),
        TokenKind::TAnyType => FlowSmolStr::new_inline("any"),
        TokenKind::TMatch => FlowSmolStr::new_inline("match"),
        TokenKind::TRecord => FlowSmolStr::new_inline("record"),
        TokenKind::TMixedType => FlowSmolStr::new_inline("mixed"),
        TokenKind::TEmptyType => FlowSmolStr::new_inline("empty"),
        TokenKind::TBooleanType(BoolOrBoolean::Bool) => FlowSmolStr::new_inline("bool"),
        TokenKind::TBooleanType(BoolOrBoolean::Boolean) => FlowSmolStr::new_inline("boolean"),
        TokenKind::TNumberType => FlowSmolStr::new_inline("number"),
        TokenKind::TBigintType => FlowSmolStr::new_inline("bigint"),
        TokenKind::TStringType => FlowSmolStr::new_inline("string"),
        TokenKind::TVoidType => FlowSmolStr::new_inline("void"),
        TokenKind::TSymbolType => FlowSmolStr::new_inline("symbol"),
        TokenKind::TUnknownType => FlowSmolStr::new_inline("unknown"),
        TokenKind::TNeverType => FlowSmolStr::new_inline("never"),
        TokenKind::TUndefinedType => FlowSmolStr::new_inline("undefined"),
        TokenKind::TKeyof => FlowSmolStr::new_inline("keyof"),
        TokenKind::TReadonly => FlowSmolStr::new_inline("readonly"),
        TokenKind::TWriteonly => FlowSmolStr::new_inline("writeonly"),
        TokenKind::TInfer => FlowSmolStr::new_inline("infer"),
        /* Contextual stuff */
        TokenKind::TOf => FlowSmolStr::new_inline("of"),
        TokenKind::TAsync => FlowSmolStr::new_inline("async"),
        /* punctuators, types, literals, etc are not identifiers */
        _ => {
            env.error_unexpected(Some("an identifier".to_owned()))?;
            FlowSmolStr::new_inline("")
        }
    };
    let name = name.dupe();
    eat::token(env)?;
    Ok(name)
}

// IdentifierName - https://tc39.github.io/ecma262/#prod-IdentifierName
pub(super) fn identifier_name<'a>(
    env: &mut ParserEnv<'a>,
) -> Result<Identifier<Loc, Loc>, Rollback> {
    let loc = peek::loc(env).dupe();
    let leading = peek::comments(env);
    let name = identifier_name_raw(env)?;
    let trailing = eat::trailing_comments(env);
    let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
    Ok(Identifier::new(IdentifierInner {
        loc,
        name,
        comments,
    }))
}

// PrivateIdentifier - https://tc39.es/ecma262/#prod-PrivateIdentifier
//
// N.B.: whitespace, line terminators, and comments are not allowed
// between the # and IdentifierName because PrivateIdentifier is a
// CommonToken which is considered a single token. See also
// https://tc39.es/ecma262/#prod-InputElementDiv
pub(super) fn private_identifier<'a>(
    env: &mut ParserEnv<'a>,
) -> Result<PrivateName<Loc>, Rollback> {
    let start_loc = peek::loc(env).dupe();
    let leading = peek::comments(env);
    expect::token(env, TokenKind::TPound)?;
    let name_loc = peek::loc(env).dupe();
    let name = identifier_name_raw(env)?;
    let trailing = eat::trailing_comments(env);
    let comments = ast_utils::mk_comments_opt(Some(leading.into()), Some(trailing.into()));
    let loc = Loc::between(&start_loc, &name_loc);
    if start_loc.end != name_loc.start {
        env.error_at(loc.dupe(), ParseError::WhitespaceInPrivateName)?;
    }
    Ok(PrivateName {
        loc,
        name,
        comments,
    })
}

// The operation IsSimpleParamterList
// https://tc39.es/ecma262/#sec-static-semantics-issimpleparameterlist
pub(super) fn is_simple_parameter_list(params: &function::Params<Loc, Loc>) -> bool {
    let is_simple_param = |param: &function::Param<Loc, Loc>| -> bool {
        match param {
            function::Param::RegularParam {
                loc: _,
                argument: pattern::Pattern::Identifier { .. },
                default: None,
            } => true,
            _ => false,
        }
    };
    let function::Params {
        loc: _,
        params,
        rest,
        comments: _,
        this_: _,
    } = params;
    rest.is_none() && params.iter().all(is_simple_param)
}

// The abstract operation IsLabelledFunction
// https://tc39.github.io/ecma262/#sec-islabelledfunction
pub(super) fn is_labelled_function(stmt: &statement::Statement<Loc, Loc>) -> bool {
    match stmt.deref() {
        statement::StatementInner::Labeled { loc: _, inner } => match &*inner.body {
            statement::StatementInner::FunctionDeclaration { .. } => true,
            _ => is_labelled_function(&inner.body),
        },
        _ => false,
    }
}

/* https://tc39.es/ecma262/#sec-exports-static-semantics-early-errors */
pub(super) fn assert_identifier_name_is_identifier(
    restricted_error: Option<ParseError>,
    env: &mut ParserEnv,
    id: &Identifier<Loc, Loc>,
) -> Result<(), Rollback> {
    let IdentifierInner {
        loc,
        name,
        comments: _,
    } = id.deref();
    match name.as_str() {
        "let" if env.no_let() => {
            env.error_at(
                loc.dupe(),
                ParseError::Unexpected(TokenKind::quote_token_value(name)),
            )?;
        }
        "await" => {
            /* `allow_await` means that `await` is allowed to be a keyword,
            which makes it illegal to use as an identifier.
            https://tc39.github.io/ecma262/#sec-identifiers-static-semantics-early-errors */
            if env.allow_await() {
                env.error_at(loc.dupe(), ParseError::AwaitAsIdentifierReference)?;
            }
        }
        "yield" => {
            /* `allow_yield` means that `yield` is allowed to be a keyword,
            which makes it illegal to use as an identifier.
            https://tc39.github.io/ecma262/#sec-identifiers-static-semantics-early-errors */
            if env.allow_yield() {
                env.error_at(loc.dupe(), ParseError::UnexpectedReserved)?;
            } else {
                env.strict_error_at((loc.dupe(), ParseError::StrictReservedWord))?;
            }
        }
        _ if is_strict_reserved(name) => {
            env.strict_error_at((loc.dupe(), ParseError::StrictReservedWord))?;
        }
        _ if is_reserved(name) => {
            env.error_at(loc.dupe(), ParseError::UnexpectedReserved)?;
        }
        /* In ambient contexts (e.g. declaration files), "eval" and "arguments" are
        allowed as identifiers since they are type-level declarations, not runtime code. */
        _ => match restricted_error {
            Some(err) if is_restricted(name) && !env.in_ambient_context() => {
                env.strict_error_at((loc.dupe(), err))?;
            }
            _ => {}
        },
    }
    Ok(())
}

pub(super) fn with_loc<T, F>(
    start_loc: Option<Loc>,
    env: &mut ParserEnv,
    f: F,
) -> Result<(Loc, T), Rollback>
where
    F: FnOnce(&mut ParserEnv) -> Result<T, Rollback>,
{
    let start_loc = start_loc.unwrap_or_else(|| peek::loc(env).dupe());
    let result = f(env)?;
    let loc = match env.last_loc() {
        Some(end_loc) => Loc::between(&start_loc, end_loc),
        None => start_loc,
    };
    Ok((loc, result))
}

pub(super) fn with_loc_opt<T, F>(
    start_loc: Option<Loc>,
    env: &mut ParserEnv,
    f: F,
) -> Result<Option<(Loc, T)>, Rollback>
where
    F: FnOnce(&mut ParserEnv) -> Result<Option<T>, Rollback>,
{
    match with_loc(start_loc, env, f)? {
        (loc, Some(x)) => Ok(Some((loc, x))),
        (_, None) => Ok(None),
    }
}

pub(super) fn with_loc_extra<T, U, F>(
    start_loc: Option<Loc>,
    env: &mut ParserEnv,
    f: F,
) -> Result<((Loc, T), U), Rollback>
where
    F: FnOnce(&mut ParserEnv) -> Result<(T, U), Rollback>,
{
    let (loc, (x, extra)) = with_loc(start_loc, env, f)?;
    Ok(((loc, x), extra))
}

pub(super) fn is_start_of_type_guard(env: &mut ParserEnv) -> bool {
    /* Parse the identifier part as normal code, since this can be any name that
     * a parameter can be. */
    eat::push_lex_mode(env, LexMode::Normal);
    let token_1_is_identifier_name = peek::ith_is_identifier_name(env, 0);
    let token_1 = peek::token(env).clone();
    eat::pop_lex_mode(env);
    if !token_1_is_identifier_name {
        return false;
    }
    let token_2 = peek::ith_token(env, 1).clone();
    match (token_1, token_2) {
        (TokenKind::TIdentifier { raw, .. }, TokenKind::TIdentifier { .. } | TokenKind::TThis)
            if raw == "asserts" =>
        {
            true
        }
        (TokenKind::TIdentifier { raw, .. }, TokenKind::TIdentifier { .. } | TokenKind::TThis)
            if raw == "implies" =>
        {
            true
        }
        (_, TokenKind::TIs) => true,
        (_, TokenKind::TIdentifier { raw, .. }) if raw == "is" => true,
        _ => false,
    }
}

pub(super) fn reparse_arguments_as_match_argument(
    env: &mut ParserEnv,
    args: expression::ArgList<Loc, Loc>,
) -> Result<expression::Expression<Loc, Loc>, Rollback> {
    let expression::ArgList {
        loc: args_loc,
        arguments,
        ..
    } = args;
    if arguments.is_empty() {
        env.error_at(args_loc.dupe(), ParseError::MatchEmptyArgument)?;
    }
    let mut filtered_args = Vec::new();
    for arg in arguments.iter() {
        match arg {
            expression::ExpressionOrSpread::Spread(spread) => {
                env.error_at(spread.loc.dupe(), ParseError::MatchSpreadArgument)?;
            }
            expression::ExpressionOrSpread::Expression(e) => filtered_args.push(e.clone()),
        }
    }
    if filtered_args.len() == 1 {
        Ok(filtered_args.pop().unwrap())
    } else {
        Ok(expression::Expression::new(ExpressionInner::Sequence {
            loc: args_loc,
            inner: Arc::new(expression::Sequence {
                expressions: filtered_args.into(),
                comments: None,
            }),
        }))
    }
}
