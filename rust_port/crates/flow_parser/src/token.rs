/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::loc::Loc;

/// `bool` and `boolean` are equivalent annotations, but we need to track
/// which one was used for when it might be an identifier, as in
/// `(bool: boolean) => void`. It's lexed as two T_BOOLEAN_TYPEs, then the
/// first one is converted into an identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoolOrBoolean {
    Bool,
    Boolean,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumberType {
    Binary,
    LegacyOctal,
    /// NonOctalDecimalIntegerLiteral in Annex B
    LegacyNonOctal,
    Octal,
    Hex,
    Normal(f64),
}

/// Parse a hex string (without 0x/0X prefix, may contain underscores) to f64.
/// Handles large values that overflow i64/u64 by accumulating as f64.
pub fn parse_hex_to_f64(hex_digits: &str) -> f64 {
    let without_underscore: String = hex_digits.replace('_', "");
    // Try u64 first for better precision on values that fit
    if let Ok(v) = u64::from_str_radix(&without_underscore, 16) {
        return v as f64;
    }
    // For values that overflow u64, accumulate as f64
    // This loses precision for very large integers, which matches JavaScript behavior
    let mut result: f64 = 0.0;
    for c in without_underscore.chars() {
        let digit = c.to_digit(16).unwrap_or(0) as f64;
        result = result * 16.0 + digit;
    }
    result
}

/// Parse a binary string (without 0b/0B prefix, may contain underscores) to f64.
/// Handles large values that overflow i64/u64 by accumulating as f64.
pub fn parse_binary_to_f64(binary_digits: &str) -> f64 {
    let without_underscore: String = binary_digits.replace('_', "");
    // Try u64 first for better precision on values that fit
    if let Ok(v) = u64::from_str_radix(&without_underscore, 2) {
        return v as f64;
    }
    // For values that overflow u64, accumulate as f64
    // This loses precision for very large integers, which matches JavaScript behavior
    let mut result: f64 = 0.0;
    for c in without_underscore.chars() {
        let digit = c.to_digit(2).unwrap_or(0) as f64;
        result = result * 2.0 + digit;
    }
    result
}

/// Parse an octal string (without 0o/0O prefix, may contain underscores) to f64.
/// Handles large values that overflow i64/u64 by accumulating as f64.
pub fn parse_octal_to_f64(octal_digits: &str) -> f64 {
    let without_underscore: String = octal_digits.replace('_', "");
    // Try u64 first for better precision on values that fit
    if let Ok(v) = u64::from_str_radix(&without_underscore, 8) {
        return v as f64;
    }
    // For values that overflow u64, accumulate as f64
    // This loses precision for very large integers, which matches JavaScript behavior
    let mut result: f64 = 0.0;
    for c in without_underscore.chars() {
        let digit = c.to_digit(8).unwrap_or(0) as f64;
        result = result * 8.0 + digit;
    }
    result
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BigintType {
    BigBinary,
    BigOctal,
    BigNormal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplatePart {
    pub loc: Loc,
    pub value: FlowSmolStr,
    pub raw: FlowSmolStr,
    pub head: bool,
    pub tail: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    TNumber {
        kind: NumberType,
        raw: FlowSmolStr,
    },
    TBigint {
        kind: BigintType,
        raw: FlowSmolStr,
    },
    TString(Loc, FlowSmolStr, FlowSmolStr, bool), /* loc, value, raw, octal */
    TTemplatePart(TemplatePart),                  /* loc, value, raw, head, tail */
    TIdentifier {
        loc: Loc,
        value: FlowSmolStr,
        raw: FlowSmolStr,
    },
    TRegexp(Loc, FlowSmolStr, FlowSmolStr), /* /pattern/flags */
    /* Syntax */
    TLcurly,
    TRcurly,
    TLcurlybar,
    TRcurlybar,
    TLparen,
    TRparen,
    TLbracket,
    TRbracket,
    TSemicolon,
    TComma,
    TPeriod,
    TArrow,
    TEllipsis,
    TAt,
    TPound,
    /* Keywords */
    TFunction,
    TIf,
    TIn,
    TInstanceof,
    TReturn,
    TSwitch,
    TMatch,
    TRecord,
    TThis,
    TThrow,
    TTry,
    TVar,
    TWhile,
    TWith,
    TConst,
    TLet,
    TNull,
    TFalse,
    TTrue,
    TBreak,
    TCase,
    TCatch,
    TContinue,
    TDefault,
    TDo,
    TFinally,
    TFor,
    TClass,
    TExtends,
    TStatic,
    TElse,
    TNew,
    TDelete,
    TTypeof,
    TVoid,
    TEnum,
    TExport,
    TImport,
    TSuper,
    TImplements,
    TInterface,
    TPackage,
    TPrivate,
    TProtected,
    TPublic,
    TYield,
    TDebugger,
    TDeclare,
    TType,
    TOpaque,
    TOf,
    TAsync,
    TAwait,
    TChecks,
    /* Operators */
    TRshift3Assign,
    TRshiftAssign,
    TLshiftAssign,
    TBitXorAssign,
    TBitOrAssign,
    TBitAndAssign,
    TModAssign,
    TDivAssign,
    TMultAssign,
    TExpAssign,
    TMinusAssign,
    TPlusAssign,
    TNullishAssign,
    TAndAssign,
    TOrAssign,
    TAssign,
    TPlingPeriod,
    TPlingPling,
    TPling,
    TColon,
    TOr,
    TAnd,
    TBitOr,
    TBitXor,
    TBitAnd,
    TEqual,
    TNotEqual,
    TStrictEqual,
    TStrictNotEqual,
    TLessThanEqual,
    TGreaterThanEqual,
    TLessThan,
    TGreaterThan,
    TLshift,
    TRshift,
    TRshift3,
    TPlus,
    TMinus,
    TDiv,
    TMult,
    TExp,
    TMod,
    TNot,
    TBitNot,
    TIncr,
    TDecr,
    /* Extra tokens */
    TInterpreter(Loc, String),
    TError(String),
    TEof,
    /* JSX */
    TJsxIdentifier {
        raw: FlowSmolStr,
        loc: Loc,
    },
    TJsxChildText(Loc, FlowSmolStr, FlowSmolStr), /* loc, value, raw */
    TJsxQuoteText(Loc, FlowSmolStr, FlowSmolStr), /* loc, value, raw */
    /* Type primitives */
    TAnyType,
    TMixedType,
    TEmptyType,
    TBooleanType(BoolOrBoolean),
    TNumberType,
    TBigintType,
    TNumberSingletonType {
        kind: NumberType,
        value: f64,
        raw: FlowSmolStr,
    },
    TBigintSingletonType {
        kind: BigintType,
        value: Option<i64>,
        raw: FlowSmolStr,
    },
    TStringType,
    TVoidType,
    TSymbolType,
    TUnknownType,
    TNeverType,
    TUndefinedType,
    TKeyof,
    TReadonly,
    TWriteonly,
    TInfer,
    TIs,
    TAsserts,
    TImplies,
    TRendersQuestion,
    TRendersStar,
}

impl TokenKind {
    /// Pretty printer (pretty?)
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::TNumber { .. } => "T_NUMBER",
            Self::TBigint { .. } => "T_BIGINT",
            Self::TString(..) => "T_STRING",
            Self::TTemplatePart(..) => "T_TEMPLATE_PART",
            Self::TIdentifier { .. } => "T_IDENTIFIER",
            Self::TRegexp(..) => "T_REGEXP",
            Self::TFunction => "T_FUNCTION",
            Self::TIf => "T_IF",
            Self::TIn => "T_IN",
            Self::TInstanceof => "T_INSTANCEOF",
            Self::TReturn => "T_RETURN",
            Self::TSwitch => "T_SWITCH",
            Self::TMatch => "T_MATCH",
            Self::TRecord => "T_RECORD",
            Self::TThis => "T_THIS",
            Self::TThrow => "T_THROW",
            Self::TTry => "T_TRY",
            Self::TVar => "T_VAR",
            Self::TWhile => "T_WHILE",
            Self::TWith => "T_WITH",
            Self::TConst => "T_CONST",
            Self::TLet => "T_LET",
            Self::TNull => "T_NULL",
            Self::TFalse => "T_FALSE",
            Self::TTrue => "T_TRUE",
            Self::TBreak => "T_BREAK",
            Self::TCase => "T_CASE",
            Self::TCatch => "T_CATCH",
            Self::TContinue => "T_CONTINUE",
            Self::TDefault => "T_DEFAULT",
            Self::TDo => "T_DO",
            Self::TFinally => "T_FINALLY",
            Self::TFor => "T_FOR",
            Self::TClass => "T_CLASS",
            Self::TExtends => "T_EXTENDS",
            Self::TStatic => "T_STATIC",
            Self::TElse => "T_ELSE",
            Self::TNew => "T_NEW",
            Self::TDelete => "T_DELETE",
            Self::TTypeof => "T_TYPEOF",
            Self::TVoid => "T_VOID",
            Self::TEnum => "T_ENUM",
            Self::TExport => "T_EXPORT",
            Self::TImport => "T_IMPORT",
            Self::TSuper => "T_SUPER",
            Self::TImplements => "T_IMPLEMENTS",
            Self::TInterface => "T_INTERFACE",
            Self::TPackage => "T_PACKAGE",
            Self::TPrivate => "T_PRIVATE",
            Self::TProtected => "T_PROTECTED",
            Self::TPublic => "T_PUBLIC",
            Self::TYield => "T_YIELD",
            Self::TDebugger => "T_DEBUGGER",
            Self::TDeclare => "T_DECLARE",
            Self::TType => "T_TYPE",
            Self::TOpaque => "T_OPAQUE",
            Self::TOf => "T_OF",
            Self::TAsync => "T_ASYNC",
            Self::TAwait => "T_AWAIT",
            Self::TChecks => "T_CHECKS",
            Self::TLcurly => "T_LCURLY",
            Self::TRcurly => "T_RCURLY",
            Self::TLcurlybar => "T_LCURLYBAR",
            Self::TRcurlybar => "T_RCURLYBAR",
            Self::TLparen => "T_LPAREN",
            Self::TRparen => "T_RPAREN",
            Self::TLbracket => "T_LBRACKET",
            Self::TRbracket => "T_RBRACKET",
            Self::TSemicolon => "T_SEMICOLON",
            Self::TComma => "T_COMMA",
            Self::TPeriod => "T_PERIOD",
            Self::TArrow => "T_ARROW",
            Self::TEllipsis => "T_ELLIPSIS",
            Self::TAt => "T_AT",
            Self::TPound => "T_POUND",
            Self::TRshift3Assign => "T_RSHIFT3_ASSIGN",
            Self::TRshiftAssign => "T_RSHIFT_ASSIGN",
            Self::TLshiftAssign => "T_LSHIFT_ASSIGN",
            Self::TBitXorAssign => "T_BIT_XOR_ASSIGN",
            Self::TBitOrAssign => "T_BIT_OR_ASSIGN",
            Self::TBitAndAssign => "T_BIT_AND_ASSIGN",
            Self::TModAssign => "T_MOD_ASSIGN",
            Self::TDivAssign => "T_DIV_ASSIGN",
            Self::TMultAssign => "T_MULT_ASSIGN",
            Self::TExpAssign => "T_EXP_ASSIGN",
            Self::TMinusAssign => "T_MINUS_ASSIGN",
            Self::TPlusAssign => "T_PLUS_ASSIGN",
            Self::TNullishAssign => "T_NULLISH_ASSIGN",
            Self::TAndAssign => "T_AND_ASSIGN",
            Self::TOrAssign => "T_OR_ASSIGN",
            Self::TAssign => "T_ASSIGN",
            Self::TPlingPeriod => "T_PLING_PERIOD",
            Self::TPlingPling => "T_PLING_PLING",
            Self::TPling => "T_PLING",
            Self::TColon => "T_COLON",
            Self::TOr => "T_OR",
            Self::TAnd => "T_AND",
            Self::TBitOr => "T_BIT_OR",
            Self::TBitXor => "T_BIT_XOR",
            Self::TBitAnd => "T_BIT_AND",
            Self::TEqual => "T_EQUAL",
            Self::TNotEqual => "T_NOT_EQUAL",
            Self::TStrictEqual => "T_STRICT_EQUAL",
            Self::TStrictNotEqual => "T_STRICT_NOT_EQUAL",
            Self::TLessThanEqual => "T_LESS_THAN_EQUAL",
            Self::TGreaterThanEqual => "T_GREATER_THAN_EQUAL",
            Self::TLessThan => "T_LESS_THAN",
            Self::TGreaterThan => "T_GREATER_THAN",
            Self::TLshift => "T_LSHIFT",
            Self::TRshift => "T_RSHIFT",
            Self::TRshift3 => "T_RSHIFT3",
            Self::TPlus => "T_PLUS",
            Self::TMinus => "T_MINUS",
            Self::TDiv => "T_DIV",
            Self::TMult => "T_MULT",
            Self::TExp => "T_EXP",
            Self::TMod => "T_MOD",
            Self::TNot => "T_NOT",
            Self::TBitNot => "T_BIT_NOT",
            Self::TIncr => "T_INCR",
            Self::TDecr => "T_DECR",
            Self::TKeyof => "T_KEYOF",
            Self::TReadonly => "T_READONLY",
            Self::TWriteonly => "T_WRITEONLY",
            Self::TInfer => "T_INFER",
            Self::TIs => "T_IS",
            Self::TAsserts => "T_ASSERTS",
            Self::TImplies => "T_IMPLIES",
            Self::TRendersQuestion => "T_RENDERS_QUESTION",
            Self::TRendersStar => "T_RENDERS_QUESTION",
            /* Extra tokens */
            Self::TInterpreter(..) => "T_INTERPRETER",
            Self::TError(..) => "T_ERROR",
            Self::TEof => "T_EOF",
            Self::TJsxIdentifier { .. } => "T_JSX_IDENTIFIER",
            Self::TJsxChildText(..) => "T_JSX_TEXT",
            Self::TJsxQuoteText(..) => "T_JSX_TEXT",
            /* Type primitives */
            Self::TAnyType => "T_ANY_TYPE",
            Self::TMixedType => "T_MIXED_TYPE",
            Self::TEmptyType => "T_EMPTY_TYPE",
            Self::TBooleanType(..) => "T_BOOLEAN_TYPE",
            Self::TNumberType => "T_NUMBER_TYPE",
            Self::TBigintType => "T_BIGINT_TYPE",
            Self::TNumberSingletonType { .. } => "T_NUMBER_SINGLETON_TYPE",
            Self::TBigintSingletonType { .. } => "T_BIGINT_SINGLETON_TYPE",
            Self::TStringType => "T_STRING_TYPE",
            Self::TVoidType => "T_VOID_TYPE",
            Self::TSymbolType => "T_SYMBOL_TYPE",
            Self::TUnknownType => "T_UNKNOWN_TYPE",
            Self::TNeverType => "T_NEVER_TYPE",
            Self::TUndefinedType => "T_UNDEFINED_TYPE",
        }
    }

    pub fn to_value(&self) -> String {
        match self {
            Self::TNumber { raw, .. } => raw.as_str().to_owned(),
            Self::TBigint { raw, .. } => raw.as_str().to_owned(),
            Self::TString(_, _, raw, _) => raw.as_str().to_owned(),
            Self::TTemplatePart(TemplatePart {
                loc: _,
                value: _,
                raw,
                head,
                tail,
            }) => {
                if *head && *tail {
                    format!("`{}`", raw)
                } else if *head {
                    format!("`{}${{", raw)
                } else if *tail {
                    format!("}}{}`", raw)
                } else {
                    format!("${{{}}}", raw)
                }
            }
            Self::TIdentifier { raw, .. } => raw.as_str().to_owned(),
            Self::TRegexp(_, pattern, flags) => format!("/{}/{}", pattern, flags),
            Self::TLcurly => "{".to_owned(),
            Self::TRcurly => "}".to_owned(),
            Self::TLcurlybar => "{|".to_owned(),
            Self::TRcurlybar => "|}".to_owned(),
            Self::TLparen => "(".to_owned(),
            Self::TRparen => ")".to_owned(),
            Self::TLbracket => "[".to_owned(),
            Self::TRbracket => "]".to_owned(),
            Self::TSemicolon => ";".to_owned(),
            Self::TComma => ",".to_owned(),
            Self::TPeriod => ".".to_owned(),
            Self::TArrow => "=>".to_owned(),
            Self::TEllipsis => "...".to_owned(),
            Self::TAt => "@".to_owned(),
            Self::TPound => "#".to_owned(),
            Self::TFunction => "function".to_owned(),
            Self::TIf => "if".to_owned(),
            Self::TIn => "in".to_owned(),
            Self::TInstanceof => "instanceof".to_owned(),
            Self::TReturn => "return".to_owned(),
            Self::TSwitch => "switch".to_owned(),
            Self::TMatch => "match".to_owned(),
            Self::TRecord => "record".to_owned(),
            Self::TThis => "this".to_owned(),
            Self::TThrow => "throw".to_owned(),
            Self::TTry => "try".to_owned(),
            Self::TVar => "var".to_owned(),
            Self::TWhile => "while".to_owned(),
            Self::TWith => "with".to_owned(),
            Self::TConst => "const".to_owned(),
            Self::TLet => "let".to_owned(),
            Self::TNull => "null".to_owned(),
            Self::TFalse => "false".to_owned(),
            Self::TTrue => "true".to_owned(),
            Self::TBreak => "break".to_owned(),
            Self::TCase => "case".to_owned(),
            Self::TCatch => "catch".to_owned(),
            Self::TContinue => "continue".to_owned(),
            Self::TDefault => "default".to_owned(),
            Self::TDo => "do".to_owned(),
            Self::TFinally => "finally".to_owned(),
            Self::TFor => "for".to_owned(),
            Self::TClass => "class".to_owned(),
            Self::TExtends => "extends".to_owned(),
            Self::TStatic => "static".to_owned(),
            Self::TElse => "else".to_owned(),
            Self::TNew => "new".to_owned(),
            Self::TDelete => "delete".to_owned(),
            Self::TTypeof => "typeof".to_owned(),
            Self::TVoid => "void".to_owned(),
            Self::TEnum => "enum".to_owned(),
            Self::TExport => "export".to_owned(),
            Self::TImport => "import".to_owned(),
            Self::TSuper => "super".to_owned(),
            Self::TImplements => "implements".to_owned(),
            Self::TInterface => "interface".to_owned(),
            Self::TPackage => "package".to_owned(),
            Self::TPrivate => "private".to_owned(),
            Self::TProtected => "protected".to_owned(),
            Self::TPublic => "public".to_owned(),
            Self::TYield => "yield".to_owned(),
            Self::TDebugger => "debugger".to_owned(),
            Self::TDeclare => "declare".to_owned(),
            Self::TType => "type".to_owned(),
            Self::TOpaque => "opaque".to_owned(),
            Self::TOf => "of".to_owned(),
            Self::TAsync => "async".to_owned(),
            Self::TAwait => "await".to_owned(),
            Self::TChecks => "%checks".to_owned(),
            Self::TRshift3Assign => ">>>=".to_owned(),
            Self::TRshiftAssign => ">>=".to_owned(),
            Self::TLshiftAssign => "<<=".to_owned(),
            Self::TBitXorAssign => "^=".to_owned(),
            Self::TBitOrAssign => "|=".to_owned(),
            Self::TBitAndAssign => "&=".to_owned(),
            Self::TModAssign => "%=".to_owned(),
            Self::TDivAssign => "/=".to_owned(),
            Self::TMultAssign => "*=".to_owned(),
            Self::TExpAssign => "**=".to_owned(),
            Self::TMinusAssign => "-=".to_owned(),
            Self::TPlusAssign => "+=".to_owned(),
            Self::TNullishAssign => "??=".to_owned(),
            Self::TAndAssign => "&&=".to_owned(),
            Self::TOrAssign => "||=".to_owned(),
            Self::TAssign => "=".to_owned(),
            Self::TPlingPeriod => "?.".to_owned(),
            Self::TPlingPling => "??".to_owned(),
            Self::TPling => "?".to_owned(),
            Self::TColon => ":".to_owned(),
            Self::TOr => "||".to_owned(),
            Self::TAnd => "&&".to_owned(),
            Self::TBitOr => "|".to_owned(),
            Self::TBitXor => "^".to_owned(),
            Self::TBitAnd => "&".to_owned(),
            Self::TEqual => "==".to_owned(),
            Self::TNotEqual => "!=".to_owned(),
            Self::TStrictEqual => "===".to_owned(),
            Self::TStrictNotEqual => "!==".to_owned(),
            Self::TLessThanEqual => "<=".to_owned(),
            Self::TGreaterThanEqual => ">=".to_owned(),
            Self::TLessThan => "<".to_owned(),
            Self::TGreaterThan => ">".to_owned(),
            Self::TLshift => "<<".to_owned(),
            Self::TRshift => ">>".to_owned(),
            Self::TRshift3 => ">>>".to_owned(),
            Self::TPlus => "+".to_owned(),
            Self::TMinus => "-".to_owned(),
            Self::TDiv => "/".to_owned(),
            Self::TMult => "*".to_owned(),
            Self::TExp => "**".to_owned(),
            Self::TMod => "%".to_owned(),
            Self::TNot => "!".to_owned(),
            Self::TBitNot => "~".to_owned(),
            Self::TIncr => "++".to_owned(),
            Self::TDecr => "--".to_owned(),
            Self::TKeyof => "keyof".to_owned(),
            Self::TReadonly => "readonly".to_owned(),
            Self::TWriteonly => "writeonly".to_owned(),
            Self::TInfer => "infer".to_owned(),
            Self::TIs => "is".to_owned(),
            Self::TAsserts => "asserts".to_owned(),
            Self::TImplies => "implies".to_owned(),
            Self::TRendersQuestion => "renders?".to_owned(),
            Self::TRendersStar => "renders*".to_owned(),
            /* Extra tokens */
            Self::TInterpreter(_, str) => str.to_owned(),
            Self::TError(raw) => raw.to_owned(),
            Self::TEof => "".to_owned(),
            Self::TJsxIdentifier { raw, .. } => raw.as_str().to_owned(),
            Self::TJsxChildText(_, _, raw) => raw.as_str().to_owned(),
            Self::TJsxQuoteText(_, _, raw) => raw.as_str().to_owned(),
            /* Type primitives */
            Self::TAnyType => "any".to_owned(),
            Self::TMixedType => "mixed".to_owned(),
            Self::TEmptyType => "empty".to_owned(),
            Self::TBooleanType(kind) => match kind {
                BoolOrBoolean::Bool => "bool".to_owned(),
                BoolOrBoolean::Boolean => "boolean".to_owned(),
            },
            Self::TNumberType => "number".to_owned(),
            Self::TBigintType => "bigint".to_owned(),
            Self::TNumberSingletonType { raw, .. } => raw.as_str().to_owned(),
            Self::TBigintSingletonType { raw, .. } => raw.as_str().to_owned(),
            Self::TStringType => "string".to_owned(),
            Self::TVoidType => "void".to_owned(),
            Self::TSymbolType => "symbol".to_owned(),
            Self::TUnknownType => "unknown".to_owned(),
            Self::TNeverType => "never".to_owned(),
            Self::TUndefinedType => "undefined".to_owned(),
        }
    }

    pub fn explanation(&self, use_article: bool) -> String {
        let (value, article) = match self {
            Self::TNumberSingletonType { .. } | Self::TNumber { .. } => ("number".to_owned(), "a"),
            Self::TBigintSingletonType { .. } | Self::TBigint { .. } => ("bigint".to_owned(), "a"),
            Self::TJsxChildText(..) | Self::TJsxQuoteText(..) | Self::TString(..) => {
                ("string".to_owned(), "a")
            }
            Self::TTemplatePart(..) => ("template literal part".to_owned(), "a"),
            Self::TJsxIdentifier { .. } | Self::TIdentifier { .. } => {
                ("identifier".to_owned(), "an")
            }
            Self::TRegexp(..) => ("regexp".to_owned(), "a"),
            Self::TEof => ("end of input".to_owned(), "the"),
            _ => (Self::quote_token_value(&self.to_value()), "the"),
        };

        if use_article {
            format!("{} {}", article, value)
        } else {
            value
        }
    }

    pub fn quote_token_value(value: &str) -> String {
        format!("token `{}`", value)
    }

    pub fn explanation_of_token(token: &TokenKind) -> String {
        token.explanation(false)
    }

    pub fn explanation_of_token_with_article(token: &TokenKind) -> String {
        token.explanation(true)
    }
}
