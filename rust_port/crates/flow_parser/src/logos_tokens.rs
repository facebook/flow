/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use logos::Logos;

// JS lexing is very context sensitive.
// This file contains tokens that are valid for various contexts.
// We can switch context using `logos::Lexer::morph.

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub(super) enum CommentToken {
    #[token("*/")]
    EndComment,
    #[token("*-/")]
    EndCommentBar,
    #[regex(r"(\r|\n|\r\n|\u2028|\u2029)")]
    LineTerminatorSequence,
    #[regex(r".", priority = 1)]
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub(super) enum TemplatePartToken {
    #[token("`")]
    Backtick,
    #[token("${")]
    SubstitutionStart,
    #[token("\\")]
    Backslash,
    #[token("\r\n")]
    Crlf,
    #[regex(r"[\n\r]")]
    Newline,
    #[regex(r".", priority = 1)]
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub enum MainToken {
    #[regex(r"(\r\n|\r|\n|\u2028|\u2029)")]
    LineTerminatorSequence,
    #[regex(r"[ \t\u000b\u000c\u0020\u00a0\ufeff\u1680\u2000-\u200a\u202f\u205f\u3000]+")]
    Whitespace,
    #[token("/*")]
    BlockCommentStart,
    #[token("//")]
    LineCommentStart,
    #[token("#!")]
    Shebang,
    #[token("'")]
    SingleQuote,
    #[token("\"")]
    DoubleQuote,
    #[token("`")]
    Backtick,
    #[regex(r"0[bB][01](_?[01])*n")]
    BinBigint,
    #[regex(r"0[bB][01](_?[01])*")]
    BinNumber,
    #[regex(r"0[oO][0-7](_?[0-7])*n")]
    OctBigint,
    #[regex(r"0[oO][0-7](_?[0-7])*")]
    OctNumber,
    #[regex(r"0[0-7]*[89][0-9]*")]
    LegacyNonOctNumber,
    #[regex(r"0[0-7]+")]
    LegacyOctNumber,
    #[regex(r"0[xX][0-9a-fA-F](_?[0-9a-fA-F])*n")]
    HexBigint,
    #[regex(r"0[xX][0-9a-fA-F](_?[0-9a-fA-F])*")]
    HexNumber,
    #[regex(r"(0[1-9]*|[1-9](_?[0-9])*)")]
    WholeNumber,
    #[regex(r"(0[1-9]*|[1-9](_?[0-9])*)\.([0-9](_?[0-9])*)?")]
    FloatNumberWithWholePart,
    #[regex(r"\.([0-9](_?[0-9])*)")]
    FloatNumberWithOnlyDecimals,
    #[token("@@iterator")]
    AtAtIterator,
    #[token("@@asyncIterator")]
    AtAtAsyncIterator,
    #[token("@@dispose")]
    AtAtDispose,
    #[token("@@asyncDispose")]
    AtAtAsyncDispose,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("...")]
    Ellipsis,
    #[token(".")]
    Period,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("?")]
    Pling,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("===")]
    StrictEqual,
    #[token("!==")]
    StrictNotEqual,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("%=")]
    ModAssign,
    #[token("^=")]
    BitXorAssign,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mult,
    #[token("%")]
    Mod,
    #[token("|")]
    BitOr,
    #[token("&")]
    BitAnd,
    #[token("^")]
    BitXor,
    #[token("!")]
    Not,
    #[token("~")]
    BitNot,
    #[token("=")]
    Assign,
    #[token("=>")]
    Arrow,
    #[token("/=")]
    DivAssign,
    #[token("/")]
    Div,
    #[token("@")]
    At,
    #[token("#")]
    Pound,
    #[token("\\")]
    Backslash,
    #[regex(r"([$_a-zA-Z]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\})([$_a-zA-Z0-9]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\})*")]
    JsIdStart,
    #[regex(r".", priority = 1)]
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub(super) enum RegexpClassToken {
    #[token("\\\\")]
    EscapedBackslash,
    #[token("\\]")]
    EscapedClosingBracket,
    #[token("]")]
    ClosingBracket,
    #[regex(r"(\r|\n|\r\n|\u2028|\u2029)")]
    LineTerminatorSequence,
    #[regex(r".", priority = 1)]
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub(super) enum RegexpBodyToken {
    #[regex(r"\\(\r|\n|\r\n|\u2028|\u2029)", priority = 3)]
    BackslashFollowedByLineTerminator,
    #[regex(r"\\.", priority = 2)]
    BackslashFollowedByAny,
    #[regex(r"/[a-zA-Z_$]+")]
    SlashWithFlags,
    #[token("/")]
    Slash,
    #[token("[")]
    OpenBracket,
    #[regex(r"(\r|\n|\r\n|\u2028|\u2029)")]
    LineTerminatorSequence,
    #[regex(r".", priority = 1)]
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub(super) enum RegexpToken {
    #[regex(r"(\r\n|\r|\n|\u2028|\u2029)")]
    LineTerminatorSequence,
    #[regex(r"[ \t\u000b\u000c\u0020\u00a0\ufeff\u1680\u2000-\u200a\u202f\u205f\u3000]+")]
    Whitespace,
    #[token("//")]
    LineCommentStart,
    #[token("/*")]
    BlockCommentStart,
    #[token("/")]
    Slash,
    #[regex(r".", priority = 1)]
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub(super) enum JsxChildTextToken {
    #[token(">")]
    GreaterThan,
    #[token("}")]
    RCurly,
    #[regex(r"(\r|\n|\r\n|\u2028|\u2029)")]
    LineTerminatorSequence,
    #[regex(r"&#x[0-9a-fA-F]+;")]
    HexEntityRef,
    #[regex(r"&#[0-9]+;")]
    DecimalEntityRef,
    #[regex(r"&[a-zA-Z][a-zA-Z0-9]{1,7};")]
    NamedEntityRef,
    #[regex(r".", priority = 1)]
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub(super) enum JsxQuoteTextToken {
    #[token("'")]
    SingleQuote,
    #[token("\"")]
    DoubleQuote,
    #[regex(r"(\r|\n|\r\n|\u2028|\u2029)")]
    LineTerminatorSequence,
    #[regex(r"&#x[0-9a-fA-F]+;")]
    HexEntityRef,
    #[regex(r"&#[0-9]+;")]
    DecimalEntityRef,
    #[regex(r"&[a-zA-Z][a-zA-Z0-9]{1,7};")]
    NamedEntityRef,
    #[regex(r#"[^'"&\r\n\u2028\u2029]+"#, priority = 2)]
    Text,
    #[regex(r".", priority = 1)]
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub(super) enum JsxTagToken {
    #[regex(r"(\r\n|\r|\n|\u2028|\u2029)")]
    LineTerminatorSequence,
    #[regex(r"[ \t\u000b\u000c\u0020\u00a0\ufeff\u1680\u2000-\u200a\u202f\u205f\u3000]+")]
    Whitespace,
    #[token("//")]
    LineCommentStart,
    #[token("/*")]
    BlockCommentStart,
    #[token("<")]
    LessThan,
    #[token("/")]
    Div,
    #[token(">")]
    GreaterThan,
    #[token("{")]
    LCurly,
    #[token(":")]
    Colon,
    #[token(".")]
    Period,
    #[token("=")]
    Assign,
    #[token("'")]
    SingleQuote,
    #[token("\"")]
    DoubleQuote,
    #[regex(r"([$_a-zA-Z]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\})([$_a-zA-Z0-9\-]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\})*")]
    JsxIdStart,
    #[regex(r".", priority = 1)]
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub(super) enum TemplateTailToken {
    #[regex(r"(\r\n|\r|\n|\u2028|\u2029)")]
    LineTerminatorSequence,
    #[regex(r"[ \t\u000b\u000c\u0020\u00a0\ufeff\u1680\u2000-\u200a\u202f\u205f\u3000]+")]
    Whitespace,
    #[token("//")]
    LineCommentStart,
    #[token("/*")]
    BlockCommentStart,
    #[token("}")]
    RCurly,
    #[regex(r".", priority = 1)]
    Other,
}
