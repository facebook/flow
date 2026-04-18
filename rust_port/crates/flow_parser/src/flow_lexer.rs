/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::borrow::Cow;
use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::ast::Comment;
use crate::ast::CommentKind;
use crate::js_id_unicode;
use crate::js_id_unicode::is_valid_unicode_id;
use crate::lex_env::LexEnv;
use crate::lex_env::LexErrors;
use crate::loc::Loc;
use crate::loc::Position;
use crate::logos_tokens::CommentToken;
use crate::logos_tokens::JsxChildTextToken;
use crate::logos_tokens::JsxQuoteTextToken;
use crate::logos_tokens::JsxTagToken;
use crate::logos_tokens::MainToken;
use crate::logos_tokens::RegexpBodyToken;
use crate::logos_tokens::RegexpClassToken;
use crate::logos_tokens::RegexpToken;
use crate::logos_tokens::TemplatePartToken;
use crate::logos_tokens::TemplateTailToken;
use crate::parse_error::ParseError;
use crate::token::BigintType;
use crate::token::NumberType;
use crate::token::TemplatePart;
use crate::token::TokenKind;
use crate::token::parse_binary_to_f64;
use crate::token::parse_hex_to_f64;
use crate::token::parse_octal_to_f64;

static HEX_DIGIT: [char; 22] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C',
    'D', 'E', 'F',
];
static ASCII_ID_CONTINUE: [char; 63] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '$', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9',
];

/// Precondition: remainder is not empty
fn starts_with_id_continues(remainder: &str) -> Option<usize> {
    if remainder.starts_with(ASCII_ID_CONTINUE) {
        return Some(1);
    }
    if let Some(remainder) = remainder.strip_prefix("\\u") {
        // unicode escape: \uHexHexHexHex
        if let Some(remainder) = remainder.strip_prefix(HEX_DIGIT)
            && let Some(remainder) = remainder.strip_prefix(HEX_DIGIT)
            && let Some(remainder) = remainder.strip_prefix(HEX_DIGIT)
            && let Some(_remainder) = remainder.strip_prefix(HEX_DIGIT)
        {
            return Some(6);
        }
        // codepoint_escape: \u{Hex+}
        if let Some(mut remainder) = remainder.strip_prefix('{') {
            let mut bump_len = 3;
            while let Some(new_remainder) = remainder.strip_prefix(HEX_DIGIT) {
                bump_len += 1;
                remainder = new_remainder;
            }
            if remainder.starts_with('}') {
                return Some(bump_len + 1);
            }
        }
    }
    let ch = remainder.chars().next().unwrap();
    if is_valid_unicode_id(ch as u32) {
        Some(ch.len_utf8())
    } else {
        None
    }
}

/// Assuming that the first code point is already lexed
/// return true means that the whole remaining buffer is valid identifier
fn loop_id_continues(lexer: &mut logos::Lexer<MainToken>) -> bool {
    loop {
        let remainder = lexer.remainder();
        if remainder.is_empty() {
            return true;
        }
        if let Some(l) = starts_with_id_continues(remainder) {
            lexer.bump(l);
            continue;
        }
        return false;
    }
}

/// Assuming that the first code point is already lexed
fn loop_jsx_id_continues(lexer: &mut logos::Lexer<JsxTagToken>) {
    loop {
        let remainder = lexer.remainder();
        if remainder.is_empty() {
            return;
        }
        if remainder.starts_with('-') {
            lexer.bump(1);
            continue;
        }
        if let Some(l) = starts_with_id_continues(remainder) {
            lexer.bump(l);
            continue;
        }
        return;
    }
}

pub(super) fn bigint_strip_n(raw: &str) -> &str {
    let size = raw.len();
    if size != 0 && raw.as_bytes()[size - 1] == b'n' {
        &raw[0..size - 1]
    } else {
        raw
    }
}

fn mk_comment(
    env: &LexEnv,
    start: Position,
    end: Position,
    text: String,
    multiline: bool,
) -> Comment<Loc> {
    let loc = Loc {
        source: env.source(),
        start,
        end,
    };
    let kind = if multiline {
        CommentKind::Block
    } else {
        CommentKind::Line
    };
    let on_newline = env.last_loc.end.line < loc.start.line;
    Comment {
        loc: loc.dupe(),
        kind,
        text: Arc::from(text.as_str()),
        on_newline,
    }
}

fn mk_num_singleton(number_type: NumberType, raw: FlowSmolStr) -> TokenKind {
    let value = match number_type {
        NumberType::LegacyOctal => {
            let mut to_parse = raw.as_str();
            while let Some(s) = to_parse.strip_prefix('0') {
                to_parse = s;
            }
            if to_parse.is_empty() {
                to_parse = "0";
            }
            // Convert legacy octal (e.g., "0755") directly from base 8
            i64::from_str_radix(to_parse, 8)
                .unwrap_or_else(|_| panic!("Invalid legacy octal {}", to_parse)) as f64
        }
        NumberType::Binary => {
            let raw = if let Some(raw) = raw.strip_prefix("0b") {
                raw
            } else {
                raw.strip_prefix("0B").expect(&raw)
            };
            parse_binary_to_f64(raw)
        }
        NumberType::Octal => {
            let raw = if let Some(raw) = raw.strip_prefix("0o") {
                raw
            } else {
                raw.strip_prefix("0O").expect(&raw)
            };
            parse_octal_to_f64(raw)
        }
        NumberType::LegacyNonOctal => {
            // Parse as regular decimal float
            raw.parse::<f64>()
                .unwrap_or_else(|_| panic!("Invalid legacy-octal number {}", raw))
        }
        NumberType::Hex => {
            let remainder = if let Some(remainder) = raw.strip_prefix("0x") {
                remainder
            } else {
                raw.strip_prefix("0X").expect(&raw)
            };
            parse_hex_to_f64(remainder)
        }
        NumberType::Normal(v) => v,
    };

    TokenKind::TNumberSingletonType {
        kind: number_type,
        value,
        raw,
    }
}

fn mk_bignum_singleton(kind: BigintType, raw: FlowSmolStr) -> TokenKind {
    let postraw = bigint_strip_n(&raw);
    // Try to parse as i64, returns None if it doesn't fit
    // Must convert based on the kind (binary, octal, hex) to get decimal value
    let value = match kind {
        BigintType::BigBinary => {
            let digits = if let Some(d) = postraw.strip_prefix("0b") {
                d
            } else {
                postraw.strip_prefix("0B").unwrap_or(postraw)
            };
            i64::from_str_radix(digits, 2).ok()
        }
        BigintType::BigOctal => {
            let digits = if let Some(d) = postraw.strip_prefix("0o") {
                d
            } else {
                postraw.strip_prefix("0O").unwrap_or(postraw)
            };
            i64::from_str_radix(digits, 8).ok()
        }
        BigintType::BigNormal => {
            // Hex literals also use BigNormal
            if postraw.starts_with("0x") || postraw.starts_with("0X") {
                let digits = if let Some(d) = postraw.strip_prefix("0x") {
                    d
                } else {
                    postraw.strip_prefix("0X").unwrap_or(postraw)
                };
                i64::from_str_radix(digits, 16).ok()
            } else {
                postraw.parse::<i64>().ok()
            }
        }
    };

    TokenKind::TBigintSingletonType { kind, value, raw }
}

fn mk_number_token(for_type_token: bool, kind: NumberType, raw: FlowSmolStr) -> TokenKind {
    if for_type_token {
        mk_num_singleton(kind, raw)
    } else {
        TokenKind::TNumber { kind, raw }
    }
}

fn mk_bigint_token(for_type_token: bool, kind: BigintType, raw: FlowSmolStr) -> TokenKind {
    if for_type_token {
        mk_bignum_singleton(kind, raw)
    } else {
        TokenKind::TBigint { kind, raw }
    }
}

/// This is valid since the escapes are already tackled
fn assert_valid_unicode_in_identifier(errors: &mut LexErrors, loc: &Loc, code: u32) {
    if !is_valid_unicode_id(code) {
        errors.push(loc.dupe(), ParseError::IllegalUnicodeEscape);
    }
}

fn add_codepoint_to_string(buf: &mut String, code: u32) {
    if let Some(ch) = char::from_u32(code) {
        buf.push(ch);
    } else {
        // Invalid codepoint, use replacement character
        buf.push(char::REPLACEMENT_CHARACTER);
    }
}

fn decode_identifier<'a>(
    errors: &mut LexErrors,
    env: &LexEnv,
    offset: usize,
    raw: &'a str,
) -> Cow<'a, str> {
    let mut raw_offset = 0;
    let mut value: Cow<'a, str> = Cow::Borrowed("");

    while raw_offset < raw.len() {
        // \uHexHexHexHex
        if let Some(new_remainder) = raw[raw_offset..].strip_prefix("\\u")
            && let Some(new_remainder) = new_remainder.strip_prefix(HEX_DIGIT)
            && let Some(new_remainder) = new_remainder.strip_prefix(HEX_DIGIT)
            && let Some(new_remainder) = new_remainder.strip_prefix(HEX_DIGIT)
            && new_remainder.starts_with(HEX_DIGIT)
        {
            let to_parse = &raw[(raw_offset + 2)..(raw_offset + 6)];
            let code = u32::from_str_radix(to_parse, 16).expect(to_parse);
            let loc = env.loc_of_offsets(offset + raw_offset + 2, offset + raw_offset + 6);
            raw_offset += 6;
            if let Some(c) = char::from_u32(code) {
                assert_valid_unicode_in_identifier(errors, &loc, code);
                value.to_mut().push(c);
            } else {
                errors.push(loc.dupe(), ParseError::IllegalUnicodeEscape);
            }
            continue;
        }
        // \u{hex+}
        if let Some(mut new_remainder) = raw[raw_offset..].strip_prefix("\\u{") {
            let mut bump_length = 3;
            while let Some(new_new_remainder) = new_remainder.strip_prefix(HEX_DIGIT) {
                new_remainder = new_new_remainder;
                bump_length += 1;
            }
            if bump_length > 3 && new_remainder.starts_with('}') {
                let hex_slice = &raw[(raw_offset + 3)..(raw_offset + bump_length)];
                let loc =
                    env.loc_of_offsets(offset + raw_offset + 3, offset + raw_offset + bump_length);
                let code: u32 = u32::from_str_radix(hex_slice, 16).expect(hex_slice);
                bump_length += 1;
                assert_valid_unicode_in_identifier(errors, &loc, code);
                raw_offset += bump_length;
                if let Some(c) = char::from_u32(code) {
                    value.to_mut().push(c);
                }
                continue;
            }
        }
        let c = raw[raw_offset..].chars().next().unwrap();
        raw_offset += c.len_utf8();
        match &mut value {
            Cow::Borrowed(s) => *s = &raw[..raw_offset],
            Cow::Owned(s) => s.push(c),
        }
    }

    value
}

fn comment(
    lexer: &mut logos::Lexer<CommentToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
    buf: &mut String,
) -> usize {
    loop {
        match lexer.next() {
            Some(Ok(CommentToken::LineTerminatorSequence)) => {
                env.new_line(lexer.span());
                buf.push_str(lexer.slice());
            }

            // */
            Some(Ok(CommentToken::EndComment)) => {
                let range = lexer.span();
                if env.in_comment_syntax {
                    let loc = env.loc_of_span(&range);
                    errors.push(
                        loc,
                        ParseError::UnexpectedTokenWithSuggestion(
                            "*/".to_owned(),
                            "*-/".to_owned(),
                        ),
                    );
                }
                return range.end;
            }

            // *-/
            Some(Ok(CommentToken::EndCommentBar)) => {
                if env.in_comment_syntax {
                    return lexer.span().end;
                } else {
                    buf.push_str("*-/");
                }
            }

            None => {
                let loc = env.loc_of_span(&lexer.span());
                errors.push(loc, ParseError::UnexpectedTokenIllegal);
                return lexer.source().len();
            }

            Some(Ok(CommentToken::Other)) => {
                buf.push_str(lexer.slice());
            }

            Some(Err(_)) => unreachable!("unreachable comment"),
        }
    }
}

fn line_comment(
    lexer: &mut logos::Lexer<CommentToken>,
    env: &mut LexEnv,
    buf: &mut String,
) -> Position {
    loop {
        match lexer.next() {
            None => {
                return env.pos_at_offset(lexer.source().len());
            }

            Some(Ok(CommentToken::LineTerminatorSequence)) => {
                let range = lexer.span();
                let mut end_pos = env.pos_at_offset(range.end);
                let len = range.len();
                env.new_line(range);
                end_pos.column -= len as i32;
                return end_pos;
            }

            Some(Ok(_)) => {
                buf.push_str(lexer.slice());
            }

            Some(Err(())) => unreachable!("unreachable line_comment"),
        }
    }
}

fn string_escape(
    remainder: &str,
    start_offset: usize,
    env: &mut LexEnv,
    errors: &mut LexErrors,
) -> (usize, Vec<u32>, bool) {
    if remainder.is_empty() {
        return (0, vec![0], false);
    }
    for lt in ["\r\n", "\r", "\n", "\u{2028}", "\u{2029}"] {
        if remainder.starts_with(lt) {
            env.new_line(start_offset..(start_offset + lt.len()));
            return (lt.len(), Vec::new(), false);
        }
    }
    if remainder.starts_with('\\') {
        return (1, vec!['\\' as u32], false);
    }
    // \xAB
    if let Some(new_remainder) = remainder.strip_prefix("x")
        && let Some(new_remainder) = new_remainder.strip_prefix(HEX_DIGIT)
        && new_remainder.starts_with(HEX_DIGIT)
    {
        let to_parse = &remainder[1..3];
        let code = u32::from_str_radix(to_parse, 16).expect(to_parse);
        return (3, vec![code], false);
    }
    // \uHexHexHexHex
    if let Some(new_remainder) = remainder.strip_prefix("u")
        && let Some(new_remainder) = new_remainder.strip_prefix(HEX_DIGIT)
        && let Some(new_remainder) = new_remainder.strip_prefix(HEX_DIGIT)
        && let Some(new_remainder) = new_remainder.strip_prefix(HEX_DIGIT)
        && new_remainder.starts_with(HEX_DIGIT)
    {
        let to_parse = &remainder[1..5];
        let code = u32::from_str_radix(to_parse, 16).expect(to_parse);
        return (5, vec![code], false);
    }
    // \u{hex+}
    if let Some(mut new_remainder) = remainder.strip_prefix("u{") {
        let mut bump_length = 2;
        while let Some(new_new_remainder) = new_remainder.strip_prefix(HEX_DIGIT) {
            new_remainder = new_new_remainder;
            bump_length += 1;
        }
        if bump_length > 2 && new_remainder.starts_with('}') {
            let hex_slice = &remainder[2..bump_length];
            let code: u32 = u32::from_str_radix(hex_slice, 16).expect(hex_slice);
            bump_length += 1;
            let range = start_offset..(start_offset + bump_length);
            // 11.8.4.1
            if code > 0x10FFFF {
                errors.push(env.loc_of_span(&range), ParseError::UnexpectedTokenIllegal);
            }
            return (bump_length, vec![code], false);
        }
    }
    let octal = ['0', '1', '2', '3', '4', '5', '6', '7'];
    // octal len 1 or \0
    if let Some(new_remainder) = remainder.strip_prefix(octal) {
        // octal len 2
        if let Some(new_remainder) = new_remainder.strip_prefix(octal) {
            // octal len 3
            if new_remainder.starts_with(octal) {
                let s = &remainder[..3];
                let code: u32 = u32::from_str_radix(s, 8).expect(s);
                // If the 3 character octal code is larger than 256
                // then it is parsed as a 2 character octal code
                return if code < 256 {
                    (3, vec![code], true)
                } else {
                    let remainer = code & 7;
                    let code = code >> 3;
                    (3, vec![code, '0' as u32 + remainer], true)
                };
            }
            let s = &remainder[..2];
            let code: u32 = u32::from_str_radix(s, 8).expect(s);
            return (2, vec![code], true);
        }
        let s = &remainder[..1];
        return if s == "0" {
            (1, vec![0x0], false)
        } else {
            let code: u32 = u32::from_str_radix(s, 8).expect(s);
            (1, vec![code], true)
        };
    }
    match remainder.chars().next().unwrap() {
        'b' => (1, vec![0x8], false),
        'f' => (1, vec![0xC], false),
        'n' => (1, vec![0xA], false),
        'r' => (1, vec![0xD], false),
        't' => (1, vec![0x9], false),
        'v' => (1, vec![0xB], false),
        'u' => {
            errors.push(
                env.loc_of_span(&(start_offset..(start_offset + 1))),
                ParseError::UnexpectedTokenIllegal,
            );
            (1, vec!['u' as u32], false)
        }
        'x' => {
            errors.push(
                env.loc_of_span(&(start_offset..(start_offset + 1))),
                ParseError::UnexpectedTokenIllegal,
            );
            (1, vec!['x' as u32], false)
        }
        c => (c.len_utf8(), vec![c as u32], false),
    }
}

enum StringQuoteValue<'a> {
    NoEscape(&'a str),
    Escaped(Vec<u32>),
}

impl<'a> StringQuoteValue<'a> {
    fn to_cow(self) -> Cow<'a, str> {
        match self {
            Self::NoEscape(s) => Cow::Borrowed(s),
            Self::Escaped(buf) => {
                let mut s = String::new();
                let mut i = 0;
                while i < buf.len() {
                    let codepoint = buf[i];
                    if let Some(c) = char::from_u32(codepoint) {
                        s.push(c);
                        i += 1;
                        continue;
                    }
                    if codepoint >= 0xD800
                        && codepoint <= 0xDBFF
                        && let Some(next_codepoint) = buf.get(i + 1)
                    {
                        let next_codepoint = *next_codepoint;
                        if next_codepoint >= 0xDC00 && next_codepoint <= 0xDFFF {
                            let combined_codepoint =
                                0x10000 + ((codepoint - 0xD800) << 10) + (next_codepoint - 0xDC00);
                            let c = char::from_u32(combined_codepoint).unwrap();
                            s.push(c);
                            i += 2;
                            continue;
                        }
                    }
                    let high = codepoint >> 4;
                    if high != 0 {
                        let escaped = format!("\\u{:04X}", high);
                        s.push_str(&escaped);
                    }
                    let escaped = format!("\\u{:04X}", codepoint & 65535);
                    s.push_str(&escaped);
                    i += 1;
                }
                Cow::Owned(s)
            }
        }
    }
}

fn string_quote<'a>(
    env: &mut LexEnv,
    errors: &mut LexErrors,
    original_remainder: &'a str,
    start_offset: usize,
    is_double_quote: bool,
    mut octal: bool,
) -> (Cow<'a, str>, usize, bool) {
    let mut intermediate_value: StringQuoteValue<'a> = StringQuoteValue::NoEscape("");
    let mut local_offset = 0;
    loop {
        let remainder = &original_remainder[local_offset..];
        let Some(c) = remainder.chars().next() else {
            let loc = env.loc_of_offsets(start_offset, start_offset);
            errors.push(loc, ParseError::UnexpectedTokenIllegal);
            return (
                intermediate_value.to_cow(),
                start_offset + local_offset,
                octal,
            );
        };
        match c {
            '"' => {
                local_offset += 1;
                if is_double_quote {
                    return (
                        intermediate_value.to_cow(),
                        start_offset + local_offset,
                        octal,
                    );
                } else {
                    match &mut intermediate_value {
                        StringQuoteValue::NoEscape(s) => {
                            *s = &original_remainder[..local_offset];
                        }
                        StringQuoteValue::Escaped(s) => s.push(b'"' as u32),
                    }
                }
            }
            '\'' => {
                local_offset += 1;
                if !is_double_quote {
                    return (
                        intermediate_value.to_cow(),
                        start_offset + local_offset,
                        octal,
                    );
                } else {
                    match &mut intermediate_value {
                        StringQuoteValue::NoEscape(s) => {
                            *s = &original_remainder[..local_offset];
                        }
                        StringQuoteValue::Escaped(s) => s.push(b'\'' as u32),
                    }
                }
            }
            '\\' => {
                local_offset += 1;
                let (bump_len, codes, octal_new) =
                    string_escape(&remainder[1..], start_offset + local_offset, env, errors);
                local_offset += bump_len;
                octal = octal || octal_new;
                let buf = match &mut intermediate_value {
                    StringQuoteValue::NoEscape(s) => {
                        let mut buf = Vec::new();
                        for c in s.chars() {
                            let v = c as u32;
                            buf.push(v);
                        }
                        intermediate_value = StringQuoteValue::Escaped(buf);
                        match &mut intermediate_value {
                            StringQuoteValue::NoEscape(_) => unreachable!(),
                            StringQuoteValue::Escaped(buf) => buf,
                        }
                    }
                    StringQuoteValue::Escaped(buf) => buf,
                };
                buf.extend(codes);
            }
            '\n' => {
                let span = (start_offset + local_offset)..(start_offset + local_offset + 1);
                let loc = env.loc_of_span(&span);
                let span_end = span.end;
                env.new_line(span);
                errors.push(loc, ParseError::UnexpectedTokenIllegal);
                match &mut intermediate_value {
                    StringQuoteValue::NoEscape(s) => {
                        *s = &original_remainder[..(local_offset + 1)];
                    }
                    StringQuoteValue::Escaped(s) => s.push(b'\n' as u32),
                }
                return (intermediate_value.to_cow(), span_end, octal);
            }
            c => {
                local_offset += c.len_utf8();
                match &mut intermediate_value {
                    StringQuoteValue::NoEscape(s) => {
                        *s = &original_remainder[..local_offset];
                    }
                    StringQuoteValue::Escaped(s) => {
                        s.push(c as u32);
                    }
                }
            }
        }
    }
}

fn template_part<'a>(
    mut lexer: logos::Lexer<'a, TemplatePartToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
    cooked: &mut String,
    raw: &mut String,
) -> (logos::Lexer<'a, TemplatePartToken>, bool) {
    loop {
        match lexer.next() {
            // EOF
            None => {
                let end_offset = lexer.source().len();
                let loc = env.loc_of_offsets(end_offset, end_offset);
                errors.push(loc, ParseError::UnexpectedTokenIllegal);
                return (lexer, true);
            }
            Some(Ok(TemplatePartToken::Backtick)) => {
                return (lexer, true);
            }
            // ${
            Some(Ok(TemplatePartToken::SubstitutionStart)) => {
                return (lexer, false);
            }
            // \
            Some(Ok(TemplatePartToken::Backslash)) => {
                raw.push('\\');
                let (bump_len, codes, _) =
                    string_escape(lexer.remainder(), lexer.span().end, env, errors);
                let str = &lexer.remainder()[..bump_len];
                raw.push_str(str);
                lexer.bump(bump_len);
                for code in codes {
                    add_codepoint_to_string(cooked, code);
                }
            }
            // \r\n
            Some(Ok(TemplatePartToken::Crlf)) => {
                raw.push_str("\r\n");
                cooked.push('\n');
                env.new_line(lexer.span());
            }
            // \n or \r
            Some(Ok(TemplatePartToken::Newline)) => {
                let s = lexer.slice();
                raw.push_str(s);
                cooked.push('\n');
                env.new_line(lexer.span());
            }
            Some(Ok(TemplatePartToken::Other)) => {
                let s = lexer.slice();
                cooked.push_str(s);
                raw.push_str(s);
            }
            Some(Err(())) => unreachable!("unreachable template_part"),
        }
    }
}

enum TokenResult {
    Token(Loc, TokenKind),
    Comment(Comment<Loc>),
    Continue,
}

static WORD_START: [char; 53] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '$',
];
static DIGIT: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
static ALPHA_NUMERIC: [char; 63] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B',
    'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U',
    'V', 'W', 'X', 'Y', 'Z', '$',
];

fn check_sci_notation(remainder: &str) -> Option<(usize, bool)> {
    let mut remainder_mut = remainder.strip_prefix(['e', 'E'])?;
    let mut bump_counter = 1;

    if let Some(new_remainder) = remainder_mut.strip_prefix(['+', '-']) {
        bump_counter += 1;
        remainder_mut = new_remainder;
    }

    if let Some(mut remainder) = remainder_mut.strip_prefix('0') {
        // We are now sure it's a scinumber
        bump_counter += 1;
        while let Some(new_remainder) = remainder.strip_prefix(DIGIT) {
            bump_counter += 1;
            remainder = new_remainder;
        }
        remainder_mut = remainder;
    } else if let Some(mut remainder) =
        remainder_mut.strip_prefix(['1', '2', '3', '4', '5', '6', '7', '8', '9'])
    {
        // We are now sure it's a scinumber
        bump_counter += 1;
        while let Some(new_remainder) =
            remainder.strip_prefix(['_', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
        {
            bump_counter += 1;
            remainder = new_remainder;
        }
        remainder_mut = remainder;
    } else {
        return None;
    }

    let is_sci_bigint = remainder_mut.starts_with('n');
    if is_sci_bigint {
        bump_counter += 1;
    }

    Some((bump_counter, is_sci_bigint))
}

fn lex_sci_number(
    env: &mut LexEnv,
    errors: &mut LexErrors,
    mut range: std::ops::Range<usize>,
    original_source: &str,
    original_remainder: &str,
    gen_token_kind: fn(f64, FlowSmolStr) -> TokenKind,
    gen_bigint_kind_for_sci_bigint: fn(FlowSmolStr) -> TokenKind,
) -> Option<(usize, TokenResult)> {
    if let Some((bump_counter, is_sci_bigint)) = check_sci_notation(original_remainder) {
        range.end += bump_counter;
        if original_remainder[bump_counter..].starts_with(WORD_START) {
            handle_word_after_number(env, errors, &original_remainder[bump_counter..], &range)
        }
        let loc = env.loc_of_span(&range);
        let s = &original_source[range];
        if is_sci_bigint {
            errors.push(loc.dupe(), ParseError::InvalidSciBigInt);
            return Some((
                bump_counter,
                TokenResult::Token(loc, gen_bigint_kind_for_sci_bigint(FlowSmolStr::new(s))),
            ));
        }
        let v = match s.replace('_', "").parse::<f64>() {
            Ok(v) => v,
            Err(_) => {
                // Due to logos bug, we might be in this weird state, even through it should be
                // impossible. This branch defends against that.
                // See https://github.com/maciejhirsz/logos/issues/420
                errors.push(loc.dupe(), ParseError::UnexpectedTokenIllegal);
                0.0
            }
        };
        return Some((
            bump_counter,
            TokenResult::Token(loc, gen_token_kind(v, FlowSmolStr::new(s))),
        ));
    }
    None
}

fn lex_whole_number(
    env: &mut LexEnv,
    errors: &mut LexErrors,
    range: std::ops::Range<usize>,
    original_source: &str,
    original_remainder: &str,
    gen_number_token_kind: fn(f64, FlowSmolStr) -> TokenKind,
    gen_bigint_token_kind: fn(FlowSmolStr) -> TokenKind,
) -> (usize, TokenResult) {
    if let Some(remainder_after_n) = original_remainder.strip_prefix('n') {
        let mut range = range;
        range.end += 1;
        if remainder_after_n.starts_with(WORD_START) {
            handle_word_after_number(env, errors, remainder_after_n, &range)
        }
        let loc = env.loc_of_span(&range);
        let raw = &original_source[range];
        if raw.contains('.') {
            errors.push(loc.dupe(), ParseError::InvalidFloatBigInt);
        }
        return (
            1,
            TokenResult::Token(loc, gen_bigint_token_kind(FlowSmolStr::new(raw))),
        );
    }

    if let Some((bump_counter, token_result)) = lex_sci_number(
        env,
        errors,
        range.clone(),
        original_source,
        original_remainder,
        gen_number_token_kind,
        gen_bigint_token_kind,
    ) {
        return (bump_counter, token_result);
    }
    if original_remainder.starts_with(WORD_START) {
        handle_word_after_number(env, errors, original_remainder, &range);
    }
    let loc = env.loc_of_span(&range);
    let s = &original_source[range];
    let v = match s.replace('_', "").parse::<f64>() {
        Ok(v) => v,
        Err(_) => {
            // Due to logos bug, we might be in this weird state, even through it should be
            // impossible. This branch defends against that.
            // See https://github.com/maciejhirsz/logos/issues/420
            errors.push(loc.dupe(), ParseError::UnexpectedTokenIllegal);
            0.0
        }
    };
    (
        0,
        TokenResult::Token(loc, gen_number_token_kind(v, FlowSmolStr::new(s))),
    )
}

fn handle_word_after_number(
    env: &mut LexEnv,
    errors: &mut LexErrors,
    remainder: &str,
    range: &std::ops::Range<usize>,
) {
    let mut len = 1;
    while len < remainder.len() && remainder[len..].starts_with(ALPHA_NUMERIC) {
        len += 1;
    }
    let loc = env.loc_of_offsets(range.start, range.end + len);
    errors.push(loc, ParseError::UnexpectedTokenIllegal);
}

fn token_base_inner<'a>(
    for_type_token: bool,
    lexer: &mut logos::Lexer<'a, MainToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
) -> TokenResult {
    let token = match lexer.next() {
        // EOF
        None => {
            let end_pos = lexer.source().len();
            let loc = env.loc_of_offsets(end_pos, end_pos);
            if env.in_comment_syntax {
                errors.push(loc.dupe(), ParseError::UnexpectedEOS);
            }
            return TokenResult::Token(loc, TokenKind::TEof);
        }
        Some(Err(_)) => panic!("unreachable token `{:?}`", lexer.slice().as_bytes()),
        Some(Ok(t)) => t,
    };
    match token {
        MainToken::LineTerminatorSequence => {
            env.new_line(lexer.span());
            TokenResult::Continue
        }
        MainToken::Whitespace => TokenResult::Continue,
        MainToken::BlockCommentStart => {
            if env.comment_syntax_enabled() {
                let mut bump_counter = 0;
                let mut remainder = lexer.remainder();
                while let Some(c) = remainder.chars().next()
                    && c.is_whitespace()
                {
                    let len = c.len_utf8();
                    bump_counter += len;
                    remainder = &remainder[len..];
                }
                for flow_comment_marker in ["::", ":", "flow-include"] {
                    if remainder.starts_with(flow_comment_marker) {
                        bump_counter += flow_comment_marker.len();
                        let mut range = lexer.span();
                        range.end += bump_counter;
                        if env.in_comment_syntax {
                            let loc = env.loc_of_span(&range);
                            errors.push(
                                loc.dupe(),
                                ParseError::Unexpected(format!(
                                    "token `{}`",
                                    &lexer.source()[range.clone()]
                                )),
                            );
                        }
                        lexer.bump(bump_counter);
                        env.in_comment_syntax = true;
                        return if flow_comment_marker == ":" {
                            let loc = env.loc_of_span(&range);
                            TokenResult::Token(loc, TokenKind::TColon)
                        } else {
                            TokenResult::Continue
                        };
                    };
                }
            }
            let start = env.pos_at_offset(lexer.span().start);
            let mut buf = String::new();
            let mut comment_lexer = lexer.clone().morph(); // clone needed: logos::Lexer doesn't implement Dupe
            let end_offset = comment(&mut comment_lexer, env, errors, &mut buf);
            let end = env.pos_at_offset(end_offset);
            let c = mk_comment(env, start, end, buf, true);
            *lexer = comment_lexer.morph();
            TokenResult::Comment(c)
        }
        MainToken::LineCommentStart => {
            let start = env.pos_at_offset(lexer.span().start);
            let mut buf = String::new();
            let mut comment_lexer = lexer.clone().morph(); // clone needed: logos::Lexer doesn't implement Dupe
            let end = line_comment(&mut comment_lexer, env, &mut buf);
            *lexer = comment_lexer.morph();
            let c = mk_comment(env, start, end, buf, false);
            TokenResult::Comment(c)
        }
        // Support for the shebang at the beginning of a file. It is treated like a
        // comment at the beginning or an error elsewhere
        MainToken::Shebang => {
            if for_type_token {
                let loc = env.loc_of_span(&lexer.span());
                TokenResult::Token(loc, TokenKind::TError("#!".to_owned()))
            } else if lexer.span().start == 0 {
                let start = env.pos_at_offset(lexer.span().start);
                let mut buf = String::new();
                let mut comment_lexer = lexer.clone().morph(); // clone needed: logos::Lexer doesn't implement Dupe
                let end = line_comment(&mut comment_lexer, env, &mut buf);
                *lexer = comment_lexer.morph();
                let loc = Loc {
                    source: env.source(),
                    start,
                    end,
                };
                TokenResult::Token(loc.dupe(), TokenKind::TInterpreter(loc, buf))
            } else {
                let loc = env.loc_of_span(&lexer.span());
                TokenResult::Token(loc, TokenKind::TError("#!".to_owned()))
            }
        }
        MainToken::SingleQuote | MainToken::DoubleQuote => {
            let span = lexer.span();
            let start_offset = span.start;
            let start_end_offset = span.end;
            let start = env.pos_at_offset(start_offset);
            let (value, end_offset, octal) = string_quote(
                env,
                errors,
                lexer.remainder(),
                start_end_offset,
                token == MainToken::DoubleQuote,
                false,
            );
            lexer.bump(end_offset - start_end_offset);
            let raw = &lexer.source()[start_offset..end_offset];
            let end = env.pos_at_offset(end_offset);
            let loc = Loc {
                source: env.source(),
                start,
                end,
            };
            TokenResult::Token(
                loc.dupe(),
                TokenKind::TString(loc, FlowSmolStr::new(value), FlowSmolStr::new(raw), octal),
            )
        }
        MainToken::Backtick => {
            let mut value = String::new();
            let mut raw = String::new();
            let start = env.pos_at_offset(lexer.span().start);
            let (template_lexer, is_tail) =
                template_part(lexer.clone().morph(), env, errors, &mut value, &mut raw);
            *lexer = template_lexer.morph();
            let end = env.pos_at_offset(lexer.span().end);
            let loc = Loc {
                source: env.source(),
                start,
                end,
            };
            TokenResult::Token(
                loc.dupe(),
                TokenKind::TTemplatePart(TemplatePart {
                    loc,
                    value: FlowSmolStr::new(&value),
                    raw: FlowSmolStr::new(&raw),
                    head: true,
                    tail: is_tail,
                }),
            )
        }
        MainToken::BinBigint => {
            if lexer.remainder().starts_with(WORD_START) {
                let range = lexer.span();
                let loc = env.loc_of_offsets(range.start, range.end + 1);
                errors.push(loc, ParseError::UnexpectedTokenIllegal);
            }
            let loc = env.loc_of_span(&lexer.span());
            let raw = FlowSmolStr::new(lexer.slice());
            TokenResult::Token(
                loc,
                mk_bigint_token(for_type_token, BigintType::BigBinary, raw),
            )
        }
        MainToken::BinNumber => {
            let remainder = lexer.remainder();
            if remainder.starts_with(WORD_START)
                || remainder.starts_with(['2', '3', '4', '5', '6', '7', '8', '9'])
            {
                let range = lexer.span();
                handle_word_after_number(env, errors, remainder, &range)
            }
            let loc = env.loc_of_span(&lexer.span());
            let raw = FlowSmolStr::new(lexer.slice());
            TokenResult::Token(
                loc,
                mk_number_token(for_type_token, NumberType::Binary, raw),
            )
        }
        MainToken::OctBigint => {
            let remainder = lexer.remainder();
            if remainder.starts_with(WORD_START) {
                let range = lexer.span();
                handle_word_after_number(env, errors, remainder, &range)
            }
            let loc = env.loc_of_span(&lexer.span());
            let raw = FlowSmolStr::new(lexer.slice());
            TokenResult::Token(
                loc,
                mk_bigint_token(for_type_token, BigintType::BigOctal, raw),
            )
        }
        MainToken::LegacyNonOctNumber => {
            let remainder = lexer.remainder();
            if remainder.starts_with(WORD_START) {
                let range = lexer.span();
                handle_word_after_number(env, errors, remainder, &range)
            }
            let loc = env.loc_of_span(&lexer.span());
            let raw = FlowSmolStr::new(lexer.slice());
            TokenResult::Token(
                loc,
                mk_number_token(for_type_token, NumberType::LegacyNonOctal, raw),
            )
        }
        MainToken::OctNumber | MainToken::LegacyOctNumber => {
            let remainder = lexer.remainder();
            if remainder.starts_with(WORD_START) || remainder.starts_with(['8', '9']) {
                let range = lexer.span();
                handle_word_after_number(env, errors, remainder, &range)
            }
            let loc = env.loc_of_span(&lexer.span());
            let kind = match token {
                MainToken::OctNumber => NumberType::Octal,
                MainToken::LegacyNonOctNumber => NumberType::LegacyNonOctal,
                _ => NumberType::LegacyOctal,
            };
            let raw = FlowSmolStr::new(lexer.slice());
            TokenResult::Token(loc, mk_number_token(for_type_token, kind, raw))
        }
        MainToken::HexBigint => {
            let remainder = lexer.remainder();
            if remainder.starts_with(WORD_START) {
                let range = lexer.span();
                handle_word_after_number(env, errors, remainder, &range)
            }
            let loc = env.loc_of_span(&lexer.span());
            let raw = FlowSmolStr::new(lexer.slice());
            TokenResult::Token(
                loc,
                mk_bigint_token(for_type_token, BigintType::BigNormal, raw),
            )
        }
        MainToken::HexNumber => {
            let remainder = lexer.remainder();
            if remainder.starts_with(WORD_START)
                && !remainder
                    .starts_with(['A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f'])
            {
                let range = lexer.span();
                handle_word_after_number(env, errors, remainder, &range)
            }
            let loc = env.loc_of_span(&lexer.span());
            let raw = FlowSmolStr::new(lexer.slice());
            TokenResult::Token(loc, mk_number_token(for_type_token, NumberType::Hex, raw))
        }
        MainToken::WholeNumber
        | MainToken::FloatNumberWithWholePart
        | MainToken::FloatNumberWithOnlyDecimals => {
            let (gen_number, gen_bigint): (
                fn(f64, FlowSmolStr) -> TokenKind,
                fn(FlowSmolStr) -> TokenKind,
            ) = if for_type_token {
                (
                    |v, raw| mk_num_singleton(NumberType::Normal(v), raw),
                    |raw| mk_bignum_singleton(BigintType::BigNormal, raw),
                )
            } else {
                (
                    |v, raw| TokenKind::TNumber {
                        kind: NumberType::Normal(v),
                        raw,
                    },
                    |raw| TokenKind::TBigint {
                        kind: BigintType::BigNormal,
                        raw,
                    },
                )
            };
            let (bump_counter, token_result) = lex_whole_number(
                env,
                errors,
                lexer.span(),
                lexer.source(),
                lexer.remainder(),
                gen_number,
                gen_bigint,
            );
            lexer.bump(bump_counter);
            token_result
        }
        // TODO: Use [Symbol.iterator] instead of @@iterator.
        // `@` is not a valid unicode name
        MainToken::AtAtIterator
        | MainToken::AtAtAsyncIterator
        | MainToken::AtAtDispose
        | MainToken::AtAtAsyncDispose => {
            let loc = env.loc_of_span(&lexer.span());
            let raw = FlowSmolStr::new(lexer.slice());

            TokenResult::Token(
                loc.dupe(),
                TokenKind::TIdentifier {
                    loc,
                    value: raw.clone(),
                    raw,
                },
            )
        }
        MainToken::LCurly => {
            if for_type_token && lexer.remainder().starts_with("|") {
                let start = lexer.span().start;
                let end = lexer.span().end;
                lexer.bump(1);
                let loc = env.loc_of_offsets(start, end + 1);
                return TokenResult::Token(loc, TokenKind::TLcurlybar);
            }
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TLcurly)
        }
        MainToken::RCurly => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TRcurly)
        }
        MainToken::LParen => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TLparen)
        }
        MainToken::RParen => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TRparen)
        }
        MainToken::LBracket => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TLbracket)
        }
        MainToken::RBracket => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TRbracket)
        }
        MainToken::Ellipsis => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TEllipsis)
        }
        MainToken::Period => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TPeriod)
        }
        MainToken::Semicolon => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TSemicolon)
        }
        MainToken::Comma => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TComma)
        }
        MainToken::Colon => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TColon)
        }
        MainToken::Pling => {
            let remainder = lexer.remainder();
            // In normal mode, compose ?? and ??= via peek+bump.
            // In type mode, ?? should remain two separate ? tokens (e.g. ??number = ?(?number)).
            if !for_type_token && remainder.starts_with('?') {
                let range = lexer.span();
                if remainder.starts_with("?=") {
                    lexer.bump(2);
                    let loc = env.loc_of_offsets(range.start, range.end + 2);
                    return TokenResult::Token(loc, TokenKind::TNullishAssign);
                }
                let loc = env.loc_of_offsets(range.start, range.end + 1);
                lexer.bump(1);
                return TokenResult::Token(loc, TokenKind::TPlingPling);
            }
            if let Some(after_pling_period) = remainder.strip_prefix('.') {
                if for_type_token || !after_pling_period.starts_with(DIGIT) {
                    // ?. confirmed
                    let range = lexer.span();
                    let loc = env.loc_of_offsets(range.start, range.end + 1);
                    lexer.bump(1);
                    return TokenResult::Token(loc, TokenKind::TPlingPeriod);
                }
            }
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TPling)
        }
        MainToken::And => {
            if !for_type_token && lexer.remainder().starts_with('=') {
                let range = lexer.span();
                lexer.bump(1);
                let loc = env.loc_of_offsets(range.start, range.end + 1);
                return TokenResult::Token(loc, TokenKind::TAndAssign);
            }
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TAnd)
        }
        MainToken::Or => {
            if !for_type_token && lexer.remainder().starts_with('=') {
                let range = lexer.span();
                lexer.bump(1);
                let loc = env.loc_of_offsets(range.start, range.end + 1);
                return TokenResult::Token(loc, TokenKind::TOrAssign);
            }
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TOr)
        }
        MainToken::StrictEqual => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TStrictEqual)
        }
        MainToken::StrictNotEqual => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TStrictNotEqual)
        }
        MainToken::Equal => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TEqual)
        }
        MainToken::NotEqual => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TNotEqual)
        }
        MainToken::ModAssign => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TModAssign)
        }
        MainToken::BitXorAssign => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TBitXorAssign)
        }
        MainToken::LessThan => {
            if for_type_token {
                let loc = env.loc_of_span(&lexer.span());
                TokenResult::Token(loc, TokenKind::TLessThan)
            } else {
                let start = lexer.span().start;
                let end = lexer.span().end;
                let remainder = lexer.remainder();
                if remainder.starts_with("<=") {
                    lexer.bump(2);
                    let loc = env.loc_of_offsets(start, end + 2);
                    TokenResult::Token(loc, TokenKind::TLshiftAssign)
                } else if remainder.starts_with("<") {
                    lexer.bump(1);
                    let loc = env.loc_of_offsets(start, end + 1);
                    TokenResult::Token(loc, TokenKind::TLshift)
                } else if remainder.starts_with("=") {
                    lexer.bump(1);
                    let loc = env.loc_of_offsets(start, end + 1);
                    TokenResult::Token(loc, TokenKind::TLessThanEqual)
                } else {
                    let loc = env.loc_of_offsets(start, end);
                    TokenResult::Token(loc, TokenKind::TLessThan)
                }
            }
        }
        MainToken::GreaterThan => {
            if for_type_token {
                let loc = env.loc_of_span(&lexer.span());
                TokenResult::Token(loc, TokenKind::TGreaterThan)
            } else {
                let start = lexer.span().start;
                let end = lexer.span().end;
                let remainder = lexer.remainder();
                if remainder.starts_with(">>=") {
                    lexer.bump(3);
                    let loc = env.loc_of_offsets(start, end + 3);
                    TokenResult::Token(loc, TokenKind::TRshift3Assign)
                } else if remainder.starts_with(">>") {
                    lexer.bump(2);
                    let loc = env.loc_of_offsets(start, end + 2);
                    TokenResult::Token(loc, TokenKind::TRshift3)
                } else if remainder.starts_with(">=") {
                    lexer.bump(2);
                    let loc = env.loc_of_offsets(start, end + 2);
                    TokenResult::Token(loc, TokenKind::TRshiftAssign)
                } else if remainder.starts_with(">") {
                    lexer.bump(1);
                    let loc = env.loc_of_offsets(start, end + 1);
                    TokenResult::Token(loc, TokenKind::TRshift)
                } else if remainder.starts_with("=") {
                    lexer.bump(1);
                    let loc = env.loc_of_offsets(start, end + 1);
                    TokenResult::Token(loc, TokenKind::TGreaterThanEqual)
                } else {
                    let loc = env.loc_of_offsets(start, end);
                    TokenResult::Token(loc, TokenKind::TGreaterThan)
                }
            }
        }
        MainToken::Plus => {
            if !for_type_token {
                let start = lexer.span().start;
                let end = lexer.span().end;
                let remainder = lexer.remainder();
                if remainder.starts_with('=') {
                    lexer.bump(1);
                    let loc = env.loc_of_offsets(start, end + 1);
                    return TokenResult::Token(loc, TokenKind::TPlusAssign);
                } else if remainder.starts_with('+') {
                    lexer.bump(1);
                    let loc = env.loc_of_offsets(start, end + 1);
                    return TokenResult::Token(loc, TokenKind::TIncr);
                }
            }
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TPlus)
        }
        MainToken::Minus => {
            if !for_type_token {
                let start = lexer.span().start;
                let end = lexer.span().end;
                let remainder = lexer.remainder();
                if remainder.starts_with('=') {
                    lexer.bump(1);
                    let loc = env.loc_of_offsets(start, end + 1);
                    return TokenResult::Token(loc, TokenKind::TMinusAssign);
                } else if remainder.starts_with('-') {
                    lexer.bump(1);
                    let loc = env.loc_of_offsets(start, end + 1);
                    return TokenResult::Token(loc, TokenKind::TDecr);
                }
            }
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TMinus)
        }
        MainToken::Mult => {
            // In the OCaml code, */ is lexed as another token, since it's significant for
            // comment syntax ending. However, it would require backtracking, which is not allowed
            // by logos. Therefore, we peek here instead.
            if env.in_comment_syntax {
                if lexer.remainder().starts_with("/") {
                    env.in_comment_syntax = false;
                    lexer.bump(1);
                    return TokenResult::Continue;
                }
            }
            if !for_type_token {
                let start = lexer.span().start;
                let end = lexer.span().end;
                let remainder = lexer.remainder();
                if remainder.starts_with("*=") {
                    lexer.bump(2);
                    let loc = env.loc_of_offsets(start, end + 2);
                    return TokenResult::Token(loc, TokenKind::TExpAssign);
                } else if remainder.starts_with('*') {
                    lexer.bump(1);
                    let loc = env.loc_of_offsets(start, end + 1);
                    return TokenResult::Token(loc, TokenKind::TExp);
                } else if remainder.starts_with('=') {
                    lexer.bump(1);
                    let loc = env.loc_of_offsets(start, end + 1);
                    return TokenResult::Token(loc, TokenKind::TMultAssign);
                }
            }
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TMult)
        }
        MainToken::Mod => {
            if for_type_token && lexer.remainder().starts_with("checks") {
                let start = lexer.span().start;
                let end = lexer.span().end;
                lexer.bump(6);
                let loc = env.loc_of_offsets(start, end + 6);
                return TokenResult::Token(loc, TokenKind::TChecks);
            }
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TMod)
        }
        MainToken::BitOr => {
            if for_type_token && lexer.remainder().starts_with("}") {
                let start = lexer.span().start;
                let end = lexer.span().end;
                lexer.bump(1);
                let loc = env.loc_of_offsets(start, end + 1);
                return TokenResult::Token(loc, TokenKind::TRcurlybar);
            }
            if !for_type_token && lexer.remainder().starts_with('=') {
                let start = lexer.span().start;
                let end = lexer.span().end;
                lexer.bump(1);
                let loc = env.loc_of_offsets(start, end + 1);
                return TokenResult::Token(loc, TokenKind::TBitOrAssign);
            }
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TBitOr)
        }
        MainToken::BitAnd => {
            if !for_type_token && lexer.remainder().starts_with('=') {
                let start = lexer.span().start;
                let end = lexer.span().end;
                lexer.bump(1);
                let loc = env.loc_of_offsets(start, end + 1);
                return TokenResult::Token(loc, TokenKind::TBitAndAssign);
            }
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TBitAnd)
        }
        MainToken::BitXor => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TBitXor)
        }
        MainToken::Not => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TNot)
        }
        MainToken::BitNot => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TBitNot)
        }
        MainToken::Assign => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TAssign)
        }
        MainToken::Arrow => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TArrow)
        }
        MainToken::DivAssign => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TDivAssign)
        }
        MainToken::Div => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TDiv)
        }
        MainToken::At => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TAt)
        }
        MainToken::Pound => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TPound)
        }
        // To reason about its correctness:
        // 1. all tokens are still matched
        // 2. tokens like opaque, opaquex are matched correctly
        //    the most fragile case is `opaquex` (matched with `opaque,x` instead)
        // 3. \a is disallowed
        // 4. a世界 recognized
        MainToken::Backslash => {
            let loc = env.loc_of_span(&lexer.span());
            errors.push(loc.dupe(), ParseError::UnexpectedTokenIllegal);
            if for_type_token {
                TokenResult::Token(loc, TokenKind::TError("\\".to_owned()))
            } else {
                TokenResult::Continue
            }
        }
        MainToken::JsIdStart => {
            let start_offset = lexer.span().start;
            loop_id_continues(lexer);
            let end_offset = lexer.span().end;
            let loc = env.loc_of_offsets(start_offset, end_offset);
            let id_str = &lexer.source()[start_offset..end_offset];
            let token_kind = if for_type_token {
                // Check "renders" first for renders? and renders*
                if id_str == "renders" {
                    let remainder = lexer.remainder();
                    if remainder.starts_with('?') {
                        let extended_loc = env.loc_of_offsets(start_offset, end_offset + 1);
                        lexer.bump(1);
                        return TokenResult::Token(extended_loc, TokenKind::TRendersQuestion);
                    }
                    if remainder.starts_with('*') {
                        let extended_loc = env.loc_of_offsets(start_offset, end_offset + 1);
                        lexer.bump(1);
                        return TokenResult::Token(extended_loc, TokenKind::TRendersStar);
                    }
                }
                // Type keyword table
                // keep this list in sync with Parser_env.is_reserved_type
                // and token_is_reserved_type
                match id_str {
                    "any" => TokenKind::TAnyType,
                    "bigint" => TokenKind::TBigintType,
                    "bool" => TokenKind::TBooleanType(crate::token::BoolOrBoolean::Bool),
                    "boolean" => TokenKind::TBooleanType(crate::token::BoolOrBoolean::Boolean),
                    "const" => TokenKind::TConst,
                    "empty" => TokenKind::TEmptyType,
                    "extends" => TokenKind::TExtends,
                    "false" => TokenKind::TFalse,
                    "import" => TokenKind::TImport,
                    "interface" => TokenKind::TInterface,
                    "keyof" => TokenKind::TKeyof,
                    "mixed" => TokenKind::TMixedType,
                    "never" => TokenKind::TNeverType,
                    "new" => TokenKind::TNew,
                    "null" => TokenKind::TNull,
                    "number" => TokenKind::TNumberType,
                    "readonly" => TokenKind::TReadonly,
                    "writeonly" => TokenKind::TWriteonly,
                    "infer" => TokenKind::TInfer,
                    "is" => TokenKind::TIs,
                    "asserts" => TokenKind::TAsserts,
                    "implies" => TokenKind::TImplies,
                    "static" => TokenKind::TStatic,
                    "string" => TokenKind::TStringType,
                    "symbol" => TokenKind::TSymbolType,
                    "true" => TokenKind::TTrue,
                    "typeof" => TokenKind::TTypeof,
                    "undefined" => TokenKind::TUndefinedType,
                    "unknown" => TokenKind::TUnknownType,
                    "void" => TokenKind::TVoidType,
                    _ => {
                        let value = decode_identifier(errors, env, start_offset, id_str);
                        TokenKind::TIdentifier {
                            loc: loc.dupe(),
                            value: FlowSmolStr::new(value),
                            raw: FlowSmolStr::new(id_str),
                        }
                    }
                }
            } else {
                // Normal keyword table
                match id_str {
                    "async" => TokenKind::TAsync,
                    "await" => TokenKind::TAwait,
                    "break" => TokenKind::TBreak,
                    "case" => TokenKind::TCase,
                    "catch" => TokenKind::TCatch,
                    "class" => TokenKind::TClass,
                    "const" => TokenKind::TConst,
                    "continue" => TokenKind::TContinue,
                    "debugger" => TokenKind::TDebugger,
                    "declare" => TokenKind::TDeclare,
                    "default" => TokenKind::TDefault,
                    "delete" => TokenKind::TDelete,
                    "do" => TokenKind::TDo,
                    "else" => TokenKind::TElse,
                    "enum" => TokenKind::TEnum,
                    "export" => TokenKind::TExport,
                    "extends" => TokenKind::TExtends,
                    "false" => TokenKind::TFalse,
                    "finally" => TokenKind::TFinally,
                    "for" => TokenKind::TFor,
                    "function" => TokenKind::TFunction,
                    "if" => TokenKind::TIf,
                    "implements" => TokenKind::TImplements,
                    "import" => TokenKind::TImport,
                    "in" => TokenKind::TIn,
                    "instanceof" => TokenKind::TInstanceof,
                    "interface" => TokenKind::TInterface,
                    "let" => TokenKind::TLet,
                    "match" => TokenKind::TMatch,
                    "record" => TokenKind::TRecord,
                    "new" => TokenKind::TNew,
                    "null" => TokenKind::TNull,
                    "of" => TokenKind::TOf,
                    "opaque" => TokenKind::TOpaque,
                    "package" => TokenKind::TPackage,
                    "private" => TokenKind::TPrivate,
                    "protected" => TokenKind::TProtected,
                    "public" => TokenKind::TPublic,
                    "return" => TokenKind::TReturn,
                    "static" => TokenKind::TStatic,
                    "super" => TokenKind::TSuper,
                    "switch" => TokenKind::TSwitch,
                    "this" => TokenKind::TThis,
                    "throw" => TokenKind::TThrow,
                    "true" => TokenKind::TTrue,
                    "try" => TokenKind::TTry,
                    "type" => TokenKind::TType,
                    "typeof" => TokenKind::TTypeof,
                    "var" => TokenKind::TVar,
                    "void" => TokenKind::TVoid,
                    "while" => TokenKind::TWhile,
                    "with" => TokenKind::TWith,
                    "yield" => TokenKind::TYield,
                    _ => {
                        let value = decode_identifier(errors, env, start_offset, id_str);
                        TokenKind::TIdentifier {
                            loc: loc.dupe(),
                            value: FlowSmolStr::new(value),
                            raw: FlowSmolStr::new(id_str),
                        }
                    }
                }
            };
            TokenResult::Token(loc, token_kind)
        }

        MainToken::Other => {
            if js_id_unicode::is_valid_js_identifier_start(
                lexer.slice().chars().next().unwrap() as u32
            ) {
                let start_offset = lexer.span().start;
                loop_id_continues(lexer);
                let end_offset = lexer.span().end;
                let loc = env.loc_of_offsets(start_offset, end_offset);
                let id_str = &lexer.source()[start_offset..end_offset];
                let value = decode_identifier(errors, env, start_offset, id_str);
                let token_kind = TokenKind::TIdentifier {
                    loc: loc.dupe(),
                    value: FlowSmolStr::new(value),
                    raw: FlowSmolStr::new(id_str),
                };
                TokenResult::Token(loc, token_kind)
            } else {
                let loc = env.loc_of_span(&lexer.span());
                errors.push(loc.dupe(), ParseError::UnexpectedTokenIllegal);
                let s = lexer.slice().to_owned();
                TokenResult::Token(loc, TokenKind::TError(s))
            }
        }
    }
}

fn regexp_class<'a>(
    mut lexer: logos::Lexer<'a, RegexpClassToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
    buf: &mut String,
) -> logos::Lexer<'a, RegexpClassToken> {
    loop {
        match lexer.next() {
            // EOF
            None => {
                return lexer;
            }
            // \\, \\]
            Some(Ok(RegexpClassToken::EscapedBackslash))
            | Some(Ok(RegexpClassToken::EscapedClosingBracket)) => {
                buf.push_str(lexer.slice());
            }
            // ]
            Some(Ok(RegexpClassToken::ClosingBracket)) => {
                buf.push_str(lexer.slice());
                return lexer;
            }
            Some(Ok(RegexpClassToken::LineTerminatorSequence)) => {
                let range = lexer.span();
                let loc = env.loc_of_span(&range);
                env.new_line(range);
                errors.push(loc, ParseError::UnterminatedRegExp);
                return lexer;
            }
            Some(Ok(RegexpClassToken::Other)) => {
                buf.push_str(lexer.slice());
            }
            Some(Err(())) => unreachable!("unreachable regexp_class"),
        }
    }
}

fn regexp_body<'a>(
    mut lexer: logos::Lexer<'a, RegexpBodyToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
    buf: &mut String,
) -> (logos::Lexer<'a, RegexpBodyToken>, &'a str) {
    loop {
        match lexer.next() {
            None => {
                let end = lexer.source().len();
                let loc = env.loc_of_offsets(end, end);
                errors.push(loc, ParseError::UnterminatedRegExp);
                return (lexer, "");
            }
            Some(Ok(RegexpBodyToken::BackslashFollowedByLineTerminator)) => {
                let range = lexer.span();
                let loc = env.loc_of_span(&range);
                env.new_line(range);
                errors.push(loc, ParseError::UnterminatedRegExp);
                return (lexer, "");
            }
            Some(Ok(RegexpBodyToken::BackslashFollowedByAny)) => {
                buf.push_str(lexer.slice());
            }
            Some(Ok(RegexpBodyToken::SlashWithFlags)) => {
                let lexeme = lexer.slice();
                // Skip the '/' and get flags
                let flags = &lexeme[1..];
                return (lexer, flags);
            }
            Some(Ok(RegexpBodyToken::Slash)) => {
                return (lexer, "");
            }
            Some(Ok(RegexpBodyToken::OpenBracket)) => {
                buf.push('[');
                lexer = regexp_class(lexer.morph(), env, errors, buf).morph();
            }
            Some(Ok(RegexpBodyToken::LineTerminatorSequence)) => {
                let range = lexer.span();
                let loc = env.loc_of_span(&range);
                env.new_line(range);
                errors.push(loc, ParseError::UnterminatedRegExp);
                return (lexer, "");
            }
            Some(Ok(RegexpBodyToken::Other)) => {
                buf.push_str(lexer.slice());
            }
            Some(Err(())) => unreachable!("unreachable regexp_body"),
        }
    }
}

fn regexp_inner<'a>(
    lexer: &mut logos::Lexer<'a, RegexpToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
) -> TokenResult {
    let token = match lexer.next() {
        None => {
            let end_pos = lexer.source().len();
            let loc = env.loc_of_offsets(end_pos, end_pos);
            return TokenResult::Token(loc, TokenKind::TEof);
        }
        Some(Err(_)) => unreachable!("unreachable regexp"),
        Some(Ok(t)) => t,
    };
    match token {
        RegexpToken::LineTerminatorSequence => {
            env.new_line(lexer.span());
            TokenResult::Continue
        }
        RegexpToken::Whitespace => TokenResult::Continue,
        RegexpToken::LineCommentStart => {
            let start = env.pos_at_offset(lexer.span().start);
            let mut buf = String::new();
            let mut comment_lexer = lexer.clone().morph(); // clone needed: logos::Lexer doesn't implement Dupe
            let end = line_comment(&mut comment_lexer, env, &mut buf);
            *lexer = comment_lexer.morph();
            let c = mk_comment(env, start, end, buf, false);
            TokenResult::Comment(c)
        }
        RegexpToken::BlockCommentStart => {
            let start = env.pos_at_offset(lexer.span().start);
            let mut buf = String::new();
            let mut comment_lexer = lexer.clone().morph(); // clone needed: logos::Lexer doesn't implement Dupe
            let end_offset = comment(&mut comment_lexer, env, errors, &mut buf);
            let end = env.pos_at_offset(end_offset);
            let c = mk_comment(env, start, end, buf, true);
            *lexer = comment_lexer.morph();
            TokenResult::Comment(c)
        }
        RegexpToken::Slash => {
            let start = env.pos_at_offset(lexer.span().start);
            let mut buf = String::new();
            let (regexp_lexer, flags) = regexp_body(lexer.clone().morph(), env, errors, &mut buf);
            *lexer = regexp_lexer.morph();
            let end = env.pos_at_offset(lexer.span().end);
            let loc = Loc {
                source: env.source(),
                start,
                end,
            };
            TokenResult::Token(
                loc.dupe(),
                TokenKind::TRegexp(loc, FlowSmolStr::new(&buf), FlowSmolStr::new(flags)),
            )
        }
        RegexpToken::Other => {
            let loc = env.loc_of_span(&lexer.span());
            errors.push(loc.dupe(), ParseError::UnexpectedTokenIllegal);
            let s = lexer.slice().to_owned();
            TokenResult::Token(loc, TokenKind::TError(s))
        }
    }
}

fn decode_html_entity(entity: &str) -> Option<u32> {
    match entity {
        "quot" => Some(0x0022),
        "amp" => Some(0x0026),
        "apos" => Some(0x0027),
        "lt" => Some(0x003C),
        "gt" => Some(0x003E),
        "nbsp" => Some(0x00A0),
        "iexcl" => Some(0x00A1),
        "cent" => Some(0x00A2),
        "pound" => Some(0x00A3),
        "curren" => Some(0x00A4),
        "yen" => Some(0x00A5),
        "brvbar" => Some(0x00A6),
        "sect" => Some(0x00A7),
        "uml" => Some(0x00A8),
        "copy" => Some(0x00A9),
        "ordf" => Some(0x00AA),
        "laquo" => Some(0x00AB),
        "not" => Some(0x00AC),
        "shy" => Some(0x00AD),
        "reg" => Some(0x00AE),
        "macr" => Some(0x00AF),
        "deg" => Some(0x00B0),
        "plusmn" => Some(0x00B1),
        "sup2" => Some(0x00B2),
        "sup3" => Some(0x00B3),
        "acute" => Some(0x00B4),
        "micro" => Some(0x00B5),
        "para" => Some(0x00B6),
        "middot" => Some(0x00B7),
        "cedil" => Some(0x00B8),
        "sup1" => Some(0x00B9),
        "ordm" => Some(0x00BA),
        "raquo" => Some(0x00BB),
        "frac14" => Some(0x00BC),
        "frac12" => Some(0x00BD),
        "frac34" => Some(0x00BE),
        "iquest" => Some(0x00BF),
        "Agrave" => Some(0x00C0),
        "Aacute" => Some(0x00C1),
        "Acirc" => Some(0x00C2),
        "Atilde" => Some(0x00C3),
        "Auml" => Some(0x00C4),
        "Aring" => Some(0x00C5),
        "AElig" => Some(0x00C6),
        "Ccedil" => Some(0x00C7),
        "Egrave" => Some(0x00C8),
        "Eacute" => Some(0x00C9),
        "Ecirc" => Some(0x00CA),
        "Euml" => Some(0x00CB),
        "Igrave" => Some(0x00CC),
        "Iacute" => Some(0x00CD),
        "Icirc" => Some(0x00CE),
        "Iuml" => Some(0x00CF),
        "ETH" => Some(0x00D0),
        "Ntilde" => Some(0x00D1),
        "Ograve" => Some(0x00D2),
        "Oacute" => Some(0x00D3),
        "Ocirc" => Some(0x00D4),
        "Otilde" => Some(0x00D5),
        "Ouml" => Some(0x00D6),
        "times" => Some(0x00D7),
        "Oslash" => Some(0x00D8),
        "Ugrave" => Some(0x00D9),
        "Uacute" => Some(0x00DA),
        "Ucirc" => Some(0x00DB),
        "Uuml" => Some(0x00DC),
        "Yacute" => Some(0x00DD),
        "THORN" => Some(0x00DE),
        "szlig" => Some(0x00DF),
        "agrave" => Some(0x00E0),
        "aacute" => Some(0x00E1),
        "acirc" => Some(0x00E2),
        "atilde" => Some(0x00E3),
        "auml" => Some(0x00E4),
        "aring" => Some(0x00E5),
        "aelig" => Some(0x00E6),
        "ccedil" => Some(0x00E7),
        "egrave" => Some(0x00E8),
        "eacute" => Some(0x00E9),
        "ecirc" => Some(0x00EA),
        "euml" => Some(0x00EB),
        "igrave" => Some(0x00EC),
        "iacute" => Some(0x00ED),
        "icirc" => Some(0x00EE),
        "iuml" => Some(0x00EF),
        "eth" => Some(0x00F0),
        "ntilde" => Some(0x00F1),
        "ograve" => Some(0x00F2),
        "oacute" => Some(0x00F3),
        "ocirc" => Some(0x00F4),
        "otilde" => Some(0x00F5),
        "ouml" => Some(0x00F6),
        "divide" => Some(0x00F7),
        "oslash" => Some(0x00F8),
        "ugrave" => Some(0x00F9),
        "uacute" => Some(0x00FA),
        "ucirc" => Some(0x00FB),
        "uuml" => Some(0x00FC),
        "yacute" => Some(0x00FD),
        "thorn" => Some(0x00FE),
        "yuml" => Some(0x00FF),
        "OElig" => Some(0x0152),
        "oelig" => Some(0x0153),
        "Scaron" => Some(0x0160),
        "scaron" => Some(0x0161),
        "Yuml" => Some(0x0178),
        "fnof" => Some(0x0192),
        "circ" => Some(0x02C6),
        "tilde" => Some(0x02DC),
        "Alpha" => Some(0x0391),
        "Beta" => Some(0x0392),
        "Gamma" => Some(0x0393),
        "Delta" => Some(0x0394),
        "Epsilon" => Some(0x0395),
        "Zeta" => Some(0x0396),
        "Eta" => Some(0x0397),
        "Theta" => Some(0x0398),
        "Iota" => Some(0x0399),
        "Kappa" => Some(0x039A),
        "Lambda" => Some(0x039B),
        "Mu" => Some(0x039C),
        "Nu" => Some(0x039D),
        "Xi" => Some(0x039E),
        "Omicron" => Some(0x039F),
        "Pi" => Some(0x03A0),
        "Rho" => Some(0x03A1),
        "Sigma" => Some(0x03A3),
        "Tau" => Some(0x03A4),
        "Upsilon" => Some(0x03A5),
        "Phi" => Some(0x03A6),
        "Chi" => Some(0x03A7),
        "Psi" => Some(0x03A8),
        "Omega" => Some(0x03A9),
        "alpha" => Some(0x03B1),
        "beta" => Some(0x03B2),
        "gamma" => Some(0x03B3),
        "delta" => Some(0x03B4),
        "epsilon" => Some(0x03B5),
        "zeta" => Some(0x03B6),
        "eta" => Some(0x03B7),
        "theta" => Some(0x03B8),
        "iota" => Some(0x03B9),
        "kappa" => Some(0x03BA),
        "lambda" => Some(0x03BB),
        "mu" => Some(0x03BC),
        "nu" => Some(0x03BD),
        "xi" => Some(0x03BE),
        "omicron" => Some(0x03BF),
        "pi" => Some(0x03C0),
        "rho" => Some(0x03C1),
        "sigmaf" => Some(0x03C2),
        "sigma" => Some(0x03C3),
        "tau" => Some(0x03C4),
        "upsilon" => Some(0x03C5),
        "phi" => Some(0x03C6),
        "chi" => Some(0x03C7),
        "psi" => Some(0x03C8),
        "omega" => Some(0x03C9),
        "thetasym" => Some(0x03D1),
        "upsih" => Some(0x03D2),
        "piv" => Some(0x03D6),
        "ensp" => Some(0x2002),
        "emsp" => Some(0x2003),
        "thinsp" => Some(0x2009),
        "zwnj" => Some(0x200C),
        "zwj" => Some(0x200D),
        "lrm" => Some(0x200E),
        "rlm" => Some(0x200F),
        "ndash" => Some(0x2013),
        "mdash" => Some(0x2014),
        "lsquo" => Some(0x2018),
        "rsquo" => Some(0x2019),
        "sbquo" => Some(0x201A),
        "ldquo" => Some(0x201C),
        "rdquo" => Some(0x201D),
        "bdquo" => Some(0x201E),
        "dagger" => Some(0x2020),
        "Dagger" => Some(0x2021),
        "bull" => Some(0x2022),
        "hellip" => Some(0x2026),
        "permil" => Some(0x2030),
        "prime" => Some(0x2032),
        "Prime" => Some(0x2033),
        "lsaquo" => Some(0x2039),
        "rsaquo" => Some(0x203A),
        "oline" => Some(0x203E),
        "frasl" => Some(0x2044),
        "euro" => Some(0x20AC),
        "image" => Some(0x2111),
        "weierp" => Some(0x2118),
        "real" => Some(0x211C),
        "trade" => Some(0x2122),
        "alefsym" => Some(0x2135),
        "larr" => Some(0x2190),
        "uarr" => Some(0x2191),
        "rarr" => Some(0x2192),
        "darr" => Some(0x2193),
        "harr" => Some(0x2194),
        "crarr" => Some(0x21B5),
        "lArr" => Some(0x21D0),
        "uArr" => Some(0x21D1),
        "rArr" => Some(0x21D2),
        "dArr" => Some(0x21D3),
        "hArr" => Some(0x21D4),
        "forall" => Some(0x2200),
        "part" => Some(0x2202),
        "exist" => Some(0x2203),
        "empty" => Some(0x2205),
        "nabla" => Some(0x2207),
        "isin" => Some(0x2208),
        "notin" => Some(0x2209),
        "ni" => Some(0x220B),
        "prod" => Some(0x220F),
        "sum" => Some(0x2211),
        "minus" => Some(0x2212),
        "lowast" => Some(0x2217),
        "radic" => Some(0x221A),
        "prop" => Some(0x221D),
        "infin" => Some(0x221E),
        "ang" => Some(0x2220),
        "and" => Some(0x2227),
        "or" => Some(0x2228),
        "cap" => Some(0x2229),
        "cup" => Some(0x222A),
        "int" => Some(0x222B),
        "there4" => Some(0x2234),
        "sim" => Some(0x223C),
        "cong" => Some(0x2245),
        "asymp" => Some(0x2248),
        "ne" => Some(0x2260),
        "equiv" => Some(0x2261),
        "le" => Some(0x2264),
        "ge" => Some(0x2265),
        "sub" => Some(0x2282),
        "sup" => Some(0x2283),
        "nsub" => Some(0x2284),
        "sube" => Some(0x2286),
        "supe" => Some(0x2287),
        "oplus" => Some(0x2295),
        "otimes" => Some(0x2297),
        "perp" => Some(0x22A5),
        "sdot" => Some(0x22C5),
        "lceil" => Some(0x2308),
        "rceil" => Some(0x2309),
        "lfloor" => Some(0x230A),
        "rfloor" => Some(0x230B),
        "lang" => Some(0x27E8), // 0x2329 in HTML4
        "rang" => Some(0x27E9), // 0x232A in HTML4
        "loz" => Some(0x25CA),
        "spades" => Some(0x2660),
        "clubs" => Some(0x2663),
        "hearts" => Some(0x2665),
        "diams" => Some(0x2666),
        _ => None,
    }
}

// In OCaml sedlex, jsx_child_text uses longest-match semantics. The pattern
// `Plus (Compl ('<' | '{' | '&' | eof | line_terminator_sequence_start))`
// includes '>' and '}'. When '>' or '}' appears and is followed by more
// "normal" chars, the Plus pattern produces a longer match than the single-char
// '>' or '}' patterns, so no error is emitted. The error only fires when
// '>' or '}' would tie at length 1 (i.e., the next char is '<', '{', '&',
// a line terminator, or eof), and the earlier error pattern wins the tie.
//
// This helper checks whether a following character would extend a
// `Plus (Compl ...)` match beyond a single '>' or '}'.
fn jsx_child_text_remainder_has_normal(remainder: &str) -> bool {
    // match multi-char substrings that don't contain the start chars of the above patterns
    match remainder.chars().next() {
        None => false, // eof
        Some('<') => false,
        Some('{') => false,
        Some('&') => false,
        // line_terminator_sequence_start
        Some('\r') => false,
        Some('\n') => false,
        Some('\u{2028}') => false,
        Some('\u{2029}') => false,
        Some(_) => true,
    }
}

// (* let rec jsx_child_text env buf raw lexbuf = *)
fn jsx_child_text<'a>(
    mut lexer: logos::Lexer<'a, JsxChildTextToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
    buf: &mut String,
    raw: &mut String,
) -> logos::Lexer<'a, JsxChildTextToken> {
    // In OCaml, `normal_prev` doesn't exist. Instead, sedlex longest-match
    // semantics handle '>' and '}' within `Plus (Compl ...)`. The Rust logos
    // lexer tokenizes them separately, so we simulate the sedlex behavior:
    // - If preceded by normal text (`normal_prev`), '>' and '}' are always
    //   part of the text (the `Plus` already started matching).
    // - If NOT preceded by normal text, '>' and '}' are only text if followed
    //   by a "normal" char (making `Plus (Compl ...)` match 2+ chars, winning
    //   over the single-char error pattern). Otherwise, the error pattern wins.
    let mut normal_prev = false;
    loop {
        let remainder = lexer.remainder();
        if remainder.starts_with('<') || remainder.starts_with('{') {
            // Don't actually want to consume these guys
            // yet...they're not part of the JSX text
            return lexer;
        }

        match lexer.next() {
            Some(Ok(JsxChildTextToken::GreaterThan)) => {
                // In OCaml sedlex, '>' is in `Compl ('<' | '{' | '&' | eof | lt_start)`.
                // When preceded by normal text, or when followed by a "normal" char,
                // `Plus (Compl ...)` matches it as text. Otherwise, the '>' error arm wins.
                if normal_prev || jsx_child_text_remainder_has_normal(lexer.remainder()) {
                    let c = lexer.slice();
                    raw.push_str(c);
                    buf.push_str(c);
                    normal_prev = true;
                } else {
                    let loc = env.loc_of_span(&lexer.span());
                    errors.push(
                        loc,
                        ParseError::UnexpectedTokenWithSuggestion(
                            ">".to_owned(),
                            "{'>'}".to_owned(),
                        ),
                    );
                    return lexer;
                }
            }
            Some(Ok(JsxChildTextToken::RCurly)) => {
                // Same longest-match logic as '>' above.
                if normal_prev || jsx_child_text_remainder_has_normal(lexer.remainder()) {
                    let c = lexer.slice();
                    raw.push_str(c);
                    buf.push_str(c);
                    normal_prev = true;
                } else {
                    let loc = env.loc_of_span(&lexer.span());
                    errors.push(
                        loc,
                        ParseError::UnexpectedTokenWithSuggestion(
                            "}".to_owned(),
                            "{'}'}".to_owned(),
                        ),
                    );
                    return lexer;
                }
            }
            None => {
                let end = lexer.source().len();
                let loc = env.loc_of_offsets(end, end);
                errors.push(loc, ParseError::UnexpectedTokenIllegal);
                return lexer;
            }
            Some(Ok(JsxChildTextToken::LineTerminatorSequence)) => {
                let lt = lexer.slice();
                raw.push_str(lt);
                buf.push_str(lt);
                env.new_line(lexer.span());
                normal_prev = true;
            }
            Some(Ok(JsxChildTextToken::HexEntityRef)) => {
                let s = lexer.slice();
                // Extract hex digits between &#x and ;
                let n = &s[3..s.len() - 1];
                raw.push_str(s);
                if let Ok(code) = u32::from_str_radix(n, 16) {
                    add_codepoint_to_string(buf, code);
                }
                normal_prev = true;
            }
            Some(Ok(JsxChildTextToken::DecimalEntityRef)) => {
                let s = lexer.slice();
                // Extract decimal digits between &# and ;
                let n = &s[2..s.len() - 1];
                raw.push_str(s);
                if let Ok(code) = n.parse::<u32>() {
                    add_codepoint_to_string(buf, code);
                }
                normal_prev = true;
            }
            Some(Ok(JsxChildTextToken::NamedEntityRef)) => {
                let s = lexer.slice();
                let entity = &s[1..s.len() - 1];
                raw.push_str(s);
                match decode_html_entity(entity) {
                    Some(code) => add_codepoint_to_string(buf, code),
                    None => {
                        buf.push_str(s);
                    }
                }
                normal_prev = true;
            }
            Some(Ok(JsxChildTextToken::Other)) => {
                let c = lexer.slice();
                raw.push_str(c);
                buf.push_str(c);
                normal_prev = true;
            }
            Some(Err(())) => unreachable!("unreachable jsx_child_text"),
        }
    }
}

fn jsx_quote_text<'a>(
    mut lexer: logos::Lexer<'a, JsxQuoteTextToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
    single: bool,
    buf: &mut String,
    raw: &mut String,
) -> logos::Lexer<'a, JsxQuoteTextToken> {
    loop {
        match lexer.next() {
            // Single quote
            Some(Ok(JsxQuoteTextToken::SingleQuote)) => {
                if single {
                    // End of single-quoted string
                    return lexer;
                } else {
                    // Single quote inside double-quoted string
                    raw.push('\'');
                    buf.push('\'');
                }
            }
            // Double quote
            Some(Ok(JsxQuoteTextToken::DoubleQuote)) => {
                if !single {
                    return lexer;
                } else {
                    raw.push('"');
                    buf.push('"');
                }
            }
            // EOF - illegal
            None => {
                let end = lexer.source().len();
                let loc = env.loc_of_offsets(end, end);
                errors.push(loc, ParseError::UnexpectedTokenIllegal);
                return lexer;
            }
            Some(Ok(JsxQuoteTextToken::LineTerminatorSequence)) => {
                let lt = lexer.slice();
                raw.push_str(lt);
                buf.push_str(lt);
                env.new_line(lexer.span());
            }
            Some(Ok(JsxQuoteTextToken::HexEntityRef)) => {
                let s = lexer.slice();
                let n = &s[3..s.len() - 1];
                raw.push_str(s);
                if let Ok(code) = u32::from_str_radix(n, 16) {
                    add_codepoint_to_string(buf, code);
                }
            }
            Some(Ok(JsxQuoteTextToken::DecimalEntityRef)) => {
                let s = lexer.slice();
                let n = &s[2..s.len() - 1];
                raw.push_str(s);
                if let Ok(code) = n.parse::<u32>() {
                    add_codepoint_to_string(buf, code);
                }
            }
            Some(Ok(JsxQuoteTextToken::NamedEntityRef)) => {
                let s = lexer.slice();
                let entity = &s[1..s.len() - 1];
                raw.push_str(s);
                match decode_html_entity(entity) {
                    Some(code) => add_codepoint_to_string(buf, code),
                    None => {
                        buf.push_str(s);
                    }
                }
            }
            Some(Ok(JsxQuoteTextToken::Text)) | Some(Ok(JsxQuoteTextToken::Other)) => {
                let c = lexer.slice();
                raw.push_str(c);
                buf.push_str(c);
            }
            Some(Err(())) => unreachable!("unreachable jsx_quote_text"),
        }
    }
}

fn jsx_tag_inner<'a>(
    lexer: &mut logos::Lexer<'a, JsxTagToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
) -> TokenResult {
    let token = match lexer.next() {
        None => {
            let end_pos = lexer.source().len();
            let loc = env.loc_of_offsets(end_pos, end_pos);
            return TokenResult::Token(loc, TokenKind::TEof);
        }
        Some(Err(_)) => unreachable!("unreachable jsx_tag"),
        Some(Ok(t)) => t,
    };
    match token {
        JsxTagToken::LineTerminatorSequence => {
            env.new_line(lexer.span());
            TokenResult::Continue
        }
        JsxTagToken::Whitespace => TokenResult::Continue,
        JsxTagToken::LineCommentStart => {
            let start = env.pos_at_offset(lexer.span().start);
            let mut buf = String::new();
            let mut comment_lexer = lexer.clone().morph();
            let end = line_comment(&mut comment_lexer, env, &mut buf);
            *lexer = comment_lexer.morph();
            let c = mk_comment(env, start, end, buf, false);
            TokenResult::Comment(c)
        }
        JsxTagToken::BlockCommentStart => {
            let start = env.pos_at_offset(lexer.span().start);
            let mut buf = String::new();
            let mut comment_lexer = lexer.clone().morph();
            let end_offset = comment(&mut comment_lexer, env, errors, &mut buf);
            let end = env.pos_at_offset(end_offset);
            let c = mk_comment(env, start, end, buf, true);
            *lexer = comment_lexer.morph();
            TokenResult::Comment(c)
        }
        JsxTagToken::LessThan => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TLessThan)
        }
        JsxTagToken::Div => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TDiv)
        }
        JsxTagToken::GreaterThan => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TGreaterThan)
        }
        JsxTagToken::LCurly => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TLcurly)
        }
        JsxTagToken::Colon => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TColon)
        }
        JsxTagToken::Period => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TPeriod)
        }
        JsxTagToken::Assign => {
            let loc = env.loc_of_span(&lexer.span());
            TokenResult::Token(loc, TokenKind::TAssign)
        }
        JsxTagToken::SingleQuote | JsxTagToken::DoubleQuote => {
            let quote = lexer.slice();
            let start = env.pos_at_offset(lexer.span().start);
            let mut buf = String::new();
            let mut raw = String::new();
            raw.push_str(quote);
            let single = token == JsxTagToken::SingleQuote;
            let jsx_quote_lexer = jsx_quote_text(
                lexer.clone().morph(),
                env,
                errors,
                single,
                &mut buf,
                &mut raw,
            );
            *lexer = jsx_quote_lexer.morph();
            let end = env.pos_at_offset(lexer.span().end);
            raw.push_str(quote);
            let loc = Loc {
                source: env.source(),
                start,
                end,
            };
            TokenResult::Token(
                loc.dupe(),
                TokenKind::TJsxQuoteText(loc, FlowSmolStr::new(&buf), FlowSmolStr::new(&raw)),
            )
        }
        JsxTagToken::JsxIdStart => {
            let start_offset = lexer.span().start;
            // see #3837, we should fix it - the work could be done in decoding later - cold path
            loop_jsx_id_continues(lexer);
            let end_offset = lexer.span().end;
            let raw = &lexer.source()[start_offset..end_offset];
            let loc = env.loc_of_offsets(start_offset, end_offset);
            TokenResult::Token(
                loc.dupe(),
                TokenKind::TJsxIdentifier {
                    raw: FlowSmolStr::new(raw),
                    loc,
                },
            )
        }
        JsxTagToken::Other => {
            let loc = env.loc_of_span(&lexer.span());
            let s = lexer.slice().to_owned();
            TokenResult::Token(loc, TokenKind::TError(s))
        }
    }
}

fn jsx_child_inner<'a>(
    lexer: &mut logos::Lexer<'a, JsxChildTextToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
    mut buf: String,
    mut raw: String,
    start: Position,
) -> (Loc, TokenKind) {
    let remainder = lexer.remainder();
    let Some(next_char) = remainder.chars().next() else {
        let end = lexer.source().len();
        let loc = env.loc_of_offsets(end, end);
        return (loc, TokenKind::TEof);
    };
    match next_char {
        '<' => {
            let span_start = lexer.span().end;
            let span = span_start..(span_start + 1);
            let loc = env.loc_of_span(&span);
            lexer.bump(1);
            (loc, TokenKind::TLessThan)
        }
        '{' => {
            let span_start = lexer.span().end;
            let span = span_start..(span_start + 1);
            let loc = env.loc_of_span(&span);
            lexer.bump(1);
            (loc, TokenKind::TLcurly)
        }
        _ => {
            for lt in ["\r\n", "\r", "\n", "\u{2028}", "\u{2029}"] {
                if remainder.starts_with(lt) {
                    let span = {
                        let span_start = lexer.span().end;
                        span_start..(span_start + lt.len())
                    };
                    lexer.bump(lt.len());
                    raw.push_str(lt);
                    buf.push_str(lt);
                    env.new_line(span);
                    *lexer = jsx_child_text(lexer.clone().morph(), env, errors, &mut buf, &mut raw);
                    let end = env.pos_at_offset(lexer.span().end);
                    let loc = Loc {
                        source: env.source(),
                        start,
                        end,
                    };
                    return (
                        loc.dupe(),
                        TokenKind::TJsxChildText(
                            loc,
                            FlowSmolStr::new(&buf),
                            FlowSmolStr::new(&raw),
                        ),
                    );
                }
            }
            *lexer = jsx_child_text(lexer.clone().morph(), env, errors, &mut buf, &mut raw);
            let end = env.pos_at_offset(lexer.span().end);
            let loc = Loc {
                source: env.source(),
                start,
                end,
            };
            (
                loc.dupe(),
                TokenKind::TJsxChildText(loc, FlowSmolStr::new(&buf), FlowSmolStr::new(&raw)),
            )
        }
    }
}

fn template_tail_inner<'a>(
    lexer: &mut logos::Lexer<'a, TemplateTailToken>,
    env: &mut LexEnv,
    errors: &mut LexErrors,
) -> TokenResult {
    let token = match lexer.next() {
        None => {
            let end_pos = lexer.source().len();
            let loc = env.loc_of_offsets(end_pos, end_pos);
            return TokenResult::Token(loc, TokenKind::TEof);
        }
        Some(Err(_)) => unreachable!("unreachable template_tail"),
        Some(Ok(t)) => t,
    };
    match token {
        TemplateTailToken::LineTerminatorSequence => {
            env.new_line(lexer.span());
            TokenResult::Continue
        }
        TemplateTailToken::Whitespace => TokenResult::Continue,
        TemplateTailToken::LineCommentStart => {
            let start = env.pos_at_offset(lexer.span().start);
            let mut buf = String::new();
            let mut comment_lexer = lexer.clone().morph(); // clone needed: logos::Lexer doesn't implement Dupe
            let end = line_comment(&mut comment_lexer, env, &mut buf);
            *lexer = comment_lexer.morph();
            let c = mk_comment(env, start, end, buf, false);
            TokenResult::Comment(c)
        }
        TemplateTailToken::BlockCommentStart => {
            let start = env.pos_at_offset(lexer.span().start);
            let mut buf = String::new();
            let mut comment_lexer = lexer.clone().morph(); // clone needed: logos::Lexer doesn't implement Dupe
            let end_offset = comment(&mut comment_lexer, env, errors, &mut buf);
            let end = env.pos_at_offset(end_offset);
            let c = mk_comment(env, start, end, buf, true);
            *lexer = comment_lexer.morph();
            TokenResult::Comment(c)
        }
        TemplateTailToken::RCurly => {
            let start = env.pos_at_offset(lexer.span().start);
            let mut value = String::new();
            let mut raw = String::new();
            let (template_part_lexer, is_tail) =
                template_part(lexer.clone().morph(), env, errors, &mut value, &mut raw);
            *lexer = template_part_lexer.morph();
            let end = env.pos_at_offset(lexer.span().end);
            let loc = Loc {
                source: env.source(),
                start,
                end,
            };
            TokenResult::Token(
                loc.dupe(),
                TokenKind::TTemplatePart(TemplatePart {
                    loc,
                    value: FlowSmolStr::new(&value),
                    raw: FlowSmolStr::new(&raw),
                    head: false,
                    tail: is_tail,
                }),
            )
        }
        TemplateTailToken::Other => {
            let loc = env.loc_of_span(&lexer.span());
            errors.push(loc.dupe(), ParseError::UnexpectedTokenIllegal);
            TokenResult::Token(
                loc.dupe(),
                TokenKind::TTemplatePart(TemplatePart {
                    loc,
                    value: FlowSmolStr::new_inline(""),
                    raw: FlowSmolStr::new_inline(""),
                    head: false,
                    tail: true,
                }),
            )
        }
    }
}

#[derive(Clone)]
pub struct LexResult {
    pub loc: Loc,
    pub token_kind: TokenKind,
    pub errors: LexErrors,
    pub comments: Vec<Comment<Loc>>,
}

pub(super) fn jsx_child<'a>(
    lexer: &mut logos::Lexer<'a, JsxChildTextToken>,
    env: &mut LexEnv,
) -> LexResult {
    let mut errors = LexErrors::empty();
    // yes, the _start_ of the child is the _end_pos_ of the previous token
    let start = env.pos_at_offset(lexer.span().end);
    let buf = String::new();
    let raw = String::new();
    let (loc, token_kind) = jsx_child_inner(lexer, env, &mut errors, buf, raw, start);
    LexResult {
        loc,
        token_kind,
        errors,
        comments: Vec::new(),
    }
}

fn lex_wrapped<L, F: FnMut(&mut L, &mut LexEnv, &mut LexErrors) -> TokenResult>(
    lexer: &mut L,
    env: &mut LexEnv,
    f: &mut F,
) -> LexResult {
    let mut comments = Vec::new();
    let mut errors = LexErrors::empty();
    loop {
        let result = f(lexer, env, &mut errors);
        match result {
            TokenResult::Token(loc, token_kind) => {
                env.last_loc = loc.dupe();
                return LexResult {
                    loc,
                    token_kind,
                    errors,
                    comments,
                };
            }
            TokenResult::Comment(comment) => {
                env.last_loc = comment.loc.dupe();
                comments.push(comment);
            }
            TokenResult::Continue => {}
        }
    }
}

pub(super) fn regexp<'a>(lexer: &mut logos::Lexer<'a, RegexpToken>, env: &mut LexEnv) -> LexResult {
    lex_wrapped(lexer, env, &mut regexp_inner)
}

pub(super) fn jsx_tag<'a>(
    lexer: &mut logos::Lexer<'a, JsxTagToken>,
    env: &mut LexEnv,
) -> LexResult {
    lex_wrapped(lexer, env, &mut jsx_tag_inner)
}

pub(super) fn template_tail_start<'a>(
    lexer: &mut logos::Lexer<'a, TemplateTailToken>,
    env: &mut LexEnv,
) -> LexResult {
    lex_wrapped(lexer, env, &mut template_tail_inner)
}

pub(super) fn type_token<'a>(
    lexer: &mut logos::Lexer<'a, MainToken>,
    env: &mut LexEnv,
) -> LexResult {
    lex_wrapped(lexer, env, &mut |lexer, env, errors| {
        token_base_inner(true, lexer, env, errors)
    })
}

pub fn token<'a>(lexer: &mut logos::Lexer<'a, MainToken>, env: &mut LexEnv) -> LexResult {
    lex_wrapped(lexer, env, &mut |lexer, env, errors| {
        token_base_inner(false, lexer, env, errors)
    })
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use crate::lex_env::LexEnv;
    use crate::lex_env::LexErrors;
    use crate::logos_tokens::MainToken;
    use crate::token::TokenKind;

    #[test]
    fn unicode_lexing_tests() {
        let mut lexer = MainToken::lexer("ૹ");
        let mut lex_env = LexEnv::new(None, false);
        let mut lex_errors = LexErrors::empty();
        let token_kind =
            match super::token_base_inner(false, &mut lexer, &mut lex_env, &mut lex_errors) {
                super::TokenResult::Token(_, token_kind) => token_kind,
                crate::flow_lexer::TokenResult::Comment(_) => panic!("Should not get comments"),
                crate::flow_lexer::TokenResult::Continue => panic!("Should not get continue"),
            };
        assert!(matches!(token_kind, TokenKind::TIdentifier { .. }));
    }
}
