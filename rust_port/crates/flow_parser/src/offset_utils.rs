/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::loc::Position;
use crate::wtf8;

/// Classify each codepoint. We care about how many bytes each codepoint takes, in order to
/// compute offsets in terms of bytes instead of codepoints. We also care about various kinds of
/// newlines. To reduce memory, it is important that this is a basic variant with no parameters
/// (so, don't make it `Chars of int`).
#[derive(Debug, Clone, Copy)]
enum Kind {
    // Char has a codepoint greater than or equal to 0x0 but less than 0x80
    Chars0x0,
    // Char has a codepoint greater than or equal to 0x80 but less than 0x800
    Chars0x80,
    Chars0x800,
    Chars0x10000,
    Malformed,
    Cr,
    Nl,
    Ls,
}

// Gives the size in bytes of the character's UTF-8 encoding
fn utf8_size_of_kind(kind: &Kind) -> u32 {
    match kind {
        Kind::Chars0x0 => 1,
        Kind::Chars0x80 => 2,
        Kind::Chars0x800 => 3,
        Kind::Chars0x10000 => 4,
        Kind::Malformed => 1,
        Kind::Cr => 1,
        Kind::Nl => 1,
        Kind::Ls => 3,
    }
}

// Gives the size in code units (16-bit blocks) of the character's UTF-16 encoding
fn js_size_of_kind(kind: &Kind) -> u32 {
    match kind {
        Kind::Chars0x0 | Kind::Chars0x80 | Kind::Chars0x800 => 1,
        Kind::Chars0x10000 => 2,
        Kind::Malformed => 1,
        Kind::Cr => 1,
        Kind::Nl => 1,
        Kind::Ls => 1,
    }
}

// table from 0-based line number and 0-based column number to the offset at that point
#[derive(Debug)]
pub struct OffsetTable {
    utf8_table: Vec<Vec<u32>>,
    js_table: Vec<Vec<u32>>,
    kind: OffsetKind,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum OffsetKind {
    Utf8,
    JavaScript,
}

impl OffsetTable {
    pub fn make(text: &str) -> OffsetTable {
        Self::make_with_kind(OffsetKind::Utf8, text)
    }

    pub fn make_with_kind(kind: OffsetKind, text: &str) -> OffsetTable {
        // Using Wtf8 allows us to properly track multi-byte characters, so that we increment the column
        // by 1 for a multi-byte character, but increment the offset by the number of bytes in the
        // character. It also keeps us from incrementing the line number if a multi-byte character happens
        // to include e.g. the codepoint for '\n' as a second-fourth byte.
        fn add_code_point(acc: &mut Vec<Kind>, _index: usize, chr: wtf8::Codepoint) {
            let kind = match chr {
                wtf8::Codepoint::Point(code) => {
                    if code == 0x2028 || code == 0x2029 {
                        Kind::Ls
                    } else if code == 0xA {
                        Kind::Nl
                    } else if code == 0xD {
                        Kind::Cr
                    } else if code >= 0x10000 {
                        Kind::Chars0x10000
                    } else if code >= 0x800 {
                        Kind::Chars0x800
                    } else if code >= 0x80 {
                        Kind::Chars0x80
                    } else {
                        Kind::Chars0x0
                    }
                }
                wtf8::Codepoint::Malformed => Kind::Malformed,
            };
            acc.push(kind);
        }

        // Traverses a `kind list`, breaking it up into an `Vec<Vec<i32>>`, where each `Vec<i32>`
        // contains the offsets at each character (aka codepoint) of a line.
        fn build_table(size_of_kind: fn(&Kind) -> u32, kinds: &[Kind]) -> Vec<Vec<u32>> {
            let mut offset: u32 = 0;
            let mut line = Vec::new();
            let mut acc = Vec::new();
            let mut i = 0;
            while i < kinds.len() {
                match &kinds[i] {
                    Kind::Cr if i + 1 < kinds.len() && matches!(kinds[i + 1], Kind::Nl) => {
                        // https://www.ecma-international.org/ecma-262/5.1/#sec-7.3 says that "\r\n"
                        // should be treated like a single line terminator, even though both '\r'
                        // and '\n' are line terminators in their own right.
                        line.push(offset);
                        line.push(offset + 2);
                        acc.push(line);
                        offset += 2;
                        line = Vec::new();
                        i += 2;
                    }
                    kind @ (Kind::Cr | Kind::Nl | Kind::Ls) => {
                        line.push(offset);
                        line.push(offset + size_of_kind(kind));
                        acc.push(line);
                        offset += size_of_kind(kind);
                        line = Vec::new();
                        i += 1;
                    }
                    kind @ (Kind::Chars0x0
                    | Kind::Chars0x80
                    | Kind::Chars0x800
                    | Kind::Chars0x10000
                    | Kind::Malformed) => {
                        line.push(offset);
                        offset += size_of_kind(kind);
                        i += 1;
                    }
                }
            }
            acc
        }

        let mut kinds = wtf8::iter_wtf_8(text.as_bytes(), None, None, Vec::new(), add_code_point);
        // Add a phantom line at the end of the file. Since end positions are reported exclusively, it
        // is possible for the lexer to output an end position with a line number one higher than the
        // last line, to indicate something such as "the entire last line." For this purpose, we can
        // return the offset that is one higher than the last legitimate offset, since it could only be
        // correctly used as an exclusive index.
        kinds.push(Kind::Nl);
        OffsetTable {
            utf8_table: build_table(utf8_size_of_kind, &kinds),
            js_table: build_table(js_size_of_kind, &kinds),
            kind,
        }
    }

    fn offset_internal(table: &[Vec<u32>], position: Position) -> Result<u32, OffsetLookupFailed> {
        // Special-case `Loc.none` so we don't try to look up line -1.
        if position.line == 0 && position.column == 0 {
            // Loc.none sets the offset as 0, so that's what we'll return here.
            return Ok(0);
        }
        // lines are 1-indexed, columns are zero-indexed
        let line_index = (position.line - 1) as usize;
        let Some(line_table) = table.get(line_index) else {
            return Err(OffsetLookupFailed {
                kind: "line",
                position,
                index: line_index,
                length: table.len(),
            });
        };
        let column = position.column as usize;
        let Some(&offset) = line_table.get(column) else {
            return Err(OffsetLookupFailed {
                kind: "column",
                position,
                index: column,
                length: line_table.len(),
            });
        };
        Ok(offset)
    }

    pub fn offset_js(&self, position: Position) -> Result<u32, OffsetLookupFailed> {
        Self::offset_internal(&self.js_table, position)
    }

    pub fn offset_utf8(&self, position: Position) -> Result<u32, OffsetLookupFailed> {
        Self::offset_internal(&self.utf8_table, position)
    }

    pub fn offset(&self, position: Position) -> Result<u32, OffsetLookupFailed> {
        let table = match self.kind {
            OffsetKind::Utf8 => &self.utf8_table,
            OffsetKind::JavaScript => &self.js_table,
        };
        Self::offset_internal(table, position)
    }

    /// Flow's position is based on byte offsets,
    /// while JS position is based on UTF16 character offsets.
    pub fn convert_flow_position_to_js_position(
        &self,
        position: Position,
    ) -> Result<Position, OffsetLookupFailed> {
        // Special-case `Loc.none` so we don't try to look up line -1.
        if position.line == 0 && position.column == 0 {
            // Loc.none sets the offset as 0, so that's what we'll return here.
            return Ok(position);
        }
        // lines are 1-indexed, columns are zero-indexed
        let line_index = (position.line - 1) as usize;
        let Some(line_table) = self.utf8_table.get(line_index) else {
            return Err(OffsetLookupFailed {
                kind: "line",
                position,
                index: line_index,
                length: self.utf8_table.len(),
            });
        };
        let target = line_table[0] + position.column as u32;
        let Ok(column_index) = line_table.binary_search(&target) else {
            return Err(OffsetLookupFailed {
                kind: "js_column",
                position,
                index: target as usize,
                length: line_table.len(),
            });
        };
        Ok(Position {
            line: position.line,
            column: column_index as i32,
        })
    }

    // let line_lengths table =
    //   Array.fold_left
    //     (fun (prev_line_end, lengths_rev) line ->
    //       let line_end = line.(Array.length line - 1) in
    //       (line_end, (line_end - prev_line_end) :: lengths_rev))
    //     (-1, [])
    //     table
    //   |> snd
    //   |> List.rev
    pub fn line_lengths(&self) -> Vec<i64> {
        let table = match self.kind {
            OffsetKind::Utf8 => &self.utf8_table,
            OffsetKind::JavaScript => &self.js_table,
        };
        let mut prev_line_end: i64 = -1;
        let mut lengths_rev: Vec<i64> = Vec::new();
        for line in table.iter() {
            let line_end = line[line.len() - 1] as i64;
            lengths_rev.push(line_end - prev_line_end);
            prev_line_end = line_end;
        }
        lengths_rev
    }

    // let contains_multibyte_character table =
    //   let exception FoundMultibyte in
    //   try
    //     Array.iter
    //       (fun line ->
    //         Array.iteri
    //           (fun i offset ->
    //             if i > 0 then
    //               let offset_before = line.(i - 1) in
    //               if offset - offset_before > 1 then raise FoundMultibyte)
    //           line)
    //       table;
    //     false
    //   with
    //   | FoundMultibyte -> true
    pub fn contains_multibyte_character(&self) -> bool {
        let table = match self.kind {
            OffsetKind::Utf8 => &self.utf8_table,
            OffsetKind::JavaScript => &self.js_table,
        };
        for line in table.iter() {
            for i in 1..line.len() {
                let offset = line[i];
                let offset_before = line[i - 1];
                if offset - offset_before > 1 {
                    return true;
                }
            }
        }
        false
    }

    #[allow(dead_code)]
    pub(super) fn debug_string(&self) -> String {
        let mut buf = String::with_capacity(4096);
        buf.push_str("UTF8:\n");
        for (line_num, line) in self.utf8_table.iter().enumerate() {
            buf.push_str(&format!("{:6}: ", line_num));
            for offset in line {
                buf.push_str(&format!("{:8} ", offset));
            }
            buf.push('\n');
        }
        buf.push_str("JS:\n");
        for (line_num, line) in self.js_table.iter().enumerate() {
            buf.push_str(&format!("{:6}: ", line_num));
            for offset in line {
                buf.push_str(&format!("{:8} ", offset));
            }
            buf.push('\n');
        }
        buf
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OffsetLookupFailed {
    pub kind: &'static str,
    pub position: Position,
    pub index: usize,
    pub length: usize,
}

impl OffsetLookupFailed {
    pub fn debug_to_string(&self) -> String {
        format!(
            "Failure while looking up {} at {:?}. Index: {}. Length: {}.",
            self.kind, self.position, self.index, self.length
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pos(line: i32, column: i32) -> Position {
        Position { line, column }
    }

    // UTF-8 encoding of code point 0x2028, line separator
    const LINE_SEP: &str = "\u{2028}";

    // UTF-8 encoding of code point 0x2029, paragraph separator
    const PAR_SEP: &str = "\u{2029}";

    // UTF-8 encoding of code point 0x1f603, some form of a smiley
    const SMILEY: &str = "\u{1f603}";

    // UTF-8 encoding of code point 0x10000, a Mycenaean Greek character. It is notable because it is
    // the lowest code point which is represented as a surrogate pair in UTF-16.
    const LINEAR_B_SYLLABLE_B008_A: &str = "\u{10000}";

    // UTF-8 encoding of code point 0xffff, which is a noncharacter. It is notable because it is the
    // highest code point which is represented as a single code unit in UTF-16.
    const NOT_A_CHARACTER: &str = "\u{ffff}";

    // UTF-8 encoding of code point 0xfffd, a replacement character used in place of a character
    // unrepresentable in Unicode. It is notable because it is the highest code point besides
    // noncharacters that is represented as a single code unit in UTF-16.
    const REPLACEMENT_CHARACTER: &str = "\u{fffd}";

    fn str_with_smiley() -> String {
        format!("foo {} bar\nbaz\n", SMILEY)
    }

    fn str_with_utf16_edge_cases() -> String {
        format!(
            "foo {} bar {} baz {} qux",
            LINEAR_B_SYLLABLE_B008_A, NOT_A_CHARACTER, REPLACEMENT_CHARACTER
        )
    }

    fn run_with_kind(table: &OffsetTable, position: Position, expected_offset: u32) {
        let offset = table.offset_js(position).expect("Failed to get offset");
        assert_eq!(
            expected_offset, offset,
            "Expected offset {}, got {}",
            expected_offset, offset
        );
    }

    fn run(text: &str, position: Position, js_expected: u32) {
        let table = OffsetTable::make(text);
        run_with_kind(&table, position, js_expected);
    }

    fn run_expect_failure(text: &str, position: Position, expected_msg: &str) {
        let table = OffsetTable::make(text);
        let result = table.offset_js(position);
        assert!(result.is_err(), "Expected failure but got success");
        let err = result.unwrap_err();
        let msg = err.debug_to_string();
        assert_eq!(
            expected_msg, msg,
            "Expected error message '{}', got '{}'",
            expected_msg, msg
        );
    }

    #[test]
    fn empty_line() {
        run("foo\n\nbar", pos(3, 0), 5);
    }

    #[test]
    fn loc_none() {
        // This is a fake location but it's used often enough that we should at least not crash when
        // encountering it.
        run("", pos(0, 0), 0);
    }

    #[test]
    fn first_char() {
        run("foo bar\n", pos(1, 0), 0);
    }

    #[test]
    fn last_char() {
        run("foo bar\n", pos(1, 6), 6);
    }

    #[test]
    fn column_after_last() {
        // The parser gives us locations where the `end` position is exclusive. Even though the last
        // character of the "foo" token is in column 2, the location of "foo" is given as
        // ((1, 0), (1, 3)). Because of this, we need to make sure we can look up locations that are
        // after the final column of a line, even though these locations don't correspond with an actual
        // character.
        run("foo\nbar\n", pos(1, 3), 3);
    }

    #[test]
    fn char_after_last() {
        // See the comment in the previous test
        run("foo\nbar", pos(2, 3), 7);
    }

    #[test]
    fn empty() {
        // Similar to above, we should be able to get one offset in an empty string
        run("", pos(1, 0), 0);
    }

    #[test]
    fn no_last_line_terminator() {
        run("foo bar", pos(1, 6), 6);
    }

    #[test]
    fn multi_line() {
        run("foo\nbar\n", pos(2, 1), 5);
    }

    #[test]
    fn carriage_return() {
        run("foo\rbar\r", pos(2, 1), 5);
    }

    #[test]
    fn windows_line_terminator() {
        run("foo\r\nbar\r\n", pos(2, 1), 6);
    }

    #[test]
    fn unicode_line_separator() {
        // Each line separator character is 3 bytes, or 1 JS code unit. The returned offset
        // reflects that.
        let text = format!("foo{}bar{}", LINE_SEP, LINE_SEP);
        run(&text, pos(2, 1), 5);
    }

    #[test]
    fn unicode_paragraph_separator() {
        // Each paragraph separator character is 3 bytes, or 1 JS code unit. The returned offset
        // reflects that.
        let text = format!("foo{}bar{}", PAR_SEP, PAR_SEP);
        run(&text, pos(2, 1), 5);
    }

    #[test]
    fn offset_before_multibyte_char() {
        let text = str_with_smiley();
        run(&text, pos(1, 3), 3);
    }

    #[test]
    fn offset_of_multibyte_char() {
        // This is the position of the smiley. The offset should give us the first byte in the
        // character.
        let text = str_with_smiley();
        run(&text, pos(1, 4), 4);
    }

    #[test]
    fn offset_after_multibyte_char() {
        // This is the position after the smiley. The offset should reflect the width of the multibyte
        // character (4 bytes in UTF-8 and 2 code units in JS in this case).
        let text = str_with_smiley();
        run(&text, pos(1, 5), 6);
    }

    #[test]
    fn offset_line_after_multibyte_char() {
        let text = str_with_smiley();
        run(&text, pos(2, 0), 11);
    }

    #[test]
    fn offset_of_multi_unit_utf16_char() {
        let text = str_with_utf16_edge_cases();
        run(&text, pos(1, 4), 4);
    }

    #[test]
    fn offset_after_multi_unit_utf16_char() {
        let text = str_with_utf16_edge_cases();
        run(&text, pos(1, 5), 6);
    }

    #[test]
    fn offset_of_single_unit_utf16_char() {
        let text = str_with_utf16_edge_cases();
        run(&text, pos(1, 10), 11);
    }

    #[test]
    fn offset_after_single_unit_utf16_char() {
        let text = str_with_utf16_edge_cases();
        run(&text, pos(1, 11), 12);
    }

    #[test]
    fn offset_of_single_unit_utf16_char2() {
        let text = str_with_utf16_edge_cases();
        run(&text, pos(1, 16), 17);
    }

    #[test]
    fn offset_after_single_unit_utf16_char2() {
        let text = str_with_utf16_edge_cases();
        run(&text, pos(1, 17), 18);
    }

    #[test]
    fn out_of_bounds_line() {
        run_expect_failure(
            "foo\n",
            pos(5, 0),
            "Failure while looking up line at Position { line: 5, column: 0 }. Index: 4. Length: 2.",
        );
    }

    #[test]
    fn out_of_bounds_column() {
        run_expect_failure(
            "foo\n",
            pos(1, 11),
            "Failure while looking up column at Position { line: 1, column: 11 }. Index: 11. Length: 5.",
        );
    }

    // TODO: run_full_test: depend on Flow_polymorphic_ast_mapper
}
