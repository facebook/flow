/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::loc::Loc;
use crate::loc::Position;

pub struct SourceLocationTable<'a> {
    source: &'a str,
    line_starts: Vec<usize>,
}

impl<'a> SourceLocationTable<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut line_starts = Vec::with_capacity(source.len() / 40 + 1);
        line_starts.push(0);
        for (offset, byte) in source.bytes().enumerate() {
            if byte == b'\n' {
                line_starts.push(offset + 1);
            }
        }
        Self {
            source,
            line_starts,
        }
    }

    pub fn position_at_byte_offset(&self, offset: usize) -> Position {
        let offset = offset.min(self.source.len());
        let line_index = self.line_starts.partition_point(|start| *start <= offset) - 1;
        Position {
            line: line_index as i32 + 1,
            column: (offset - self.line_starts[line_index]) as i32,
        }
    }

    pub fn byte_offset_at_position(&self, position: Position) -> Option<usize> {
        if position.line < 1 || position.column < 0 {
            return None;
        }
        let line_start = *self.line_starts.get(position.line as usize - 1)?;
        Some((line_start + position.column as usize).min(self.source.len()))
    }

    pub fn token_start_offset_at_loc(&self, loc: &Loc) -> Option<usize> {
        self.byte_offset_at_position(loc.start)
            .map(|offset| self.skip_trivia(offset))
    }

    pub fn loc_starts_with_keyword(&self, loc: &Loc, keyword: &str) -> bool {
        let Some(start) = self.token_start_offset_at_loc(loc) else {
            return false;
        };
        let bytes = self.source.as_bytes();
        let keyword = keyword.as_bytes();
        let end = start + keyword.len();
        end <= bytes.len()
            && &bytes[start..end] == keyword
            && (end == bytes.len() || !is_identifier_continue(bytes[end]))
    }

    pub fn next_token_loc_after(&self, loc: &Loc) -> Loc {
        let Some(offset) = self.byte_offset_at_position(loc.end) else {
            return loc.dupe();
        };
        let start = self.skip_trivia(offset);
        let bytes = self.source.as_bytes();
        let end = if start + 3 <= bytes.len() && &bytes[start..start + 3] == b"..." {
            start + 3
        } else if start + 2 <= bytes.len() && &bytes[start..start + 2] == b"=>" {
            start + 2
        } else if start < bytes.len() && is_identifier_start(bytes[start]) {
            let mut end = start + 1;
            while end < bytes.len() && is_identifier_continue(bytes[end]) {
                end += 1;
            }
            end
        } else if start < bytes.len() {
            start
                + self.source[start..]
                    .chars()
                    .next()
                    .map_or(1, char::len_utf8)
        } else {
            start
        };
        Loc {
            source: loc.source.dupe(),
            start: self.position_at_byte_offset(start),
            end: self.position_at_byte_offset(end),
        }
    }

    fn skip_trivia(&self, mut offset: usize) -> usize {
        let bytes = self.source.as_bytes();
        while offset < bytes.len() {
            match bytes[offset] {
                b' ' | b'\t' | b'\n' | b'\r' | 0x0b | 0x0c => offset += 1,
                b'/' if offset + 1 < bytes.len() && bytes[offset + 1] == b'/' => {
                    offset += 2;
                    while offset < bytes.len() && bytes[offset] != b'\n' && bytes[offset] != b'\r' {
                        offset += 1;
                    }
                }
                b'/' if offset + 1 < bytes.len() && bytes[offset + 1] == b'*' => {
                    offset += 2;
                    while offset + 1 < bytes.len()
                        && !(bytes[offset] == b'*' && bytes[offset + 1] == b'/')
                    {
                        offset += 1;
                    }
                    offset = (offset + 2).min(bytes.len());
                }
                _ => break,
            }
        }
        offset
    }
}

fn is_identifier_start(byte: u8) -> bool {
    byte == b'$' || byte == b'_' || byte.is_ascii_alphabetic()
}

fn is_identifier_continue(byte: u8) -> bool {
    is_identifier_start(byte) || byte.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn position(line: i32, column: i32) -> Position {
        Position { line, column }
    }

    fn loc(start: Position, end: Position) -> Loc {
        Loc {
            source: None,
            start,
            end,
        }
    }

    #[test]
    fn converts_between_byte_offsets_and_positions() {
        let table = SourceLocationTable::new("one\ntwo\nthree");
        assert_eq!(table.position_at_byte_offset(5), position(2, 1));
        assert_eq!(table.byte_offset_at_position(position(3, 2)), Some(10));
    }

    #[test]
    fn finds_the_next_token_after_trivia() {
        let table = SourceLocationTable::new("x /* block */ // line\n  ...rest");
        assert_eq!(
            table.next_token_loc_after(&loc(position(1, 0), position(1, 1))),
            loc(position(2, 2), position(2, 5)),
        );
    }

    #[test]
    fn preserves_byte_columns_for_multibyte_characters() {
        let table = SourceLocationTable::new("x\n  😃");
        assert_eq!(
            table.next_token_loc_after(&loc(position(1, 0), position(1, 1))),
            loc(position(2, 2), position(2, 6)),
        );
    }
}
