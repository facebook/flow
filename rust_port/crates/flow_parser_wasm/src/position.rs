/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Position computation with UTF-16 awareness.
//!
//! Converts the Flow parser's byte-based positions (line, byte_column) to
//! UTF-16 positions (line, utf16_column, utf16_offset) for the binary protocol's
//! position buffer.

/// Collected during serialization: a source position to resolve later.
#[derive(Debug, Clone)]
pub struct PositionInfo {
    pub loc_id: u32,
    /// 0 = start, 1 = end
    pub kind: u32,
    /// 1-indexed line number
    pub line: i32,
    /// 0-indexed byte-column
    pub byte_column: i32,
}

impl PositionInfo {
    pub fn start(loc_id: u32, line: i32, column: i32) -> Self {
        PositionInfo {
            loc_id,
            kind: 0,
            line,
            byte_column: column,
        }
    }

    pub fn end(loc_id: u32, line: i32, column: i32) -> Self {
        PositionInfo {
            loc_id,
            kind: 1,
            line,
            byte_column: column,
        }
    }
}

/// The final position result written to the position buffer.
/// Each entry is 5 x u32 = 20 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct PositionResult {
    pub loc_id: u32,
    pub kind: u32,
    pub line: u32,
    pub column: u32,
    pub offset: u32,
}

/// Compute line/column/offset for all collected positions.
///
/// The Flow parser provides line (1-indexed) and byte-column (0-indexed).
/// We need to convert to:
///   - line: 1-indexed (same)
///   - column: 0-indexed in UTF-16 code units
///   - offset: 0-indexed total UTF-16 code units from start of source
///
/// Algorithm:
/// 1. Build a line-start byte offset table from the source text
/// 2. For each position, compute byte_offset = line_start[line-1] + byte_column
/// 3. Sort positions by byte_offset
/// 4. Single-pass through source text computing utf16 offset and column
pub fn compute_positions(source: &[u8], positions: &mut [PositionInfo]) -> Vec<PositionResult> {
    if positions.is_empty() {
        return Vec::new();
    }

    // Step 1: Build line-start byte offset table and parallel UTF-16 offset table.
    // line_starts[i] = byte offset at the beginning of line (i+1) (0-indexed into array).
    // line_utf16_starts[i] = UTF-16 offset at the beginning of line (i+1).
    let mut line_starts: Vec<usize> = vec![0];
    let mut line_utf16_starts: Vec<u32> = vec![0];
    let mut i = 0;
    let mut utf16_idx: u32 = 0;
    while i < source.len() {
        let b = source[i];
        if b == b'\n' {
            utf16_idx += 1;
            line_starts.push(i + 1);
            line_utf16_starts.push(utf16_idx);
            i += 1;
        } else if b == b'\r' {
            if i + 1 < source.len() && source[i + 1] == b'\n' {
                utf16_idx += 2;
                line_starts.push(i + 2);
                line_utf16_starts.push(utf16_idx);
                i += 2;
            } else {
                utf16_idx += 1;
                line_starts.push(i + 1);
                line_utf16_starts.push(utf16_idx);
                i += 1;
            }
        } else if b == 0xe2 && i + 2 < source.len() && source[i + 1] == 0x80 {
            // U+2028 LINE SEPARATOR = 0xe2 0x80 0xa8
            // U+2029 PARAGRAPH SEPARATOR = 0xe2 0x80 0xa9
            if source[i + 2] == 0xa8 || source[i + 2] == 0xa9 {
                utf16_idx += 1;
                line_starts.push(i + 3);
                line_utf16_starts.push(utf16_idx);
                i += 3;
            } else {
                utf16_idx += 1;
                i += 3;
            }
        } else if b < 0x80 {
            utf16_idx += 1;
            i += 1;
        } else if b & 0xe0 == 0xc0 {
            utf16_idx += 1;
            i += 2;
        } else if b & 0xf0 == 0xe0 {
            utf16_idx += 1;
            i += 3;
        } else if b & 0xf8 == 0xf0 {
            // 4-byte UTF-8 → 2 UTF-16 code units (surrogate pair)
            utf16_idx += 2;
            i += 4;
        } else {
            // Malformed byte: treat as 1 byte, 1 UTF-16 unit
            utf16_idx += 1;
            i += 1;
        }
    }
    // Add a phantom entry for "past end of file"
    line_starts.push(source.len());
    line_utf16_starts.push(utf16_idx);

    // Step 2: Compute byte_offset for each position and sort.
    // We store (byte_offset, index_into_positions) for sorting.
    let mut sorted_indices: Vec<(usize, usize)> = positions
        .iter()
        .enumerate()
        .map(|(idx, pos)| {
            let byte_offset = if pos.line <= 0 || pos.byte_column < 0 {
                // Loc.none case
                0
            } else {
                let line_idx = (pos.line - 1) as usize;
                let start = if line_idx < line_starts.len() {
                    line_starts[line_idx]
                } else {
                    // past end of file
                    source.len()
                };
                start + pos.byte_column as usize
            };
            (byte_offset, idx)
        })
        .collect();

    sorted_indices.sort_by_key(|&(byte_offset, _)| byte_offset);

    // Step 3: Single-pass through source text computing UTF-16 offsets.
    // The `line` and `column` fields of the result preserve the input
    // `pos.line` and the UTF-16 column relative to that input line. This
    // matters when a position points at a byte offset immediately after a
    // line terminator: the OCaml lexer represents the regex/error loc end
    // there as `(prev_line, prev_line_length+1)`, and we must mirror that
    // exact representation rather than the post-newline `(next_line, 0)`.
    let mut results = vec![
        PositionResult {
            loc_id: 0,
            kind: 0,
            line: 0,
            column: 0,
            offset: 0,
        };
        positions.len()
    ];

    let mut utf16_offset: u32 = 0;
    let mut byte_idx: usize = 0;
    let mut sorted_pos = 0;

    while sorted_pos < sorted_indices.len() {
        let (target_byte, _orig_idx) = sorted_indices[sorted_pos];

        // Advance through source until we reach the target byte offset
        while byte_idx < target_byte && byte_idx < source.len() {
            let b = source[byte_idx];
            if b == b'\n' {
                utf16_offset += 1;
                byte_idx += 1;
            } else if b == b'\r' {
                if byte_idx + 1 < source.len() && source[byte_idx + 1] == b'\n' {
                    // \r\n treated as one line terminator
                    utf16_offset += 2;
                    byte_idx += 2;
                } else {
                    utf16_offset += 1;
                    byte_idx += 1;
                }
            } else if b < 0x80 {
                // ASCII
                utf16_offset += 1;
                byte_idx += 1;
            } else if b & 0xe0 == 0xc0 {
                // 2-byte UTF-8
                utf16_offset += 1;
                byte_idx += 2;
            } else if b & 0xf0 == 0xe0 {
                // 3-byte UTF-8 (including U+2028 / U+2029 line separators)
                utf16_offset += 1;
                byte_idx += 3;
            } else if b & 0xf8 == 0xf0 {
                // 4-byte UTF-8 → 2 UTF-16 code units (surrogate pair)
                utf16_offset += 2;
                byte_idx += 4;
            } else {
                // Malformed byte: treat as 1 byte, 1 UTF-16 unit
                utf16_offset += 1;
                byte_idx += 1;
            }
        }

        // Emit all positions that map to this byte offset.
        // Trust the input `pos.line`; compute the UTF-16 column relative
        // to that input line's UTF-16 start offset.
        while sorted_pos < sorted_indices.len() && sorted_indices[sorted_pos].0 == target_byte {
            let (_, orig_idx) = sorted_indices[sorted_pos];
            let pos = &positions[orig_idx];
            let (out_line, out_col) = if pos.line <= 0 || pos.byte_column < 0 {
                // Loc.none case
                (1, 0)
            } else {
                let line_idx = (pos.line - 1) as usize;
                let line_utf16_start = if line_idx < line_utf16_starts.len() {
                    line_utf16_starts[line_idx]
                } else {
                    utf16_idx
                };
                (pos.line as u32, utf16_offset - line_utf16_start)
            };
            results[orig_idx] = PositionResult {
                loc_id: pos.loc_id,
                kind: pos.kind,
                line: out_line,
                column: out_col,
                offset: utf16_offset,
            };
            sorted_pos += 1;
        }
    }

    results
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ascii_positions() {
        let source = b"const x = 42;";
        let mut positions = vec![
            PositionInfo::start(0, 1, 0), // 'c' at byte 0
            PositionInfo::end(0, 1, 13),  // after ';' at byte 13
        ];
        let results = compute_positions(source, &mut positions);
        assert_eq!(results[0].line, 1);
        assert_eq!(results[0].column, 0);
        assert_eq!(results[0].offset, 0);
        assert_eq!(results[1].line, 1);
        assert_eq!(results[1].column, 13);
        assert_eq!(results[1].offset, 13);
    }

    #[test]
    fn test_multiline_positions() {
        let source = b"foo\nbar\n";
        let mut positions = vec![
            PositionInfo::start(0, 1, 0),
            PositionInfo::end(0, 2, 3),
            PositionInfo::start(1, 2, 0),
        ];
        let results = compute_positions(source, &mut positions);
        assert_eq!(results[0].line, 1);
        assert_eq!(results[0].column, 0);
        assert_eq!(results[0].offset, 0);
        assert_eq!(results[1].line, 2);
        assert_eq!(results[1].column, 3);
        assert_eq!(results[1].offset, 7);
        assert_eq!(results[2].line, 2);
        assert_eq!(results[2].column, 0);
        assert_eq!(results[2].offset, 4);
    }

    #[test]
    fn test_emoji_utf16_positions() {
        // "a😀b" - emoji is 4 bytes UTF-8, 2 UTF-16 code units
        let source = "a\u{1F600}b".as_bytes();
        let mut positions = vec![
            PositionInfo::start(0, 1, 0), // 'a' at byte 0
            PositionInfo::start(1, 1, 1), // emoji at byte 1
            PositionInfo::start(2, 1, 5), // 'b' at byte 5
        ];
        let results = compute_positions(source, &mut positions);
        assert_eq!(results[0].column, 0);
        assert_eq!(results[0].offset, 0);
        assert_eq!(results[1].column, 1);
        assert_eq!(results[1].offset, 1);
        assert_eq!(results[2].column, 3); // 1(a) + 2(emoji) = 3
        assert_eq!(results[2].offset, 3); // 1(a) + 2(emoji) = 3
    }

    #[test]
    fn test_loc_none() {
        let source = b"hello";
        let mut positions = vec![PositionInfo::start(0, 0, 0)]; // Loc.none
        let results = compute_positions(source, &mut positions);
        assert_eq!(results[0].line, 1);
        assert_eq!(results[0].column, 0);
        assert_eq!(results[0].offset, 0);
    }
}
