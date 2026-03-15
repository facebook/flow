/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This module defines a compact encoding for sorted locations with the same
// source file.
//
// As a first optimization, we don't store the source file at all. Instead, we
// assume that the source information is stored elsewhere and can be provided
// when unpacking.
//
// Line and column numbers use a variable-length code, similar to unsigned
// LEB128. Numbers less than 0xFF can be stored in a single byte, e.g.
//
// Positions are stored relative to the previous location's start position. This
// helps keep the line/column numbers small, making them more amenable to the
// variable-length code, hopefully each fitting into a single byte.
//
// Finally, we distinguish between some common cases to avoid storing all four
// of start line, start column, end line, and end column. The first byte of each
// location is a tag:
//
// 0x00 - 0x3F + 1 int: single line, start_rline = 0, start_rcolumn < 0x40
//  { .tag = start_rcolumn;
//    .0 = length }
//
// 0x40 - 0x7E + 2 ints: single line, start_rline < 0x3F
//  { .tag = start_rline + 0x40;
//    .0 = if start_rline = 0 then start_rcolumn - 0x40 else start_column;
//    .1 = length }
//
// 0x7F + 3 ints: single line, rline >= 0x3F
//  { .tag = 0x7F;
//    .0 = start_rline - 0x3F;
//    .1 = start_column;
//    .2 = length }
//
// 0x80 - 0xBF + 2 ints: multi line, rline = 0, rcolumn < 0x40
//  { .tag = start_rcolumn + 0x80;
//    .0 = end_rline;
//    .1 = end_column }
//
// 0xC0 - 0xFE + 3 ints: multi line, rline < 0x3F
//  { .tag = start_rline + 0xC0;
//    .0 = if start_rline = 0 then start_rcolumn - 0x40 else start_column;
//    .1 = end_rline;
//    .2 = end_column }
//
// 0xFF + 4 ints; multi line, rline >= 0x3F
//  { .tag = 0xFF;
//    .0 = start_rline - 0x3F;
//    .1 = start_column;
//    .2 = end_rline;
//    .3 = end_column }

use flow_common_leb128 as leb128;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;

pub fn compare_locs(loc0: &Loc, loc1: &Loc) -> std::cmp::Ordering {
    let k = loc0.start.cmp(&loc1.start);
    if k == std::cmp::Ordering::Equal {
        loc1.end.cmp(&loc0.end)
    } else {
        k
    }
}

fn add_int(buf: &mut Vec<u8>, i: usize) {
    leb128::unsigned::write(&mut |byte| buf.push(byte), i);
}

fn write_loc_start(buf: &mut Vec<u8>, tag_adjust: u8, prev_column: i32, rline: i32, column: i32) {
    if rline == 0 {
        let rcolumn = column - prev_column;
        if rcolumn < 0x40 {
            buf.push(tag_adjust + rcolumn as u8);
        } else {
            buf.push(tag_adjust + 0x40);
            add_int(buf, (rcolumn - 0x40) as usize);
        }
    } else if rline < 0x3F {
        buf.push(tag_adjust + 0x40 + rline as u8);
        add_int(buf, column as usize);
    } else {
        buf.push(tag_adjust + 0x7F);
        add_int(buf, (rline - 0x3F) as usize);
        add_int(buf, column as usize);
    }
}

pub fn pack<'a, I>(len: usize, locs: I) -> Vec<u8>
where
    I: IntoIterator<Item = &'a Loc>,
{
    let mut buf = Vec::new();
    let mut prev_line = 0;
    let mut prev_column = 0;

    add_int(&mut buf, len);

    for loc in locs {
        let start_rline = loc.start.line - prev_line;
        let end_rline = loc.end.line - loc.start.line;

        if end_rline == 0 {
            // single line
            write_loc_start(&mut buf, 0, prev_column, start_rline, loc.start.column);
            add_int(&mut buf, (loc.end.column - loc.start.column) as usize);
        } else {
            // multiline
            write_loc_start(&mut buf, 0x80, prev_column, start_rline, loc.start.column);
            add_int(&mut buf, end_rline as usize);
            add_int(&mut buf, loc.end.column as usize);
        }

        prev_line = loc.start.line;
        prev_column = loc.start.column;
    }

    buf
}

fn mk_loc(
    source: Option<FileKey>,
    start_line: i32,
    start_column: i32,
    end_line: i32,
    end_column: i32,
) -> Loc {
    Loc {
        source,
        start: flow_parser::loc::Position {
            line: start_line,
            column: start_column,
        },
        end: flow_parser::loc::Position {
            line: end_line,
            column: end_column,
        },
    }
}

struct UnpackState {
    prev_line: i32,
    prev_column: i32,
    pos: usize,
}

impl UnpackState {
    fn read_i8(&mut self, packed: &[u8]) -> u8 {
        let c = packed[self.pos];
        self.pos += 1;
        c
    }

    fn read_int(&mut self, packed: &[u8]) -> usize {
        leb128::unsigned::read(&mut || self.read_i8(packed))
    }

    fn read_loc(&mut self, source: &Option<FileKey>, packed: &[u8]) -> Loc {
        let tag = self.read_i8(packed);
        if tag < 0x40 {
            let length = self.read_int(packed) as i32;
            let start_line = self.prev_line;
            let start_column = self.prev_column + tag as i32;
            mk_loc(
                source.clone(),
                start_line,
                start_column,
                start_line,
                start_column + length,
            )
        } else if tag < 0x7F {
            let start_rcolumn = self.read_int(packed) as i32;
            let length = self.read_int(packed) as i32;
            let start_rline = (tag - 0x40) as i32;
            let start_line = self.prev_line + start_rline;
            let start_column = if start_rline == 0 {
                self.prev_column + start_rcolumn + 0x40
            } else {
                start_rcolumn
            };
            mk_loc(
                source.clone(),
                start_line,
                start_column,
                start_line,
                start_column + length,
            )
        } else if tag == 0x7F {
            let start_rline = self.read_int(packed) as i32 + 0x3F;
            let start_column = self.read_int(packed) as i32;
            let length = self.read_int(packed) as i32;
            let start_line = self.prev_line + start_rline;
            mk_loc(
                source.clone(),
                start_line,
                start_column,
                start_line,
                start_column + length,
            )
        } else if tag < 0xC0 {
            let end_rline = self.read_int(packed) as i32;
            let end_column = self.read_int(packed) as i32;
            let start_line = self.prev_line;
            let start_column = self.prev_column + (tag - 0x80) as i32;
            mk_loc(
                source.clone(),
                start_line,
                start_column,
                start_line + end_rline,
                end_column,
            )
        } else if tag < 0xFF {
            let start_rcolumn = self.read_int(packed) as i32;
            let end_rline = self.read_int(packed) as i32;
            let end_column = self.read_int(packed) as i32;
            let start_rline = (tag - 0xC0) as i32;
            let start_line = self.prev_line + start_rline;
            let start_column = if start_rline == 0 {
                self.prev_column + start_rcolumn + 0x40
            } else {
                start_rcolumn
            };
            mk_loc(
                source.clone(),
                start_line,
                start_column,
                start_line + end_rline,
                end_column,
            )
        } else {
            let start_rline = self.read_int(packed) as i32 + 0x3F;
            let start_column = self.read_int(packed) as i32;
            let end_rline = self.read_int(packed) as i32;
            let end_column = self.read_int(packed) as i32;
            let start_line = self.prev_line + start_rline;
            mk_loc(
                source.clone(),
                start_line,
                start_column,
                start_line + end_rline,
                end_column,
            )
        }
    }
}

pub fn unpack<F, R>(source: Option<FileKey>, init: F, packed: &[u8]) -> R
where
    F: FnOnce(usize, &mut dyn FnMut(usize) -> Loc) -> R,
{
    let mut state = UnpackState {
        prev_line: 0,
        prev_column: 0,
        pos: 0,
    };

    let len = state.read_int(packed);
    init(len, &mut |_| {
        let loc = state.read_loc(&source, packed);
        state.prev_line = loc.start.line;
        state.prev_column = loc.start.column;
        loc
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mk_loc_test(start_line: i32, start_column: i32, end_line: i32, end_column: i32) -> Loc {
        mk_loc(None, start_line, start_column, end_line, end_column)
    }

    fn hexdump(bytes: &[u8]) -> String {
        bytes
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn locdump(loc: &Loc) -> String {
        format!(
            "{}:{}-{}:{}",
            loc.start.line, loc.start.column, loc.end.line, loc.end.column
        )
    }

    fn dump_and_verify(
        locs: Vec<(i32, i32, i32, i32)>,
        expected_hex: &str,
        expected_locs: &[&str],
    ) {
        let locs: Vec<Loc> = locs
            .into_iter()
            .map(|(sl, sc, el, ec)| mk_loc_test(sl, sc, el, ec))
            .collect();
        let len = locs.len();
        let packed = pack(len, locs.iter());

        let unpacked: Vec<Loc> = unpack(None, |len, f| (0..len).map(f).collect(), &packed);

        let hex = hexdump(&packed);
        assert_eq!(hex, expected_hex);

        assert_eq!(unpacked.len(), expected_locs.len());
        for (i, expected) in expected_locs.iter().enumerate() {
            assert_eq!(locdump(&unpacked[i]), *expected);
        }
    }

    #[test]
    fn single_same_line_near_column() {
        dump_and_verify(vec![(0, 63, 0, 64)], "01 3f 01", &["0:63-0:64"]);
    }

    #[test]
    fn single_same_line_far_column() {
        dump_and_verify(vec![(0, 64, 0, 65)], "01 40 00 01", &["0:64-0:65"]);
    }

    #[test]
    fn single_near_line() {
        dump_and_verify(vec![(62, 1, 62, 3)], "01 7e 01 02", &["62:1-62:3"]);
    }

    #[test]
    fn single_far_line() {
        dump_and_verify(vec![(63, 1, 63, 3)], "01 7f 00 01 02", &["63:1-63:3"]);
    }

    #[test]
    fn multi_same_line_near_column() {
        dump_and_verify(vec![(0, 63, 1, 2)], "01 bf 01 02", &["0:63-1:2"]);
    }

    #[test]
    fn multi_same_line_far_column() {
        dump_and_verify(vec![(0, 64, 1, 2)], "01 c0 00 01 02", &["0:64-1:2"]);
    }

    #[test]
    fn multi_near_line() {
        dump_and_verify(vec![(62, 1, 64, 3)], "01 fe 01 02 03", &["62:1-64:3"]);
    }

    #[test]
    fn multi_far_line() {
        dump_and_verify(vec![(63, 1, 64, 3)], "01 ff 00 01 01 03", &["63:1-64:3"]);
    }
}
