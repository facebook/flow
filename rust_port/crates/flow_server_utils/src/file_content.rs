/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Range {
    pub st: Position,
    pub ed: Position,
}

#[derive(Debug, Clone)]
pub struct TextEdit {
    pub range: Option<Range>,
    pub text: String,
}

fn get_char_length(c: u8) -> usize {
    if c >> 7 == 0b0 {
        1
    } else if c >> 5 == 0b110 {
        2
    } else if c >> 4 == 0b1110 {
        3
    } else if c >> 3 == 0b11110 {
        4
    } else {
        panic!("Invalid UTF-8 leading byte: {}", c)
    }
}

fn is_target(t: &Position, line: usize, column: usize) -> bool {
    t.line == line && t.column == column
}

pub fn get_char(content: &[u8], offset: usize) -> u8 {
    if offset == content.len() {
        b'\n'
    } else {
        content[offset]
    }
}

fn get_offsets_rec(
    content: &[u8],
    queries: (&Position, &Position),
    mut line: usize,
    mut column: usize,
    mut offset: usize,
    mut acc: (Option<usize>, Option<usize>),
) -> (Option<usize>, Option<usize>) {
    loop {
        if acc.0.is_some() && acc.1.is_some() {
            return acc;
        }
        if acc.0.is_none() && is_target(queries.0, line, column) {
            acc.0 = Some(offset);
            continue;
        }
        if acc.0.is_some() && acc.1.is_none() && is_target(queries.1, line, column) {
            acc.1 = Some(offset);
            continue;
        }
        let c = get_char(content, offset);
        if c == b'\n' {
            line += 1;
            column = 1;
            offset += 1;
        } else {
            column += 1;
            offset += get_char_length(c);
        }
    }
}

fn invalid_position(p: &Position) -> ! {
    panic!(
        "Invalid position: {{line: {}; column: {}}}",
        p.line, p.column
    )
}

pub fn get_offsets(content: &str, queries: (&Position, &Position)) -> (usize, usize) {
    match get_offsets_rec(content.as_bytes(), queries, 1, 1, 0, (None, None)) {
        (Some(r1), Some(r2)) => (r1, r2),
        (None, _) => invalid_position(queries.0),
        (_, None) => invalid_position(queries.1),
    }
}

pub fn get_offset(content: &str, position: &Position) -> usize {
    get_offsets(content, (position, position)).0
}

pub fn offset_to_position(content: &str, offset: usize) -> Position {
    if offset > content.len() {
        panic!("Invalid offset: {}", offset);
    }
    let bytes = content.as_bytes();
    let mut line: usize = 1;
    let mut column: usize = 1;
    let mut index: usize = 0;
    loop {
        if index >= offset {
            return Position { line, column };
        }
        let c = get_char(bytes, index);
        let clen = get_char_length(c);
        if c == b'\n' {
            line += 1;
            column = 1;
            index += clen;
        } else {
            column += 1;
            index += clen;
        }
    }
}

fn apply_edit(content: &str, edit: &TextEdit) -> String {
    match &edit.range {
        None => edit.text.clone(),
        Some(Range { st, ed }) => {
            let (start_offset, end_offset) = get_offsets(content, (st, ed));
            let prefix = &content[..start_offset];
            let suffix = &content[end_offset..];
            format!("{}{}{}", prefix, edit.text, suffix)
        }
    }
}

pub fn edit_file(content: &str, edits: &[TextEdit]) -> Result<String, String> {
    let result = std::panic::catch_unwind(|| {
        let mut current = content.to_string();
        for edit in edits {
            current = apply_edit(&current, edit);
        }
        current
    });
    match result {
        Ok(s) => Ok(s),
        Err(e) => {
            let msg = if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = e.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "Unknown error".to_string()
            };
            Err(format!(
                "Invalid edit: {}\nOriginal content:\n{}",
                msg, content
            ))
        }
    }
}

pub fn edit_file_unsafe(content: &str, edits: &[TextEdit]) -> String {
    match edit_file(content, edits) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("{}", e);
            panic!("{}", e);
        }
    }
}
