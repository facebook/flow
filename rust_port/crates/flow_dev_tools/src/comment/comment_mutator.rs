/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io;
use std::sync::LazyLock;

use regex::Regex;
use regex::RegexBuilder;
use serde_json::Value;

use crate::comment::get_context::Context;
use crate::comment::get_path_to_loc::get_node_at_range;
use crate::comment::get_path_to_loc::node_type;
use crate::flow_result::FlowLoc;
use crate::flow_result::required_offset;

static FLOWLINT_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    RegexBuilder::new(r"^[ \t\n\r*]*flowlint(-line|-next-line)?\b")
        .unicode(false)
        .build()
        .expect("flowlint suppression regex should compile")
});

static EMPTY_FLOWLINT_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    RegexBuilder::new(r"^[ \t\n\r*]*flowlint(-line|-next-line)?[ \t\n\r*]*$")
        .unicode(false)
        .build()
        .expect("empty flowlint regex should compile")
});

static ESLINT_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    RegexBuilder::new(r"eslint-disable")
        .build()
        .expect("eslint suppression regex should compile")
});

#[derive(Clone, Debug)]
pub(crate) struct CommentAst {
    pub(crate) value: String,
    pub(crate) range: (usize, usize),
}

fn buffer_char_at(contents: &[u8], pos: usize) -> Option<u8> {
    contents.get(pos).copied()
}

pub(crate) fn is_lint_suppression(comment_ast: &CommentAst) -> bool {
    FLOWLINT_REGEX.is_match(&comment_ast.value)
}

pub(crate) fn is_eslint_suppression(value: &str) -> bool {
    ESLINT_REGEX.is_match(value)
}

fn is_newline(byte: u8) -> bool {
    byte == b'\n' || byte == b'\r'
}

fn is_edible(byte: u8) -> bool {
    byte == b'\t' || byte == b' '
}

fn expand_comment(
    contents: &[u8],
    mut start_offset: usize,
    mut end_offset: usize,
    comment_ast: Option<&mut CommentAst>,
    ast: &Value,
) -> (usize, usize) {
    let length = contents.len();
    if let Some(comment_ast) = comment_ast
        && is_lint_suppression(comment_ast)
    {
        // We're operating on a flowlint comment

        // All comments start with 2 chars
        let (comment_start_offset, comment_end_offset) = comment_ast.range;
        let comment_value_offset = comment_start_offset + 2;
        if start_offset
            .checked_sub(comment_value_offset + 1)
            .and_then(|preceding_index| comment_ast.value.as_bytes().get(preceding_index))
            .is_some_and(|byte| matches!(byte, b' ' | b'\t' | b'\n' | b'\r'))
        {
            start_offset = start_offset.saturating_sub(1);
        }

        let rel_start = start_offset.saturating_sub(comment_value_offset);
        let rel_end = end_offset.saturating_sub(comment_value_offset);
        let mut new_comment_value = Vec::new();
        if rel_start > comment_ast.value.len() || rel_end > comment_ast.value.len() {
            return (start_offset, end_offset);
        }
        new_comment_value.extend(&comment_ast.value.as_bytes()[..rel_start]);
        new_comment_value.extend(&comment_ast.value.as_bytes()[rel_end..]);
        let new_comment_value = String::from_utf8_lossy(&new_comment_value).to_string();
        if EMPTY_FLOWLINT_REGEX.is_match(&new_comment_value) {
            start_offset = comment_start_offset;
            end_offset = comment_end_offset;
        } else {
            comment_ast.range.1 -= end_offset - start_offset;
            comment_ast.value = new_comment_value;
            return (start_offset, end_offset);
        }
    }

    let mut orig_before_start = start_offset as isize - 1;
    let mut orig_after_end = end_offset;
    let mut before_start = orig_before_start;
    let mut after_end = orig_after_end;

    while before_start >= 0
        && buffer_char_at(contents, before_start as usize).is_some_and(is_edible)
    {
        before_start -= 1;
    }
    while after_end < length && buffer_char_at(contents, after_end).is_some_and(is_edible) {
        after_end += 1;
    }

    if before_start >= 0
        && after_end < length
        && buffer_char_at(contents, before_start as usize) == Some(b'{')
        && buffer_char_at(contents, after_end) == Some(b'}')
        && get_node_at_range((before_start as usize, after_end + 1), ast).and_then(node_type)
            == Some("JSXExpressionContainer")
    {
        before_start -= 1;
        after_end += 1;
        orig_before_start = before_start;
        orig_after_end = after_end;
        while before_start >= 0
            && buffer_char_at(contents, before_start as usize).is_some_and(is_edible)
        {
            before_start -= 1;
        }
        while after_end < length && buffer_char_at(contents, after_end).is_some_and(is_edible) {
            after_end += 1;
        }
    }

    if before_start < 0 || buffer_char_at(contents, before_start as usize) == Some(b'\n') {
        if after_end > length || buffer_char_at(contents, after_end) == Some(b'\n') {
            if after_end < length {
                after_end += 1;
            } else if before_start >= 0 {
                before_start -= 1;
            }
        } else {
            // There's something after the comment. We shouldn't remove
            // preceding whitespace thanks to indentation
            before_start = orig_before_start;
        }
    } else if after_end >= length || buffer_char_at(contents, after_end) == Some(b'\n') {
        // There's something preceding the comment but nothing afterwards. We can
        // just remove the rest of the line
    } else {
        before_start = orig_before_start;
        after_end = orig_after_end;
    }

    ((before_start + 1).max(0) as usize, after_end)
}

pub(crate) fn find_start_of_line(contents: &[u8], start_offset: usize) -> usize {
    // if startOffset is already a newline, that's not the start of the line,
    // it's the end of the line. so start from the character before.
    let mut start = start_offset as isize - 1;
    while start >= 0
        && buffer_char_at(contents, start as usize).is_some_and(|byte| !is_newline(byte))
    {
        start -= 1;
    }
    (start + 1) as usize
}

fn find_end_of_line(contents: &[u8], start_offset: usize) -> usize {
    let mut start = start_offset;
    while start < contents.len()
        && buffer_char_at(contents, start).is_some_and(|byte| !is_newline(byte))
    {
        start += 1;
    }
    start
}

fn insert_comment_to_text(mut contents: Vec<u8>, start_offset: usize, comment: &str) -> Vec<u8> {
    contents.splice(start_offset..start_offset, comment.bytes());
    contents
}

pub(crate) fn add_comment_to_text(
    contents: Vec<u8>,
    loc: &FlowLoc,
    (inside, ast): (Context, &Value),
    comments: &[String],
    start_of_line: Option<usize>,
) -> io::Result<Vec<u8>> {
    let loc_start_offset = required_offset(&loc.start, "comment insertion start")?;
    let (start_offset, start) = match start_of_line {
        Some(start_of_line) => (start_of_line, start_of_line),
        None => (
            loc_start_offset,
            find_start_of_line(&contents, loc_start_offset),
        ),
    };
    let end_of_line = find_end_of_line(&contents, start_offset);
    let line = String::from_utf8_lossy(&contents[start..end_of_line]).to_string();
    let in_jsx = inside == Context::JsxFragment || inside == Context::Jsx;

    if inside == Context::Normal {
        let comment = format!("{}\n", format_comment(comments, &line, false).join("\n"));
        return Ok(insert_comment_to_text(contents, start, &comment));
    }

    if in_jsx && node_type(ast) == Some("JSXElement") {
        let comment = format!("{}\n", format_comment(comments, &line, true).join("\n"));
        return Ok(insert_comment_to_text(contents, start, &comment));
    }

    if inside == Context::Template || (in_jsx && node_type(ast) == Some("JSXExpressionContainer")) {
        /* Ok, so we have something like
         *
         * <jsx>
         *   {10 * 'hello'}
         * <jsx>
         *
         * We need to stick the comment inside the expression container.
         * So the above example turns into
         *
         * <jsx>
         *   {
         *     // Comment
         *     10 * 'hello'}
         * <jsx>
         *
         * Same thing if we have something like
         *
         * var str = `hello
         *   ${10 * 'hello'}
         * `;
         *
         * We need to stick the comment inside of the template element. So the
         * above example turns into
         *
         * var str = `hello
         *   ${
         *     // Comment
         *     10 * 'hello'}
         * `;
         */
        let ast_start_column = ast
            .get("loc")
            .and_then(|loc| loc.get("start"))
            .and_then(|start| start.get("column"))
            .and_then(Value::as_i64)
            .unwrap_or(0);
        let start_col = if in_jsx {
            ast_start_column + 1
        } else {
            ast_start_column
        }
        .max(0) as usize;
        let split_at = start_col.min(line.len());

        let part1 = &line[..split_at];
        let part2 = &line[split_at..];
        let padding_len = part1.bytes().take_while(|byte| *byte == b' ').count();
        let padding = format!("{}  ", &part1[..padding_len]);

        let mut new_code_parts = vec![part1.to_string()];
        new_code_parts.extend(format_comment(comments, &padding, false));

        if !part2.trim().is_empty() {
            new_code_parts.push(format!("{}{}", padding, part2));
        }

        let mut result = contents[..start].to_vec();
        result.extend(new_code_parts.join("\n").bytes());
        result.extend(&contents[end_of_line..]);
        return Ok(result);
    }

    if in_jsx && node_type(ast) == Some("JSXText") {
        let mut first_non_whitespace_character = start_offset;
        let mut at_end_of_line = true;
        while first_non_whitespace_character < contents.len() {
            let Some(byte) = buffer_char_at(&contents, first_non_whitespace_character) else {
                break;
            };
            if is_newline(byte) {
                break;
            }
            if is_edible(byte) {
                first_non_whitespace_character += 1;
            } else {
                at_end_of_line = false;
                break;
            }
        }
        if at_end_of_line {
            return Ok(contents);
        }

        /*
         * Otherwise add an expression container above the text with our comment.
         *
         * <jsx>
         *   {// Comment}
         *   JSX Text Here
         * <jsx>
         */
        let comment = format!("{}\n", format_comment(comments, &line, true).join("\n"));
        return Ok(insert_comment_to_text(contents, start, &comment));
    }

    Ok(contents)
}

pub(crate) fn remove_unused_error_suppression_from_text(
    mut contents: Vec<u8>,
    start_offset: usize,
    end_offset: usize,
    comment_ast: Option<&mut CommentAst>,
    ast: &Value,
) -> io::Result<Vec<u8>> {
    // remove the comment and surrounding whitespace
    let (start, end) = expand_comment(&contents, start_offset, end_offset, comment_ast, ast);
    if start > end || end > contents.len() {
        return Err(io::Error::other("computed invalid comment removal range"));
    }
    contents.drain(start..end);
    Ok(contents)
}

/* Take up to `max` characters from str, trying to split at a space or dash or
 * something like that. */
fn split_at_word(str_: &str, max: usize) -> (String, String) {
    let mut ret = String::new();
    let mut maybe = String::new();
    let chars = str_.chars().collect::<Vec<_>>();
    for index in 0..max {
        if index == chars.len() {
            ret.push_str(&maybe);
            break;
        }
        let ch = chars[index];
        maybe.push(ch);
        if matches!(ch, '-' | ' ' | '_' | '\t') {
            ret.push_str(&maybe);
            maybe.clear();
        }
    }
    if ret.is_empty() {
        ret = maybe;
    }
    let rest = chars[ret.chars().count()..].iter().copied().collect();
    (ret, rest)
}

/* Figures out how to pad the comment and split it into multiple lines */
fn format_comment(comments: &[String], line: &str, jsx: bool) -> Vec<String> {
    let padding_len = line.bytes().take_while(|byte| *byte == b' ').count();
    let mut padding = line[..padding_len].to_string();
    if padding.len() > 40 {
        padding = "    ".to_string();
    }

    if !jsx {
        let single_line_comments = comments
            .iter()
            .map(|comment| format!("{}// {}", padding, comment))
            .collect::<Vec<_>>();
        if comments.iter().all(|comment| comment.len() <= 80) {
            return single_line_comments;
        }
    }

    let mut comment_lines = Vec::new();
    let first_line_prefix = if jsx {
        format!("{} /* ", padding)
    } else {
        format!("{}/* ", padding)
    };
    for comment in comments {
        let (first_line_comment, mut remaining_comment) = split_at_word(
            comment.trim(),
            80usize.saturating_sub(first_line_prefix.len()),
        );
        comment_lines.push(format!(
            "{}{}",
            first_line_prefix,
            first_line_comment.trim(),
        ));

        let prefix = if jsx {
            format!("{}  * ", padding)
        } else {
            format!("{} * ", padding)
        };
        while !remaining_comment.is_empty() {
            let (comment_line, rest) = split_at_word(
                remaining_comment.trim(),
                80usize.saturating_sub(prefix.len()),
            );
            remaining_comment = rest;
            comment_lines.push(format!("{}{}", prefix, comment_line.trim()));
        }
        if comment_lines.last().is_some_and(|last| last.len() < 76) {
            let last = comment_lines
                .pop()
                .expect("comment_lines should contain the current comment");
            comment_lines.push(format!("{} */", last));
        } else {
            comment_lines.push(format!("{} */", padding));
        }
    }
    if jsx {
        if let Some(first) = comment_lines.first_mut() {
            *first = format!("{}{{{}", padding, first.trim());
        }
        if let Some(last) = comment_lines.last_mut() {
            *last = format!("{}}}", last);
        }
    }
    comment_lines
}
