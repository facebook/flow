/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::sync::LazyLock;

use regex::Regex;
use serde_json::Value;

use crate::ErrorCheckCommand;
use crate::comment::comment_mutator::CommentAst;
use crate::comment::comment_mutator::remove_unused_error_suppression_from_text;
use crate::comment::get_ast::get_ast;
use crate::errors::collate_locs;
use crate::errors::get_unused_suppression_errors;
use crate::flow_result::FlowLoc;
use crate::flow_result::required_offset;

static FLOWTEST_SUFFIX_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"-flowtest\.js$").expect("flowtest suffix regex should compile"));

static FLOWTEST_DIR_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"[/\\]__flowtests__[/\\]").expect("flowtest directory regex should compile")
});

#[derive(Clone, Debug)]
pub struct Args {
    pub bin: String,
    pub flowconfig_name: String,
    pub error_check_command: ErrorCheckCommand,
    pub root: PathBuf,
    pub include_flowtest: bool,
}

fn get_errors(args: &Args) -> io::Result<BTreeMap<String, Vec<FlowLoc>>> {
    let errors = get_unused_suppression_errors(
        &args.bin,
        args.error_check_command,
        &args.root,
        &args.flowconfig_name,
    )?;
    Ok(collate_locs(errors))
}

fn remove_unused_error_suppressions(
    filename: &str,
    errors: Vec<FlowLoc>,
    flow_bin_path: &str,
) -> io::Result<()> {
    let contents = fs::read(filename)?;
    let contents = remove_unused_error_suppressions_from_text(contents, errors, flow_bin_path)?;
    fs::write(filename, contents)
}

fn remove_unused_error_suppressions_from_text(
    mut contents: Vec<u8>,
    mut errors: Vec<FlowLoc>,
    _flow_bin_path: &str,
) -> io::Result<Vec<u8>> {
    // Sort in reverse order so that we remove comments later in the file first. Otherwise, the
    // removal of comments earlier in the file would outdate the locations for comments later in the
    // file.
    errors.sort_by(|left, right| {
        right
            .start
            .offset
            .unwrap_or(0)
            .cmp(&left.start.offset.unwrap_or(0))
    });

    let ast = get_ast(&String::from_utf8_lossy(&contents))?;
    let mut comments = ast
        .get("comments")
        .and_then(Value::as_array)
        .map(|comments| {
            comments
                .iter()
                .filter_map(|comment| {
                    let range = comment.get("range")?.as_array()?;
                    let comment_start_offset = range.first()?.as_u64()? as usize;
                    let comment_end_offset = range.get(1)?.as_u64()? as usize;
                    Some(CommentAst {
                        value: comment
                            .get("value")
                            .and_then(Value::as_str)
                            .unwrap_or("")
                            .to_string(),
                        range: (comment_start_offset, comment_end_offset),
                    })
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    for error in errors {
        let orig_start = required_offset(&error.start, "unused suppression start")?;
        let orig_end = required_offset(&error.end, "unused suppression end")?;

        let mut comment_ast = None;
        for comment in &mut comments {
            let (comment_start_offset, comment_end_offset) = comment.range;
            if orig_start >= comment_start_offset && orig_end <= comment_end_offset {
                comment_ast = Some(comment);
                break;
            }
        }

        contents = remove_unused_error_suppression_from_text(
            contents,
            orig_start,
            orig_end,
            comment_ast,
            &ast,
        )?;
    }
    Ok(contents)
}

/* A flowtest is a file that ends in -flowtest.js or which is in a directory
 * named __flowtests__
 */
fn is_flowtest(filename: &str) -> bool {
    FLOWTEST_SUFFIX_REGEX.is_match(filename) || FLOWTEST_DIR_REGEX.is_match(filename)
}

pub fn runner(args: Args) -> io::Result<()> {
    let mut ignored_file_count = 0;
    let mut ignored_error_count = 0;
    let mut removed_error_count = 0;
    let raw_errors = get_errors(&args)?;
    let mut errors = Vec::new();
    for (filename, file_errors) in raw_errors {
        // Filter out flowtests
        if !args.include_flowtest && is_flowtest(&filename) {
            ignored_file_count += 1;
            ignored_error_count += file_errors.len();
        } else {
            removed_error_count += file_errors.len();
            errors.push((filename, file_errors));
        }
    }
    for (filename, file_errors) in &errors {
        remove_unused_error_suppressions(filename, file_errors.clone(), &args.bin)?;
    }
    println!(
        "Removed {} comments in {} files",
        removed_error_count,
        errors.len(),
    );
    if ignored_file_count > 0 {
        println!(
            "Ignored {} comments in {} files due to -flowtest.js suffix or __flowtests__ directory. Run with `--include-flowtest` to also remove those files.",
            ignored_error_count, ignored_file_count,
        );
    }
    Ok(())
}
