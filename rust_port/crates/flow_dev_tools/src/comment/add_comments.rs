/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Reverse;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::sync::LazyLock;

use regex::Regex;

use crate::ErrorCheckCommand;
use crate::comment::comment_mutator::add_comment_to_text;
use crate::comment::get_ast::get_ast;
use crate::comment::get_context::get_context;
use crate::comment::get_path_to_loc::PathNode;
use crate::comment::get_path_to_loc::get_path_to_loc;
use crate::errors::filter_errors;
use crate::errors::get_flow_errors;
use crate::errors::main_source_loc_of_error;
use crate::flow_result::FlowError;
use crate::flow_result::FlowLoc;

static LINT_NAME_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\(`([^`]+)`\)$").expect("lint name regex should compile"));

#[derive(Clone, Debug)]
pub struct Args {
    pub bin: String,
    pub flowconfig_name: String,
    pub comment: Option<String>,
    pub error_code: Option<String>,
    pub error_check_command: ErrorCheckCommand,
    pub root: PathBuf,
}

#[derive(Clone, Debug)]
struct Suppression {
    loc: FlowLoc,
    is_error: bool,
    lints: BTreeSet<String>,
    error_codes: Vec<String>,
}

pub fn runner(args: Args) -> io::Result<()> {
    let flow_result = get_flow_errors(
        &args.bin,
        args.error_check_command,
        &args.root,
        &args.flowconfig_name,
    )?;

    if flow_result.passed {
        println!("No errors found. Nothing to do. Exiting");
        return Ok(());
    }

    let errors = filter_errors(&flow_result.errors);

    add_comments(&args, errors)
}

fn add_comments(args: &Args, errors: Vec<FlowError>) -> io::Result<()> {
    let mut filename_to_line_to_locs_map: BTreeMap<String, BTreeMap<i64, Suppression>> =
        BTreeMap::new();
    // Filter out errors without a main location
    let mut error_count = 0;
    for error in errors {
        let loc = main_source_loc_of_error(&error);
        let error_codes = error.error_codes.clone();
        if let Some(loc) = loc
            && let Some(source) = loc.source.clone()
        {
            let line_to_locs_map = filename_to_line_to_locs_map
                .entry(source.clone())
                .or_default();
            let is_error = error.kind != "lint";
            let mut lints = BTreeSet::new();
            if error.kind == "lint" {
                if let Some(descr) = error.message.first().map(|message| message.descr.as_str())
                    && let Some(captures) = LINT_NAME_REGEX.captures(descr)
                    && let Some(lint) = captures.get(1)
                {
                    lints.insert(lint.as_str().to_string());
                }
            }
            let prev_value = line_to_locs_map.remove(&loc.start.line);
            let value = join_suppression(
                prev_value,
                Suppression {
                    loc: loc.clone(),
                    is_error,
                    lints,
                    error_codes,
                },
            );
            line_to_locs_map.insert(loc.start.line, value);
            error_count += 1;
        }
    }

    let mut counts = Vec::new();
    for (source, line_to_locs_map) in filename_to_line_to_locs_map {
        counts.push(add_comments_to_source(
            args,
            &source,
            line_to_locs_map.into_values().collect(),
        )?);
    }
    let comment_count = counts.into_iter().sum::<usize>();
    println!(
        "Added {} comments to suppress {} errors",
        comment_count, error_count,
    );
    Ok(())
}

fn join_suppression(prev_value: Option<Suppression>, mut new_value: Suppression) -> Suppression {
    let Some(prev_value) = prev_value else {
        return new_value;
    };

    new_value.is_error = new_value.is_error || prev_value.is_error;
    new_value.lints.extend(prev_value.lints);
    new_value.error_codes.extend(prev_value.error_codes);
    new_value
}

/* A single file needs 1 or more comments added. Start at the bottom of the
 * file, and add comments going up. Then write the changes */
fn add_comments_to_source(args: &Args, source: &str, locs: Vec<Suppression>) -> io::Result<usize> {
    let code_string = fs::read_to_string(source)?;

    let (code, comment_count) = add_comments_to_code(
        args.comment.as_deref(),
        args.error_code.as_deref(),
        code_string,
        locs,
        &args.bin,
    )?;
    fs::write(source, code)?;
    Ok(comment_count)
}

fn add_comments_to_code_internal(
    comments: &[String],
    code: String,
    loc: &FlowLoc,
    path: &[PathNode<'_>],
) -> io::Result<String> {
    String::from_utf8(add_comment_to_text(
        code.into_bytes(),
        loc,
        get_context(loc, path),
        comments,
        None,
    )?)
    .map_err(io::Error::other)
}

fn add_comments_to_code(
    comment: Option<&str>,
    error_code: Option<&str>,
    mut code: String,
    mut locs: Vec<Suppression>,
    _flow_bin_path: &str,
) -> io::Result<(String, usize)> {
    locs.sort_by_key(|suppression| Reverse(suppression.loc.start.line));

    let ast = get_ast(&code)?;

    let mut comment_count = 0;
    for Suppression {
        loc,
        is_error: _is_error,
        lints: _lints,
        mut error_codes,
    } in locs
    {
        if let Some(error_code) = error_code {
            error_codes.retain(|code| code == error_code);
        }
        if error_codes.is_empty() {
            continue;
        }

        let path = get_path_to_loc(&loc, &ast);

        if let Some(path) = path {
            let c = comment.unwrap_or("");
            let mut unique_error_codes = Vec::new();
            for error_code in &error_codes {
                if !unique_error_codes
                    .iter()
                    .any(|existing| existing == error_code)
                {
                    unique_error_codes.push(error_code.clone());
                }
            }
            let mut comments = unique_error_codes
                .into_iter()
                .map(|error_code| {
                    if c.is_empty() {
                        format!("$FlowFixMe[{}]", error_code)
                    } else {
                        format!("$FlowFixMe[{}] {}", error_code, c)
                    }
                })
                .collect::<Vec<_>>();

            // The order doesn't matter for suppression comments. For implementation reasons
            // we had comments in reverse order. This is now no longer the case but we are
            // preserving this behavour to make testing against existing suppression
            // locations easier.
            comments.reverse();

            code = add_comments_to_code_internal(&comments, code, &loc, &path)?;
            comment_count += error_codes.len();
        }
    }
    Ok((code, comment_count))
}
