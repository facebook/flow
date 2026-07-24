/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::sync::LazyLock;

use regex::Captures;
use regex::Regex;
use serde_json::Value;

use crate::comment::comment_mutator::CommentAst;
use crate::comment::comment_mutator::add_comment_to_text;
use crate::comment::comment_mutator::find_start_of_line;
use crate::comment::comment_mutator::is_eslint_suppression;
use crate::comment::comment_mutator::is_lint_suppression;
use crate::comment::comment_mutator::remove_unused_error_suppression_from_text;
use crate::comment::get_ast::get_ast;
use crate::comment::get_context::get_context;
use crate::comment::get_path_to_loc::get_path_to_loc;
use crate::errors::collate_errors;
use crate::errors::filter_errors;
use crate::errors::is_unused_suppression;
use crate::flow_result::FlowError;
use crate::flow_result::FlowLoc;
use crate::flow_result::FlowResult;
use crate::flow_result::required_offset;

static SITE_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\bsite=([a-z,_]+)\)").expect("site regex should compile"));

static EXISTING_PARENS_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^( *[^ ]+)\(([^)]+)\)").expect("existing parens regex should compile")
});

static FIRST_TOKEN_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^( *[^ ]+)").expect("first token regex should compile"));

static FLOWTEST_SUFFIX_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"-flowtest\.js$").expect("flowtest suffix regex should compile"));

static FLOWTEST_DIR_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"[/\\]__flowtests__[/\\]").expect("flowtest directory regex should compile")
});

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Only {
    Add,
    Remove,
}

pub struct Args<LoadRoot> {
    pub comment: String,
    pub roots: Vec<PathBuf>,
    pub root_names: Vec<String>,
    pub include_flowtest: bool,
    pub only: Option<Only>,
    pub load_root: LoadRoot,
}

#[derive(Clone, Debug)]
struct RootInput {
    name: String,
    files: Vec<String>,
    errors: Vec<FlowError>,
}

impl RootInput {
    fn from_json(name: String, files: Vec<String>, result: Value) -> io::Result<Self> {
        let result: FlowResult = serde_json::from_value(result).map_err(io::Error::other)?;
        Ok(Self {
            name,
            files,
            errors: result.errors,
        })
    }
}

#[derive(Clone, Debug)]
struct UnusedSuppressions {
    roots: BTreeSet<String>,
    loc: FlowLoc,
}

#[derive(Clone, Debug)]
struct ErrorsForLine {
    // Flow only reports one unused suppression per line
    unused_suppressions: Option<UnusedSuppressions>,
    error_codes: OrderedStrings,
    loc: FlowLoc,
}

#[derive(Clone, Debug, Default)]
struct OrderedStrings(Vec<String>);

impl OrderedStrings {
    fn insert(&mut self, value: String) {
        if !self.0.iter().any(|existing| existing == &value) {
            self.0.push(value);
        }
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn iter(&self) -> impl Iterator<Item = &String> {
        self.0.iter()
    }
}

fn normalize_flow_path_key(path: &str) -> String {
    path.trim_end_matches('\r').replace('\\', "/")
}

fn get_files(roots: &[RootInput]) -> BTreeMap<String, BTreeSet<String>> {
    let mut roots_by_file: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for root in roots {
        for file in &root.files {
            let result = roots_by_file
                .entry(normalize_flow_path_key(file))
                .or_default();
            result.insert(root.name.clone());
        }
    }
    roots_by_file
}

fn get_errors_for_all_roots(
    roots: &[RootInput],
    only: Option<Only>,
) -> io::Result<BTreeMap<String, BTreeMap<i64, ErrorsForLine>>> {
    let mut errors_by_file: BTreeMap<String, BTreeMap<i64, ErrorsForLine>> = BTreeMap::new();
    for root in roots {
        for (file, errors_in_file) in collate_errors(root.errors.clone()) {
            let errors_by_line = errors_by_file.entry(file).or_default();
            let errors_in_file_with_main_source_locs = filter_errors(&errors_in_file);
            for error in errors_in_file_with_main_source_locs {
                if error.level == "warning" && !is_unused_suppression(&error) {
                    // Skip regular warnings
                    continue;
                }
                // Only errors with locations can be in here
                let Some(loc) = error
                    .message
                    .first()
                    .and_then(|message| message.loc.clone())
                else {
                    return Err(io::Error::other("Found an error without a location"));
                };
                let data_for_line =
                    errors_by_line
                        .entry(loc.start.line)
                        .or_insert_with(|| ErrorsForLine {
                            unused_suppressions: None,
                            error_codes: OrderedStrings::default(),
                            loc: loc.clone(),
                        });
                if is_unused_suppression(&error) && only != Some(Only::Add) {
                    match &mut data_for_line.unused_suppressions {
                        Some(unused_suppressions) => {
                            unused_suppressions.roots.insert(root.name.clone());
                        }
                        None => {
                            data_for_line.unused_suppressions = Some(UnusedSuppressions {
                                roots: BTreeSet::from([root.name.clone()]),
                                loc,
                            });
                        }
                    }
                } else if only != Some(Only::Remove) {
                    for code in error.error_codes {
                        data_for_line.error_codes.insert(code);
                    }
                }
            }
        }
    }
    Ok(errors_by_file)
}

fn get_sites(text: &str) -> BTreeSet<String> {
    SITE_REGEX
        .captures(text)
        .and_then(|captures| captures.get(1))
        .map(|sites| sites.as_str().split(',').map(str::to_string).collect())
        .unwrap_or_default()
}

fn replace_sites(text: &str, sites: &BTreeSet<String>) -> String {
    let remaining_sites_str = sites.iter().cloned().collect::<Vec<_>>().join(",");

    if SITE_REGEX.is_match(text) {
        // has existing sites, replace them
        SITE_REGEX
            .replacen(text, 1, format!("site={})", remaining_sites_str))
            .to_string()
    } else {
        // no existing sites, try to insert within existing parens
        let new_text = EXISTING_PARENS_REGEX
            .replacen(text, 1, |captures: &Captures<'_>| {
                format!(
                    "{}({} site={})",
                    &captures[1], &captures[2], remaining_sites_str,
                )
            })
            .to_string();

        // if there were no existing parens, add some
        if new_text == text {
            FIRST_TOKEN_REGEX
                .replacen(text, 1, |captures: &Captures<'_>| {
                    format!("{}(site={})", &captures[1], remaining_sites_str)
                })
                .to_string()
        } else {
            new_text
        }
    }
}

/**
 * Updates the `(site=...)` of a suppression comment, or removes the whole
 * comment if it's unused everywhere.
 */
fn update_error_suppression(
    contents: Vec<u8>,
    start_offset: usize,
    end_offset: usize,
    comment_ast: Option<&mut CommentAst>,
    ast: &Value,
    known_roots: &BTreeSet<String>,
    unused_roots: &BTreeSet<String>,
) -> io::Result<Vec<u8>> {
    let (comment_start_offset, _comment_end_offset) = comment_ast
        .as_ref()
        .map(|comment| comment.range)
        .unwrap_or((start_offset, end_offset));
    let inner_offset = comment_start_offset + 2;
    let Some(text_bytes) = contents.get(inner_offset..end_offset) else {
        return Err(io::Error::other("computed invalid suppression text range"));
    };
    let mut text = String::from_utf8_lossy(text_bytes).to_string();

    // do not remove the comment if it's a eslint suppression
    // TODO update the logging provided in the end of the command
    if is_eslint_suppression(&text) {
        return Ok(contents);
    }

    let mut roots = get_sites(&text);
    roots.extend(known_roots.iter().cloned());
    for root in unused_roots {
        roots.remove(root);
    }

    if roots.is_empty() {
        // no roots need the suppression, so remove it
        remove_unused_error_suppression_from_text(
            contents,
            start_offset,
            end_offset,
            comment_ast,
            ast,
        )
    } else {
        if comment_ast
            .as_ref()
            .is_some_and(|comment| is_lint_suppression(comment))
        {
            // lints can't be suppressed per site, so we have to convert it to a
            // $ FlowFixMe(site=<roots>)
            // (deliberately insert space above to avoid removing the comment by suppression removal tool)
            let sites = roots.iter().cloned().collect::<Vec<_>>().join(",");
            text = format!(" $FlowFixMe(site={}) --{}", sites, text);
        } else {
            // only sites in `roots` need the suppression, so add/update site=
            text = replace_sites(&text, &roots);
        }

        let mut result = Vec::new();
        result.extend_from_slice(&contents[..inner_offset]);
        result.extend(text.bytes());
        result.extend_from_slice(&contents[end_offset..]);
        Ok(result)
    }
}

fn update_suppressions(
    filename: &str,
    all_roots: &BTreeSet<String>,
    errors: &BTreeMap<i64, ErrorsForLine>,
    comment: &str,
) -> io::Result<()> {
    let contents = fs::read(filename)?;
    let contents = update_suppressions_in_text(contents, all_roots, errors, comment)?;
    fs::write(filename, contents)
}

fn update_suppressions_in_text(
    mut contents: Vec<u8>,
    all_roots: &BTreeSet<String>,
    errors_by_line: &BTreeMap<i64, ErrorsForLine>,
    comment: &str,
) -> io::Result<Vec<u8>> {
    let mut errors = errors_by_line.iter().collect::<Vec<_>>();
    // Sort in reverse order so that we remove comments later in the file first. Otherwise, the
    // removal of comments earlier in the file would outdate the locations for comments later in the
    // file.
    errors.sort_by(|(line1, _), (line2, _)| line2.cmp(line1));

    let ast = get_ast(&String::from_utf8_lossy(&contents))?;
    if ast
        .get("errors")
        .and_then(Value::as_array)
        .is_some_and(|errors| !errors.is_empty())
    {
        eprintln!(
            "Unable to parse content:\n\n{}",
            String::from_utf8_lossy(&contents),
        );
        // On parser failure, be conservative and do nothing.
        // It's better than going ahead anyways and break original semantics.
        return Ok(contents);
    }

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

    for (
        _line,
        ErrorsForLine {
            loc: error_loc,
            error_codes,
            unused_suppressions,
        },
    ) in errors
    {
        // For each line we:
        // 1. Get the start of the line. All changes will go _AFTER_ this line.
        // 2. Remove the unused suppressions from the line
        // 3. Add the error suppressions at the original start of the line.
        // This ensures that we do not disrupt any offsets for errors coming before this one in the file.
        let beginning_of_line =
            find_start_of_line(&contents, required_offset(&error_loc.start, "error start")?);

        if let Some(unused_suppressions) = unused_suppressions {
            // Need to remove a suppression
            let loc = &unused_suppressions.loc;
            let orig_start = required_offset(&loc.start, "unused suppression start")?;
            let orig_end = required_offset(&loc.end, "unused suppression end")?;
            let mut comment_ast = None;
            for comment in &mut comments {
                let (comment_start_offset, comment_end_offset) = comment.range;
                if orig_start >= comment_start_offset && orig_end <= comment_end_offset {
                    comment_ast = Some(comment);
                    break;
                }
            }

            contents = update_error_suppression(
                contents,
                orig_start,
                orig_end,
                comment_ast,
                &ast,
                all_roots,
                &unused_suppressions.roots,
            )?;
        }
        if !error_codes.is_empty() {
            /* Need to add a suppression.
             * First of all, we want to know if we can add a comment to the line before
             * the error. So we need to see if we're in a JSX children block or inside a
             * template string when we reach the line with the error */
            let path = get_path_to_loc(error_loc, &ast);
            if let Some(path) = path {
                let comments = error_codes
                    .iter()
                    .map(|code| {
                        if comment.is_empty() {
                            format!("$FlowFixMe[{}]", code)
                        } else {
                            format!("$FlowFixMe[{}] {}", code, comment)
                        }
                    })
                    .collect::<Vec<_>>();
                contents = add_comment_to_text(
                    contents,
                    error_loc,
                    get_context(error_loc, &path),
                    &comments,
                    Some(beginning_of_line),
                )?;
            }
        }
    }
    Ok(contents)
}

/* A flowtest is a file that ends in -flowtest.js or which is in a directory
 * named __flowtests__
 */
fn is_flowtest(filename: &str) -> bool {
    FLOWTEST_SUFFIX_REGEX.is_match(filename) || FLOWTEST_DIR_REGEX.is_match(filename)
}

pub fn runner<LoadRoot>(mut args: Args<LoadRoot>) -> io::Result<()>
where
    LoadRoot: FnMut(&Path) -> io::Result<(Vec<String>, Value)>,
{
    let mut roots = Vec::with_capacity(args.roots.len());
    for (index, root) in args.roots.iter().enumerate() {
        let Some(root_name) = args.root_names.get(index) else {
            return Err(io::Error::other(format!(
                "name for {} not set",
                root.display()
            )));
        };
        let (files, result) = (args.load_root)(root)?;
        roots.push(RootInput::from_json(root_name.clone(), files, result)?);
    }

    let mut ignored_file_count = 0;
    let mut ignored_error_count = 0;
    let mut removed_error_count = 0;
    let mut added_suppression_count = 0;
    let mut files_with_unused_suppressions_count = 0;
    let mut files_with_needed_suppressions_count = 0;
    let roots_by_file = get_files(&roots);
    let raw_errors = get_errors_for_all_roots(&roots, args.only)?;
    let mut errors = Vec::new();
    for (filename, errors_for_file) in raw_errors {
        // Filter out flowtests and generate stats
        if !args.include_flowtest && is_flowtest(&filename) {
            ignored_file_count += 1;
            ignored_error_count += errors_for_file.len();
        } else {
            let mut file_has_unused_suppression = false;
            let mut file_needs_suppression = false;
            for ErrorsForLine {
                unused_suppressions,
                error_codes,
                ..
            } in errors_for_file.values()
            {
                if !error_codes.is_empty() {
                    added_suppression_count += error_codes.len();
                    file_needs_suppression = true;
                }
                if unused_suppressions.is_some() {
                    removed_error_count += 1;
                    file_has_unused_suppression = true;
                }
            }
            if file_has_unused_suppression {
                files_with_unused_suppressions_count += 1;
            }
            if file_needs_suppression {
                files_with_needed_suppressions_count += 1;
            }
            errors.push((filename, errors_for_file));
        }
    }
    let mut error_batches = Vec::new();
    while !errors.is_empty() {
        let len = errors.len().min(500);
        error_batches.push(errors.drain(0..len).collect::<Vec<_>>());
    }
    for errors in error_batches {
        for (filename, errors_for_file) in errors {
            let file_key = normalize_flow_path_key(&filename);
            let Some(all_roots) = roots_by_file.get(&file_key) else {
                return Err(io::Error::other(format!("{} not in any site???", filename)));
            };
            update_suppressions(&filename, all_roots, &errors_for_file, &args.comment)?;
        }
    }
    println!(
        "Removed {} comments in {} files",
        removed_error_count, files_with_unused_suppressions_count,
    );
    println!(
        "Added {} comments in {} files",
        added_suppression_count, files_with_needed_suppressions_count,
    );
    if ignored_file_count > 0 {
        println!(
            "Ignored {} comments in {} files due to -flowtest.js suffix or __flowtests__ directory. Run with `--include-flowtest` to also remove those files.",
            ignored_error_count, ignored_file_count,
        );
    }
    Ok(())
}
