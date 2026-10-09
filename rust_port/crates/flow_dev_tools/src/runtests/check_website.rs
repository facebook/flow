/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;

use regex::Regex;
use similar::TextDiff;

use super::ExecOptions;
use super::ExecResult;
use super::check_run_one_test::copy_dir;
use super::check_test_config::TestVariant;
use super::exec_file;

pub(super) struct WebsiteTestOptions<'a> {
    pub(super) variant: TestVariant,
    pub(super) flow_bin: &'a Path,
    pub(super) website_dir: &'a Path,
    pub(super) work_dir: &'a Path,
    pub(super) env: &'a HashMap<String, String>,
    pub(super) record: bool,
}

pub(super) fn run_website_test(opts: WebsiteTestOptions<'_>) -> io::Result<ExecResult> {
    match opts.variant {
        TestVariant::WebsiteFlowCheck => {
            let mut result = check_website_root(&opts, opts.website_dir)?;
            if result.code != 0 {
                result.stderr.push_str(&format!(
                    "Website full-check exited with code {}\n",
                    result.code
                ));
            }
            Ok(result)
        }
        TestVariant::WebsiteDocsFlowCheck => check_docs(opts),
        TestVariant::Standard => Err(io::Error::other("Expected a website test variant")),
    }
}

fn check_website_root(opts: &WebsiteTestOptions<'_>, root: &Path) -> io::Result<ExecResult> {
    exec_file(
        &opts.flow_bin.to_string_lossy(),
        &[
            "full-check".to_owned(),
            root.display().to_string(),
            "--builtin-lib".to_owned(),
            "default".to_owned(),
            "--strip-root".to_owned(),
            "--show-all-errors".to_owned(),
            "--message-width".to_owned(),
            "120".to_owned(),
        ],
        &ExecOptions {
            cwd: Some(opts.work_dir.to_path_buf()),
            env: Some(opts.env.clone()),
            ..ExecOptions::default()
        },
        None,
    )
}

fn collect_files(root: &Path, relative: &Path, files: &mut Vec<PathBuf>) -> io::Result<()> {
    for entry in fs::read_dir(root.join(relative))? {
        let entry = entry?;
        let path = relative.join(entry.file_name());
        if entry.file_type()?.is_dir() {
            collect_files(root, &path, files)?;
        } else {
            files.push(path);
        }
    }
    Ok(())
}

struct Block {
    snippet_name: String,
    code: String,
    first_error: bool,
}

fn extract_blocks(opts: &WebsiteTestOptions<'_>) -> io::Result<BTreeMap<PathBuf, Vec<Block>>> {
    let fence_re = Regex::new(r"```(?:js|jsx)\s+flow-check\b").map_err(io::Error::other)?;
    let block_re = Regex::new(r"(?s)```(?:js|jsx)\s+flow-check\b([^\n]*)\n(.*?)```")
        .map_err(io::Error::other)?;
    let safe_name_re = Regex::new(r"[^a-zA-Z0-9]").map_err(io::Error::other)?;
    let snippets_dir = opts.work_dir.join("snippets");
    fs::create_dir_all(&snippets_dir)?;
    let mut blocks_by_doc = BTreeMap::new();
    let mut snippet_names = BTreeSet::new();
    for (source, extension, prefix) in [("docs", "md", ""), ("src/pages", "mdx", "pages")] {
        let root = opts.website_dir.join(source);
        let mut files = Vec::new();
        collect_files(&root, Path::new(""), &mut files)?;
        files.sort();
        for relative in files {
            if relative.extension().is_none_or(|ext| ext != extension) {
                continue;
            }
            let content = fs::read_to_string(root.join(&relative))?;
            let display = Path::new(prefix).join(relative);
            for (line_no, line) in content.lines().enumerate() {
                if line.trim_start().starts_with("```")
                    && line.contains("flow-check")
                    && !fence_re.is_match(line)
                {
                    return Err(io::Error::other(format!(
                        "Malformed flow-check code fence at {}:{}:\n  {}\nExpected ```js flow-check or ```jsx flow-check",
                        display.display(),
                        line_no + 1,
                        line.trim()
                    )));
                }
            }
            let safe_name = safe_name_re
                .replace_all(&display.with_extension("").to_string_lossy(), "_")
                .into_owned();
            let blocks = block_re
                .captures_iter(&content)
                .enumerate()
                .map(|(i, capture)| {
                    let snippet_name = format!("{safe_name}__{:03}.js", i + 1);
                    if !snippet_names.insert(snippet_name.clone()) {
                        return Err(io::Error::other(format!(
                            "Duplicate snippet filename: {snippet_name}"
                        )));
                    }
                    let code = format!("// @flow\n{}", &capture[2]);
                    fs::write(snippets_dir.join(&snippet_name), &code)?;
                    Ok(Block {
                        snippet_name,
                        code,
                        first_error: capture[1]
                            .split_whitespace()
                            .any(|option| option == "first-error"),
                    })
                })
                .collect::<io::Result<Vec<_>>>()?;
            if !blocks.is_empty() {
                blocks_by_doc.insert(display, blocks);
            }
        }
    }
    if blocks_by_doc.is_empty() {
        return Err(io::Error::other("No flow-check blocks found"));
    }
    Ok(blocks_by_doc)
}

fn render_snapshots(
    blocks_by_doc: &BTreeMap<PathBuf, Vec<Block>>,
    raw_output: &str,
) -> io::Result<BTreeMap<PathBuf, String>> {
    let header_re = Regex::new(r"(?m)^Error\s+-+\s+([^\n]+)|^Found\s+\d+\s+errors?\b")
        .map_err(io::Error::other)?;
    let snippet_re = Regex::new(r"^snippets/([^:]+):\d+:\d+").map_err(io::Error::other)?;
    let path_re = Regex::new(r"snippets/[A-Za-z0-9_]+\.js").map_err(io::Error::other)?;
    let error_line_re = Regex::new(r"(?m)^Error\s+-+\s+-:(\d+):\d+").map_err(io::Error::other)?;
    let annotation_re = Regex::new(r"(?i)(?://|/\*).*\berror\b").map_err(io::Error::other)?;
    let excluded_re =
        Regex::new(r"(?i)(?:\bno\b.*\berror\b|flowlint)").map_err(io::Error::other)?;
    let mut errors_by_snippet: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let headers: Vec<_> = header_re.captures_iter(raw_output).collect();
    for (index, header) in headers.iter().enumerate() {
        let Some(location) = header.get(1) else {
            continue;
        };
        let Some(snippet) = snippet_re.captures(location.as_str()) else {
            return Err(io::Error::other(format!(
                "Documentation error outside a snippet: {}",
                location.as_str()
            )));
        };
        let start = header.get(0).expect("matched error header").start();
        let end = headers.get(index + 1).map_or(raw_output.len(), |next| {
            next.get(0).expect("matched error header").start()
        });
        errors_by_snippet
            .entry(snippet[1].to_owned())
            .or_default()
            .push(
                path_re
                    .replace_all(raw_output[start..end].trim_end(), "-")
                    .into_owned(),
            );
    }
    let mut validation_failures = Vec::new();
    let mut snapshots = BTreeMap::new();
    for (doc, blocks) in blocks_by_doc {
        let mut out_lines = Vec::new();
        for (block_index, block) in blocks.iter().enumerate() {
            let errors = errors_by_snippet
                .get(&block.snippet_name)
                .map_or(&[][..], Vec::as_slice);
            out_lines.push(format!("=== block {} ===", block_index + 1));
            if errors.is_empty() {
                out_lines.push("No errors!".to_owned());
            } else {
                out_lines.push(errors.join("\n\n"));
                out_lines.extend([String::new(), String::new()]);
                out_lines.push(format!(
                    "Found {} error{}",
                    errors.len(),
                    if errors.len() == 1 { "" } else { "s" }
                ));
            }
            out_lines.push(String::new());
            let error_lines = errors
                .iter()
                .flat_map(|error| error_line_re.captures_iter(error))
                .map(|capture| capture[1].parse::<usize>().map_err(io::Error::other))
                .collect::<io::Result<BTreeSet<_>>>()?;
            for (line_index, line) in block.code.lines().enumerate() {
                let line_no = line_index + 1;
                let is_error = error_lines.contains(&line_no);
                let required =
                    is_error && (!block.first_error || error_lines.first() == Some(&line_no));
                let has_annotation = annotation_re.is_match(line);
                let mismatch = if required && !has_annotation {
                    Some("missing // error")
                } else if has_annotation
                    && !excluded_re.is_match(line)
                    && !line.trim_start().starts_with("//")
                    && !line.trim_start().starts_with("/*")
                    && !is_error
                {
                    Some("has // error but no error reported")
                } else {
                    None
                };
                if let Some(message) = mismatch {
                    validation_failures.push(format!(
                        "{} block {} line {line_no}: {message}: {}",
                        doc.display(),
                        block_index + 1,
                        line.trim()
                    ));
                }
            }
        }
        snapshots.insert(
            doc.with_extension("exp"),
            format!("{}\n", out_lines.join("\n").trim_end()),
        );
    }
    if !validation_failures.is_empty() {
        return Err(io::Error::other(format!(
            "Error annotation validation failures:\n{}",
            validation_failures.join("\n")
        )));
    }
    Ok(snapshots)
}

fn compare_snapshots(root: &Path, actual: &BTreeMap<PathBuf, String>) -> io::Result<String> {
    let mut files = Vec::new();
    if root.is_dir() {
        collect_files(root, Path::new(""), &mut files)?;
    }
    let paths: BTreeSet<_> = files.into_iter().chain(actual.keys().cloned()).collect();
    let mut diff = String::new();
    for relative in paths {
        let expected = match fs::read_to_string(root.join(&relative)) {
            Ok(content) => content,
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                diff.push_str(&format!("Missing snapshot: {}\n", relative.display()));
                String::new()
            }
            Err(error) => return Err(error),
        };
        let Some(actual) = actual.get(&relative) else {
            diff.push_str(&format!("Stale snapshot: {}\n", relative.display()));
            continue;
        };
        let expected = expected.replace("\r\n", "\n");
        diff.push_str(
            &TextDiff::from_lines(&expected, actual)
                .unified_diff()
                .header(
                    &relative.display().to_string(),
                    &format!("generated/{}", relative.display()),
                )
                .to_string(),
        );
    }
    Ok(diff)
}

fn check_docs(opts: WebsiteTestOptions<'_>) -> io::Result<ExecResult> {
    let flowconfig = fs::read_to_string(opts.website_dir.join(".flowconfig.snippets"))?;
    let flowconfig = flowconfig
        .split_inclusive('\n')
        .filter(|line| line.trim_end_matches(['\r', '\n']) != "**")
        .collect::<String>();
    fs::write(opts.work_dir.join(".flowconfig"), flowconfig)?;
    flow_tokio_runtime::block_on(copy_dir(
        &opts.website_dir.join("flow-typed"),
        &opts.work_dir.join("flow-typed"),
    ))?;
    let blocks_by_doc = extract_blocks(&opts)?;
    let mut result = check_website_root(&opts, opts.work_dir)?;
    if result.code != 0 && result.code != 2 {
        result.stderr.push_str(&format!(
            "Documentation full-check exited with code {}\n",
            result.code
        ));
        return Ok(result);
    }
    let snapshots = render_snapshots(&blocks_by_doc, &result.stdout)?;
    let snapshot_dir = opts.website_dir.join("tests/snapshots");
    if opts.record {
        if snapshot_dir.is_dir() {
            fs::remove_dir_all(&snapshot_dir)?;
        }
        for (relative, content) in &snapshots {
            let path = snapshot_dir.join(relative);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(path, content)?;
        }
        result.stdout = "All flow-check examples match snapshots.\n".to_owned();
    } else {
        let diff = compare_snapshots(&snapshot_dir, &snapshots)?;
        if diff.is_empty() {
            result.stdout = "All flow-check examples match snapshots.\n".to_owned();
        } else {
            result.code = 1;
            result.stdout = format!(
                "{diff}\nFlow-check examples differ from snapshots.\nRe-record with: FLOW_BINARY dev-tools runtests -r -t website_docs_flow_check\n"
            );
            return Ok(result);
        }
    }
    result.code = 0;
    result.stderr.push_str(&format!(
        "Checked {} flow-check blocks from {} files\n",
        blocks_by_doc.values().map(Vec::len).sum::<usize>(),
        blocks_by_doc.len()
    ));
    Ok(result)
}
