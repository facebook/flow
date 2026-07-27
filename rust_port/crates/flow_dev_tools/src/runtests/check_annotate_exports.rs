/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::fs;
use std::future::Future;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use futures::StreamExt;
use regex::Regex;
use similar::DiffOp;
use similar::TextDiff;

use super::ExecOptions;
use super::exec_file;
use super::split_shell_args;

// Recursively find files matching patterns, sorted with LC_ALL=C semantics.
// Uses async fs to avoid blocking the event loop during file discovery.
// Limits concurrency to avoid exhausting file descriptors.
const FIND_CONCURRENCY: usize = 32;

fn find_files<'a>(
    dir: &'a Path,
    patterns: &'a [Regex],
    base_dir: &'a Path,
) -> Pin<Box<dyn Future<Output = io::Result<Vec<String>>> + Send + 'a>> {
    Box::pin(async move {
        let mut results = Vec::new();
        let mut read_dir = tokio::fs::read_dir(dir).await?;
        let mut entries = Vec::new();
        while let Some(entry) = read_dir.next_entry().await? {
            entries.push(entry.file_name());
        }
        for batch in entries.chunks(FIND_CONCURRENCY) {
            let batch_results = futures::stream::iter(batch.to_vec())
                .map(|entry| {
                    let full_path = dir.join(entry);
                    async move {
                        let metadata = match tokio::fs::metadata(&full_path).await {
                            Ok(metadata) => metadata,
                            Err(_) => return Vec::new(),
                        };
                        if metadata.is_dir() {
                            return find_files(&full_path, patterns, base_dir)
                                .await
                                .unwrap_or_default();
                        }
                        if metadata.is_file() {
                            let relative = match full_path.strip_prefix(base_dir) {
                                Ok(relative) => relative,
                                Err(_) => return Vec::new(),
                            };
                            let relative =
                                format!("./{}", relative.to_string_lossy().replace('\\', "/"));
                            if patterns.iter().any(|pattern| pattern.is_match(&relative)) {
                                return vec![relative];
                            }
                        }
                        Vec::new()
                    }
                })
                .buffer_unordered(FIND_CONCURRENCY)
                .collect::<Vec<_>>()
                .await;
            for batch_result in batch_results {
                results.extend(batch_result);
            }
        }
        Ok(results)
    })
}

// Generate normal diff format (matching `diff` without -u flag) using
// the `diff` npm package.  Normal format uses `<` / `>` / `---` markers
// and `N,NcN,N` style range headers.
fn normal_diff(old_text: &str, new_text: &str) -> String {
    // Normalize \r\n and \r → \n to match bash's `diff --strip-trailing-cr`
    let old_norm = old_text.replace("\r\n", "\n").replace('\r', "\n");
    let new_norm = new_text.replace("\r\n", "\n").replace('\r', "\n");
    let changes = TextDiff::from_lines(&old_norm, &new_norm);
    let mut result = Vec::new();
    let mut old_line = 1;
    let mut new_line = 1;

    for change in changes.ops() {
        match change {
            DiffOp::Replace {
                old_index,
                old_len,
                new_index,
                new_len,
            } => {
                let lines = &changes.old_slices()[*old_index..old_index + old_len];
                let count = lines.len();
                let ends_with_newline = lines.last().is_some_and(|line| line.ends_with('\n'));
                let added_lines = &changes.new_slices()[*new_index..new_index + new_len];
                let added_count = added_lines.len();
                let added_ends_with_newline =
                    added_lines.last().is_some_and(|line| line.ends_with('\n'));
                let old_range = if count == 1 {
                    old_line.to_string()
                } else {
                    format!("{old_line},{}", old_line + count - 1)
                };
                let new_range = if added_count == 1 {
                    new_line.to_string()
                } else {
                    format!("{new_line},{}", new_line + added_count - 1)
                };
                result.push(format!("{old_range}c{new_range}"));
                for line in lines {
                    result.push(format!("< {}", line.strip_suffix('\n').unwrap_or(line)));
                }
                if !ends_with_newline {
                    result.push("\\ No newline at end of file".to_owned());
                }
                result.push("---".to_owned());
                for line in added_lines {
                    result.push(format!("> {}", line.strip_suffix('\n').unwrap_or(line)));
                }
                if !added_ends_with_newline {
                    result.push("\\ No newline at end of file".to_owned());
                }
                old_line += count;
                new_line += added_count;
            }
            DiffOp::Delete {
                old_index, old_len, ..
            } => {
                // Pure deletion
                let lines = &changes.old_slices()[*old_index..old_index + old_len];
                let count = lines.len();
                let ends_with_newline = lines.last().is_some_and(|line| line.ends_with('\n'));
                let old_range = if count == 1 {
                    old_line.to_string()
                } else {
                    format!("{old_line},{}", old_line + count - 1)
                };
                result.push(format!("{old_range}d{}", new_line - 1));
                for line in lines {
                    result.push(format!("< {}", line.strip_suffix('\n').unwrap_or(line)));
                }
                if !ends_with_newline {
                    result.push("\\ No newline at end of file".to_owned());
                }
                old_line += count;
            }
            DiffOp::Insert {
                new_index, new_len, ..
            } => {
                // Pure addition (no preceding removal)
                let lines = &changes.new_slices()[*new_index..new_index + new_len];
                let count = lines.len();
                let ends_with_newline = lines.last().is_some_and(|line| line.ends_with('\n'));
                let new_range = if count == 1 {
                    new_line.to_string()
                } else {
                    format!("{new_line},{}", new_line + count - 1)
                };
                result.push(format!("{}a{new_range}", old_line - 1));
                for line in lines {
                    result.push(format!("> {}", line.strip_suffix('\n').unwrap_or(line)));
                }
                if !ends_with_newline {
                    result.push("\\ No newline at end of file".to_owned());
                }
                new_line += count;
            }
            DiffOp::Equal { len, .. } => {
                // Unchanged
                old_line += len;
                new_line += len;
            }
        }
    }

    result.join("\n")
}

pub(super) struct AnnotateExportsResult {
    pub(super) output: String,  // JS: output: string,
    pub(super) stderr: String,  // JS: stderr: string,
    pub(super) error_code: i32, // JS: errorCode: number,
}

pub(super) struct AnnotateExportsOptions {
    pub(super) flow_bin: PathBuf,
    pub(super) test_dir: PathBuf,
    pub(super) no_flowlib: bool,
    pub(super) cmd_args: String,
    pub(super) log_file: PathBuf,
    pub(super) monitor_log_file: PathBuf,
    pub(super) wait_for_recheck: String,
    pub(super) file_watcher: String,
    pub(super) long_lived_workers: String,
    pub(super) env: HashMap<String, String>,
}

pub(super) fn run_annotate_exports(
    options: AnnotateExportsOptions,
) -> io::Result<AnnotateExportsResult> {
    let AnnotateExportsOptions {
        flow_bin,
        test_dir,
        no_flowlib,
        cmd_args,
        log_file,
        monitor_log_file,
        wait_for_recheck,
        file_watcher,
        long_lived_workers,
        env,
    } = options;
    let mut output = String::new();
    let mut stderr_output = String::new();
    let mut error_code = 0;

    // Find all .js and .js.flow files, sorted with C locale (byte order)
    let patterns = [
        Regex::new(r"\.js$").map_err(io::Error::other)?,
        Regex::new(r"\.js\.flow$").map_err(io::Error::other)?,
    ];
    let mut files = flow_tokio_runtime::block_on(find_files(&test_dir, &patterns, &test_dir))?;
    files.sort();

    // Write input file
    let input_file = test_dir.join("input.txt");
    fs::write(&input_file, format!("{}\n", files.join("\n")))?;
    output.push_str("\n=== Codemod annotate-exports ===\n\n");

    // Keep copies of original files
    for file in &files {
        fs::copy(test_dir.join(file), test_dir.join(format!("{file}.orig")))?;
    }

    // Run codemod annotate-exports
    let flowlib_args = if no_flowlib {
        vec!["--no-flowlib".to_owned()]
    } else {
        Vec::new()
    };
    let extra_args = if cmd_args.trim().is_empty() {
        Vec::new()
    } else {
        split_shell_args(cmd_args.trim())?
    };
    let mut codemod_args = vec!["codemod".to_owned(), "annotate-exports".to_owned()];
    codemod_args.extend(flowlib_args.iter().cloned());
    codemod_args.extend(extra_args);
    codemod_args.extend([
        "--strip-root".to_owned(),
        "--quiet".to_owned(),
        "--input-file".to_owned(),
        input_file.display().to_string(),
        "--write".to_owned(),
        ".".to_owned(),
    ]);
    let codemod_result = exec_file(
        &flow_bin.to_string_lossy(),
        &codemod_args,
        &ExecOptions {
            cwd: Some(test_dir.clone()),
            env: Some(env.clone()),
            max_buffer: Some(100 * 1024 * 1024),
            ..ExecOptions::default()
        },
        None,
    )?;
    stderr_output.push_str(&codemod_result.stderr);
    let codemod_status_code = codemod_result.code;

    // Keep copies of codemod-ed files
    for file in &files {
        fs::copy(
            test_dir.join(file),
            test_dir.join(format!("{file}.codemod")),
        )?;
    }

    // Compare codemod-ed with original
    for file in &files {
        let original = fs::read_to_string(test_dir.join(format!("{file}.orig")))?;
        let current = fs::read_to_string(test_dir.join(file))?;
        if original.replace('\r', "") != current.replace('\r', "") {
            output.push_str(&format!(">>> {file}\n"));
            output.push_str(&format!("{}\n", current.replace('\r', "")));
        }
    }

    // Match bash `(echo "$codemod_out"; echo "")` semantics: command
    // substitution strips trailing newlines, echo adds one, echo "" adds another.
    output.push_str(
        codemod_result
            .stdout
            .replace('\r', "")
            .trim_end_matches('\n'),
    );
    output.push_str("\n\n");
    output.push_str("\n=== Autofix exports ===\n\n");

    // Restore original versions
    for file in &files {
        fs::copy(test_dir.join(format!("{file}.orig")), test_dir.join(file))?;
    }

    // Start flow for autofix (assert exit 0, matching bash's `start_flow . --quiet`)
    let mut start_args = vec!["start".to_owned(), ".".to_owned()];
    start_args.extend(flowlib_args);
    start_args.extend([
        "--wait".to_owned(),
        "--quiet".to_owned(),
        "--wait-for-recheck".to_owned(),
        wait_for_recheck,
        "--file-watcher".to_owned(),
        file_watcher,
        "--log-file".to_owned(),
        log_file.display().to_string(),
        "--monitor-log-file".to_owned(),
        monitor_log_file.display().to_string(),
        "--long-lived-workers".to_owned(),
        long_lived_workers,
    ]);
    let start_result = exec_file(
        &flow_bin.to_string_lossy(),
        &start_args,
        &ExecOptions {
            cwd: Some(test_dir.clone()),
            env: Some(env.clone()),
            max_buffer: Some(100 * 1024 * 1024),
            ..ExecOptions::default()
        },
        None,
    )?;
    if start_result.code != 0 {
        stderr_output.push_str(&start_result.stderr);
        return Ok(AnnotateExportsResult {
            output,
            stderr: stderr_output,
            error_code: start_result.code,
        });
    }

    // Run autofix exports on each file, ensuring the server is stopped
    // even if an autofix call fails.
    let autofix_result = (|| -> io::Result<()> {
        for file in &files {
            let args = [
                "autofix".to_owned(),
                "exports".to_owned(),
                "--strip-root".to_owned(),
                "--in-place".to_owned(),
                file.clone(),
            ];
            let result = exec_file(
                &flow_bin.to_string_lossy(),
                &args,
                &ExecOptions {
                    cwd: Some(test_dir.clone()),
                    env: Some(env.clone()),
                    max_buffer: Some(100 * 1024 * 1024),
                    ..ExecOptions::default()
                },
                None,
            )?;
            stderr_output.push_str(&result.stderr);
        }
        Ok(())
    })();

    let stop_result = exec_file(
        &flow_bin.to_string_lossy(),
        &["stop".to_owned(), ".".to_owned()],
        &ExecOptions {
            cwd: Some(test_dir.clone()),
            env: Some(env.clone()),
            max_buffer: Some(100 * 1024 * 1024),
            ..ExecOptions::default()
        },
        None,
    );
    match (autofix_result, stop_result) {
        (Ok(()), Ok(_)) => {}
        (Err(error), Ok(_)) | (Ok(()), Err(error)) => return Err(error),
        (Err(autofix_error), Err(stop_error)) => {
            return Err(io::Error::new(
                autofix_error.kind(),
                format!("{autofix_error}; additionally failed to stop Flow: {stop_error}"),
            ));
        }
    }

    // Keep copies of autofix-ed files
    for file in &files {
        fs::copy(
            test_dir.join(file),
            test_dir.join(format!("{file}.autofix")),
        )?;
    }

    // Compare autofix-ed with original
    for file in &files {
        let original = fs::read_to_string(test_dir.join(format!("{file}.orig")))?;
        let autofix = fs::read_to_string(test_dir.join(format!("{file}.autofix")))?;
        if original.replace('\r', "") != autofix.replace('\r', "") {
            output.push_str(&format!(">>> {file}\n"));
            output.push_str(&format!("{}\n", autofix.replace('\r', "")));
        }
    }

    // Compare codemod-ed and autofix-ed files using normal diff format
    // (matching bash's plain `diff`, not `diff -u`) for cross-platform
    // compatibility instead of shelling out to `diff`.
    output.push_str("\n=== Diff between codemod-ed & autofix-ed ===\n");
    for file in &files {
        // JS: for (const file of files) {
        let codemod = fs::read_to_string(test_dir.join(format!("{file}.codemod")))?;
        let autofix = fs::read_to_string(test_dir.join(format!("{file}.autofix")))?;
        if codemod.replace('\r', "") != autofix.replace('\r', "") {
            let patch = normal_diff(&codemod, &autofix);
            output.push_str(&format!(">>> {file}\n{patch}\n\n"));
        }
    }
    if codemod_status_code != 0 && codemod_status_code != 2 {
        error_code = codemod_status_code;
    }
    Ok(AnnotateExportsResult {
        output,
        stderr: stderr_output,
        error_code,
    })
}
