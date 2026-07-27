/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::fs;
use std::io;
use std::io::IsTerminal;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use fancy_regex::Regex;
use serde_json::Map;
use serde_json::Value;

use super::CheckRunQueue;
use super::ExecOptions;
use super::QueueOpts;
use super::RunOneTestOptions;
use super::TestJob;
use super::TestStatus;
use super::exec_file;
use super::parse_test_config;
use super::run_one_test;

pub(super) struct RunnerArgs {
    pub(super) version: Option<String>,
    pub(super) bin: PathBuf,
    pub(super) tests_dir: PathBuf,
    pub(super) filter: Option<String>,
    pub(super) parallelism: usize,
    pub(super) check_only: bool,
    pub(super) saved_state: bool,
    pub(super) long_lived_workers: bool,
    pub(super) record: bool,
    pub(super) quiet: bool,
    pub(super) verbose: bool,
    pub(super) json_output: bool,
    pub(super) list_tests: bool,
    pub(super) scripts_dir: PathBuf,
}

const COLOR_RESET: &str = "\x1b[0m";
const COLOR_DEFAULT: &str = "\x1b[39;49;0m";
const COLOR_DEFAULT_BOLD: &str = "\x1b[39;49;1m";
const COLOR_RED_BOLD: &str = "\x1b[31;1m";
const COLOR_GREEN_BOLD: &str = "\x1b[32;1m";
const COLOR_YELLOW_BOLD: &str = "\x1b[33;1m";
const COLOR_MAGENTA_BOLD: &str = "\x1b[35;1m";
const COLOR_WHITE_ON_RED_BOLD: &str = "\x1b[37;41;1m";

fn use_color() -> bool {
    // $FlowFixMe[prop-missing] - isTTY exists on process.stdout at runtime
    io::stdout().is_terminal()
}

fn color(code: &str) -> &str {
    if use_color() { code } else { "" }
}

fn should_list_test(
    test_dir: &Path,
    check_only: bool,
    saved_state: bool,
    env: &HashMap<String, String>,
) -> io::Result<bool> {
    let name = test_dir
        .file_name()
        .map_or_else(String::new, |name| name.to_string_lossy().into_owned());
    if cfg!(windows) && (name == "symlink" || name == "node_tests") {
        return Ok(false);
    }
    let has_exp_file = test_dir.join(format!("{name}.exp")).exists();
    let has_flowconfig = test_dir.join(".flowconfig").exists();
    let has_testconfig = test_dir.join(".testconfig").exists();
    if !has_exp_file || (!has_flowconfig && !has_testconfig) {
        return Ok(name != "auxiliary" && name != "callable");
    }
    let config = parse_test_config(test_dir)?;
    if saved_state && config.skip_saved_state {
        return Ok(false);
    }
    if !saved_state && config.saved_state_only {
        return Ok(false);
    }
    if env.get("FLOW_GIT_BINARY").is_none_or(String::is_empty) && config.git {
        return Ok(false);
    }
    if cfg!(windows) && config.skip_windows {
        return Ok(false);
    }
    if check_only && config.cmd.trim() != "full-check" {
        return Ok(false);
    }
    Ok(true)
}

pub(super) fn check_test_runner(args: RunnerArgs) -> io::Result<bool> {
    // JS: async function checkTestRunner(args: RunnerArgs): Promise<void> {
    let RunnerArgs {
        version,
        bin,
        tests_dir,
        filter,
        parallelism,
        check_only,
        saved_state,
        long_lived_workers,
        record,
        quiet,
        verbose,
        json_output,
        list_tests,
        scripts_dir,
    } = args;

    // Set environment variables needed by tests. Save originals so we can
    // restore them when done, avoiding permanent process.env mutation.
    let mut env: HashMap<String, String> = if cfg!(windows) {
        std::env::vars()
            .map(|(key, value)| (key.to_ascii_uppercase(), value))
            .collect()
    } else {
        std::env::vars().collect()
    };
    let mut saved_env = HashMap::new();
    fn set_env_if_needed(
        env: &mut HashMap<String, String>,
        saved_env: &mut HashMap<String, Option<String>>,
        key: &str,
        value: &str,
        only_if_unset: bool,
    ) {
        if only_if_unset && env.contains_key(key) {
            return;
        }
        saved_env.insert(key.to_owned(), env.get(key).cloned());
        env.insert(key.to_owned(), value.to_owned());
    }
    set_env_if_needed(&mut env, &mut saved_env, "IN_FLOW_TEST", "1", false);
    set_env_if_needed(&mut env, &mut saved_env, "FLOW_LOG_LEVEL", "debug", false);
    set_env_if_needed(&mut env, &mut saved_env, "FLOW_MAX_WORKERS", "2", true);

    // Set git binary
    if !env.contains_key("FLOW_GIT_BINARY") {
        let command = if cfg!(windows) { "where" } else { "which" };
        match exec_file(
            command,
            &["git".to_owned()],
            &ExecOptions {
                env: Some(env.clone()),
                ..ExecOptions::default()
            },
            None,
        ) {
            Ok(result) if result.code == 0 => {
                if let Some(git) = result
                    .stdout
                    .lines()
                    .next()
                    .map(str::trim)
                    .filter(|path| !path.is_empty())
                {
                    // On Windows, `where` can return multiple lines; take the first.
                    set_env_if_needed(&mut env, &mut saved_env, "FLOW_GIT_BINARY", git, false);
                }
            }
            Ok(_) | Err(_) => {
                // git not available
            }
        }
    }

    // Set node binary
    if env.get("FLOW_NODE_BINARY").is_none_or(String::is_empty) {
        let node = env
            .get("NODE_BINARY")
            .filter(|node| !node.is_empty())
            .cloned()
            .unwrap_or_else(|| "node".to_owned());
        set_env_if_needed(&mut env, &mut saved_env, "FLOW_NODE_BINARY", &node, false);
    }

    // Discover test directories
    let tests_dir_resolved = match std::path::absolute(&tests_dir) {
        Ok(path) => path,
        Err(error) => {
            eprintln!(
                "Failed to read tests directory {}: {error}",
                tests_dir.display()
            );
            return Ok(false);
        }
    };
    let mut test_dirs = Vec::new();
    match fs::read_dir(&tests_dir_resolved) {
        Ok(entries) => {
            for entry in entries {
                let Ok(entry) = entry else {
                    continue;
                };
                if fs::metadata(entry.path()).is_ok_and(|metadata| metadata.is_dir()) {
                    test_dirs.push(entry.path());
                }
            }
            test_dirs.sort();
        }
        Err(error) => {
            eprintln!(
                "Failed to read tests directory {}: {error}",
                tests_dir_resolved.display()
            );
            return Ok(false);
        }
    }

    // Apply filter
    if let Some(filter) = &filter {
        let filter_re = match Regex::new(filter) {
            Ok(filter_re) => filter_re,
            Err(error) => {
                eprintln!("Invalid filter regex \"{filter}\": {error}");
                return Ok(false);
            }
        };
        let mut filtered = Vec::new();
        for dir in test_dirs {
            let name = dir
                .file_name()
                .map_or_else(String::new, |name| name.to_string_lossy().into_owned());
            match filter_re.is_match(&name) {
                Ok(true) => filtered.push(dir),
                Ok(false) => {}
                Err(error) => {
                    eprintln!("Invalid filter regex \"{filter}\": {error}");
                    return Ok(false);
                }
            }
        }
        test_dirs = filtered;
    }

    if check_only {
        let mut check_test_dirs = Vec::new();
        for dir in test_dirs {
            if parse_test_config(&dir)?.cmd.trim() == "full-check" {
                check_test_dirs.push(dir);
            }
        }
        test_dirs = check_test_dirs;
    }

    // List mode
    if list_tests {
        let mut listed_test_dirs = Vec::new();
        for dir in &test_dirs {
            if should_list_test(dir, check_only, saved_state, &env)? {
                listed_test_dirs.push(dir);
            }
        }
        for dir in listed_test_dirs {
            let mut name = dir
                .file_name()
                .map_or_else(String::new, |name| name.to_string_lossy().into_owned());
            if saved_state {
                name.push_str("-saved-state");
            } else if long_lived_workers {
                name.push_str("-long-lived-workers");
            }
            println!("{name}");
        }
        return Ok(true);
    }

    // Get version
    let version = if let Some(version) = version {
        version
    } else if test_dirs.is_empty() {
        String::new()
    } else {
        let version_result = exec_file(
            &bin.to_string_lossy(),
            &["version".to_owned(), "--semver".to_owned()],
            &ExecOptions {
                env: Some(env.clone()),
                ..ExecOptions::default()
            },
            None,
        )?;
        if version_result.code != 0 {
            eprintln!(
                "Failed to get Flow version from {}: {}",
                bin.display(),
                version_result.stderr.trim()
            );
            return Ok(false);
        }
        version_result.stdout.trim().to_owned()
    };
    if !quiet {
        println!("Running up to {parallelism} test(s) in parallel");
    }

    // Create jobs
    let jobs = test_dirs
        .iter()
        .enumerate()
        .map(|(index, dir)| {
            let options = RunOneTestOptions {
                test_dir: dir.clone(),
                flow_bin: bin.clone(),
                version: version.clone(),
                check_only,
                saved_state,
                long_lived_workers,
                record,
                scripts_dir: scripts_dir.clone(),
                env: env.clone(),
            };
            TestJob {
                index,
                test_dir: dir.clone(),
                run: Arc::new(move || run_one_test(options.clone())),
            }
        })
        .collect();

    // Run tests
    let queue = CheckRunQueue::new(jobs, QueueOpts { parallelism });
    let completion_receiver = queue.run();
    let mut results = vec![None; test_dirs.len()];

    let mut passed = 0;
    let mut failed = 0;
    let mut skipped = 0;
    let mut errored = 0;
    let mut json_map = Value::Object(Map::new());

    // Print results in order
    for (index, test_dir) in test_dirs.iter().enumerate() {
        let test_name = test_dir
            .file_name()
            .map_or_else(String::new, |name| name.to_string_lossy().into_owned());
        while results[index].is_none() {
            if !quiet && io::stdout().is_terminal() {
                print!(
                    "{}[ ] RUNNING:{} {}{}\r",
                    color(COLOR_DEFAULT_BOLD),
                    color(COLOR_DEFAULT),
                    test_name,
                    color(COLOR_RESET)
                );
                io::stdout().flush()?;
            }
            let Ok((completed_index, result)) = completion_receiver.recv() else {
                break;
            };
            if let Some(slot) = results.get_mut(completed_index) {
                *slot = Some(result);
            }
        }
        let Some(result) = results[index].as_ref() else {
            errored += 1;
            if json_output {
                json_map[&test_name] = Value::Bool(false);
            } else if !quiet {
                println!(
                    "{}[✗] ERRORED:{} {}{}",
                    color(COLOR_RED_BOLD),
                    color(COLOR_DEFAULT),
                    test_name,
                    color(COLOR_RESET)
                );
            }
            continue;
        };

        match result.status {
            TestStatus::Success => {
                passed += 1;
                if json_output {
                    json_map[&test_name] = Value::Bool(true);
                } else if !quiet {
                    println!(
                        "{}[✓] PASSED:{}  {}{}",
                        color(COLOR_GREEN_BOLD),
                        color(COLOR_DEFAULT),
                        test_name,
                        color(COLOR_RESET)
                    );
                }
            }
            TestStatus::Failure => {
                failed += 1;
                if json_output {
                    json_map[&test_name] = Value::Bool(false);
                } else {
                    if record {
                        println!(
                            "{}[✗] UPDATED:{} {}{}",
                            color(COLOR_MAGENTA_BOLD),
                            color(COLOR_DEFAULT),
                            test_name,
                            color(COLOR_RESET)
                        );
                    } else {
                        println!(
                            "{}[✗] FAILED:{}  {}{}",
                            color(COLOR_RED_BOLD),
                            color(COLOR_DEFAULT),
                            test_name,
                            color(COLOR_RESET)
                        );
                    }

                    // Print diff
                    let err_path = test_dir.join(format!("{test_name}.err"));
                    if err_path.exists() {
                        print!("{}", fs::read_to_string(err_path)?);
                    }
                    // Use result.diff directly (more reliable than reading from file,
                    // which may have been cleaned up in record mode)
                    if let Some(diff) = &result.diff {
                        let mut diff_content = diff.clone();
                        if use_color() {
                            diff_content = diff_content
                                .split('\n')
                                .map(|line| {
                                    if line.starts_with('-') {
                                        format!("\x1b[31m{line}\x1b[0m")
                                    } else if line.starts_with('+') {
                                        format!("\x1b[32m{line}\x1b[0m")
                                    } else if line.starts_with('@') {
                                        format!("\x1b[35m{line}\x1b[0m")
                                    } else {
                                        line.to_owned()
                                    }
                                })
                                .collect::<Vec<_>>()
                                .join("\n");
                        }
                        print!("{diff_content}");
                    }
                }
            }
            TestStatus::Skip => {
                skipped += 1;
                if !json_output && !quiet && verbose {
                    println!(
                        "{}[-] SKIPPED:{} {}{}",
                        color(COLOR_YELLOW_BOLD),
                        color(COLOR_DEFAULT),
                        test_name,
                        color(COLOR_RESET)
                    );
                }
            }
            TestStatus::MissingFiles => {
                errored += 1;
                if json_output {
                    json_map[&test_name] = Value::Bool(false);
                } else {
                    println!(
                        "{}[✗] ERRORED:{} {}{}",
                        color(COLOR_RED_BOLD),
                        color(COLOR_DEFAULT),
                        test_name,
                        color(COLOR_RESET)
                    );
                    println!("Missing {test_name}.exp file or .flowconfig file");
                }
            }
            TestStatus::MissingAllOption => {
                errored += 1;
                if json_output {
                    json_map[&test_name] = Value::Bool(false);
                } else {
                    println!(
                        "{}[✗] ERRORED:{} {}{}",
                        color(COLOR_RED_BOLD),
                        color(COLOR_DEFAULT),
                        test_name,
                        color(COLOR_RESET)
                    );
                    println!(
                        "You are required to set either `all=true` or `all=false` in your test `.flowconfig`."
                    );
                }
            }
            TestStatus::Error => {
                errored += 1;
                if json_output {
                    json_map[&test_name] = Value::Bool(false);
                } else {
                    println!(
                        "{}[✗] ERRORED:{} {}{}",
                        color(COLOR_RED_BOLD),
                        color(COLOR_DEFAULT),
                        test_name,
                        color(COLOR_RESET)
                    );

                    // Print error artifacts
                    let out_path = test_dir.join(format!("{test_name}.out"));
                    if out_path.exists() {
                        print!("{}", fs::read_to_string(out_path)?);
                    }
                    let err_path = test_dir.join(format!("{test_name}.err"));
                    if err_path.exists() {
                        print!("\n\nStderr:\n{}", fs::read_to_string(err_path)?);
                    }
                    let monitor_log_path = test_dir.join(format!("{test_name}.monitor_log"));
                    if monitor_log_path.exists() {
                        print!(
                            "\n\nServer monitor log:\n{}",
                            fs::read_to_string(monitor_log_path)?
                        );
                    }
                    let log_path = test_dir.join(format!("{test_name}.log"));
                    if log_path.exists() {
                        print!("\n\nServer log:\n{}", fs::read_to_string(log_path)?);
                    }
                }
            }
        }
    }

    // JSON output
    if json_output {
        writeln!(
            io::stdout().lock(),
            "{}",
            serde_json::to_string(&json_map).map_err(io::Error::other)?
        )?;
    }

    // Summary
    if !quiet {
        let failed_color = if failed > 0 {
            color(COLOR_WHITE_ON_RED_BOLD)
        } else {
            color(COLOR_DEFAULT_BOLD)
        };
        let errored_color = if errored > 0 {
            color(COLOR_WHITE_ON_RED_BOLD)
        } else {
            color(COLOR_DEFAULT_BOLD)
        };
        println!();
        println!(
            "{}Passed: {}, {}Failed: {}{}, Skipped: {}, {}Errored: {}{}",
            color(COLOR_DEFAULT_BOLD),
            passed,
            failed_color,
            failed,
            color(COLOR_DEFAULT_BOLD),
            skipped,
            errored_color,
            errored,
            color(COLOR_RESET)
        );
    }

    // Use exitCode instead of process.exit() to allow stdout to flush
    // completely (process.exit can truncate buffered output when piped).
    io::stdout().flush()?; // allow stdout to flush completely

    // Restore environment variables to their original values.
    for (key, value) in saved_env {
        if let Some(value) = value {
            env.insert(key, value);
        } else {
            env.remove(&key);
        }
    }
    Ok(failed == 0 && errored == 0)
}
