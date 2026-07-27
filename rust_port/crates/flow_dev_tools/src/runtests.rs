/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io;
use std::path::Path;

mod check_annotate_exports;
mod check_diff_compare;
mod check_exec_file_promise;
mod check_run_one_test;
mod check_run_queue;
mod check_test_config;
mod check_test_helpers;
mod check_test_runner;

use check_annotate_exports::AnnotateExportsOptions;
use check_annotate_exports::run_annotate_exports;
use check_diff_compare::diff_output;
use check_diff_compare::record_output;
use check_exec_file_promise::ExecOptions;
use check_exec_file_promise::ExecResult;
use check_exec_file_promise::exec_file;
use check_exec_file_promise::exec_shell;
use check_exec_file_promise::exists;
use check_exec_file_promise::split_shell_args;
use check_run_one_test::RunOneTestOptions;
use check_run_one_test::TestResult;
use check_run_one_test::TestStatus;
use check_run_one_test::run_one_test;
use check_run_queue::CheckRunQueue;
use check_run_queue::QueueOpts;
use check_run_queue::TestJob;
use check_test_config::parse_test_config;
use check_test_runner::RunnerArgs;
use check_test_runner::check_test_runner;

pub struct Args {
    pub current_version: String,
    pub tests_dir: Option<String>,
    pub dir: Option<String>,
    pub filter: Option<String>,
    pub test: Option<String>,
    pub run_test: Option<String>,
    pub positional_filter: Option<String>,
    pub parallelism: Option<i32>,
    pub check_only: bool,
    pub saved_state: bool,
    pub long_lived_workers: bool,
    pub record: bool,
    pub quiet: bool,
    pub verbose: bool,
    pub json_output: bool,
    pub list: bool,
    pub list_tests: bool,
}

fn resolve_parallelism(argument: Option<i32>, environment: Option<&str>) -> usize {
    argument
        .filter(|parallelism| *parallelism > 0)
        .or_else(|| {
            environment
                .and_then(|parallelism| parallelism.parse().ok())
                .filter(|parallelism| *parallelism > 0)
        })
        .unwrap_or(16) as usize
}

pub fn run(args: Args) -> io::Result<bool> {
    let Args {
        current_version,
        tests_dir,
        dir,
        mut filter,
        test,
        run_test,
        positional_filter,
        parallelism,
        check_only,
        saved_state,
        long_lived_workers,
        record,
        quiet,
        verbose,
        json_output,
        list,
        list_tests,
    } = args;

    let flow_root = if let Ok(exe) = std::env::current_exe()
        && let Some(root) = exe.parent().and_then(Path::parent)
        && root.join("tests").is_dir()
    {
        root.to_path_buf()
    } else {
        let cwd = std::env::current_dir()?;
        cwd.ancestors()
            .find(|root| {
                root.join("tests").is_dir() && root.join("packages/flow-dev-tools").is_dir()
            })
            .map(Path::to_path_buf)
            .unwrap_or(cwd)
    };

    let bin = std::env::current_exe()?;

    // Determine tests directory
    let tests_dir = match tests_dir {
        Some(tests_dir) => std::path::absolute(tests_dir)?,
        None => match dir {
            Some(dir) => std::path::absolute(dir)?.join("tests"),
            None => flow_root.join("tests"),
        },
    };

    // Handle specific test (-t) as filter
    if let Some(mut test_name) = test.or(run_test) {
        // Strip suffixes like -saved-state or -long-lived-workers
        if saved_state {
            test_name = test_name
                .strip_suffix("-saved-state")
                .unwrap_or(&test_name)
                .to_owned();
        }
        if long_lived_workers {
            test_name = test_name
                .strip_suffix("-long-lived-workers")
                .unwrap_or(&test_name)
                .to_owned();
        }
        filter = Some(format!("^{test_name}$"));
    }

    // Positional arg as filter fallback
    if filter.as_ref().is_none_or(String::is_empty) {
        filter = positional_filter;
    }

    check_test_runner(RunnerArgs {
        version: Some(current_version),
        bin,
        tests_dir,
        filter,
        parallelism: resolve_parallelism(
            parallelism,
            std::env::var("FLOW_RUNTESTS_PARALLELISM").ok().as_deref(),
        ),
        check_only,
        saved_state,
        long_lived_workers,
        record,
        quiet: quiet || json_output,
        verbose,
        json_output,
        list_tests: list || list_tests,
        scripts_dir: flow_root.join("scripts"),
    })
}
