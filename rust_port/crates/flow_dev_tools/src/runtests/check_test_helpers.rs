/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::Path;

use super::ExecOptions;
use super::ExecResult;
use super::exec_file;

pub(super) struct TestContextOptions<'a> {
    pub(super) flow_bin: &'a Path,
    pub(super) test_dir: &'a Path,
    pub(super) log_file: &'a Path,
    pub(super) monitor_log_file: &'a Path,
    pub(super) no_flowlib: bool,
    pub(super) wait_for_recheck: &'a str,
    pub(super) file_watcher: &'a str,
    pub(super) long_lived_workers: &'a str,
    pub(super) env: &'a HashMap<String, String>,
}

pub(super) struct TestContext<'a> {
    flow_bin: &'a Path,
    test_dir: &'a Path,
    log_file: &'a Path,
    monitor_log_file: &'a Path,
    no_flowlib: bool,
    wait_for_recheck: &'a str,
    file_watcher: &'a str,
    long_lived_workers: &'a str,
    env: &'a HashMap<String, String>,
}

impl<'a> TestContext<'a> {
    pub(super) fn new(opts: TestContextOptions<'a>) -> Self {
        Self {
            flow_bin: opts.flow_bin,
            test_dir: opts.test_dir,
            log_file: opts.log_file,
            monitor_log_file: opts.monitor_log_file,
            no_flowlib: opts.no_flowlib,
            wait_for_recheck: opts.wait_for_recheck,
            file_watcher: opts.file_watcher,
            long_lived_workers: opts.long_lived_workers,
            env: opts.env,
        }
    }

    fn flow_cmd(&self, args: &[String]) -> io::Result<ExecResult> {
        exec_file(
            &self.flow_bin.to_string_lossy(),
            args,
            &ExecOptions {
                cwd: Some(self.test_dir.to_path_buf()),
                env: Some(self.env.clone()),
                ..ExecOptions::default()
            },
            None,
        )
    }

    pub(super) fn create_saved_state(&self, root: &Path, flowconfig_name: &str) -> bool {
        let mut start_args = vec!["start".to_owned(), root.display().to_string()];
        if self.no_flowlib {
            start_args.push("--no-flowlib".to_owned());
        }
        start_args.extend([
            "--wait".to_owned(),
            "--wait-for-recheck".to_owned(),
            self.wait_for_recheck.to_owned(),
            "--lazy-mode".to_owned(),
            "none".to_owned(),
            "--file-watcher".to_owned(),
            self.file_watcher.to_owned(),
            "--flowconfig-name".to_owned(),
            flowconfig_name.to_owned(),
            "--log-file".to_owned(),
            self.log_file.display().to_string(),
            "--monitor-log-file".to_owned(),
            self.monitor_log_file.display().to_string(),
            "--long-lived-workers".to_owned(),
            self.long_lived_workers.to_owned(),
        ]);
        let start_result = match self.flow_cmd(&start_args) {
            Ok(result) => result,
            Err(_) => return false,
        };
        if start_result.code != 0 {
            return false;
        }

        let saved_state_file = root.join(".flow.saved_state");
        let changes_file = root.join(".flow.saved_state_file_changes");
        let saved = match self.flow_cmd(&[
            "save-state".to_owned(),
            "--root".to_owned(),
            root.display().to_string(),
            "--out".to_owned(),
            saved_state_file.display().to_string(),
            "--flowconfig-name".to_owned(),
            flowconfig_name.to_owned(),
        ]) {
            Ok(result) if result.code == 0 => match fs::write(changes_file, "") {
                Ok(()) => true,
                Err(_) => false,
            },
            Ok(_) | Err(_) => false,
        };
        match self.flow_cmd(&[
            "stop".to_owned(),
            "--flowconfig-name".to_owned(),
            flowconfig_name.to_owned(),
            root.display().to_string(),
        ]) {
            Ok(_) => saved,
            Err(_) => false,
        }
    }
}
