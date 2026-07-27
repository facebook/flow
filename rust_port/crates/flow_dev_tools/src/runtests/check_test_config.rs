/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs;
use std::io;
use std::path::Path;

use super::exists;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct TestConfig {
    pub(super) auto_start: bool,
    pub(super) shell: String,
    pub(super) cmd: String,
    pub(super) stdin: String,
    pub(super) ignore_stderr: bool,
    pub(super) cwd: String,
    pub(super) file_watcher: String,
    pub(super) start_args: String,
    pub(super) wait_for_recheck: String,
    pub(super) skip_saved_state: bool,
    pub(super) saved_state_only: bool,
    pub(super) git: bool,
    pub(super) skip_windows: bool,
}

pub(super) fn parse_test_config(test_dir: &Path) -> io::Result<TestConfig> {
    let mut config = TestConfig {
        auto_start: true,
        shell: String::new(),
        cmd: "full-check".to_owned(),
        stdin: String::new(),
        ignore_stderr: true,
        cwd: String::new(),
        file_watcher: "none".to_owned(),
        start_args: String::new(),
        wait_for_recheck: "true".to_owned(),
        skip_saved_state: false,
        saved_state_only: false,
        git: false,
        skip_windows: false,
    };

    let config_path = test_dir.join(".testconfig");
    if !exists(&config_path) {
        return Ok(config);
    }

    let content = fs::read_to_string(config_path)?;
    let lines = content.split('\n');

    // Track whether cmd was explicitly set in the config file.
    // In bash, shell is processed first (clears cmd), then cmd is processed
    // (overrides). This means cmd always wins regardless of file order.
    // We match that by deferring the shell→cmd clearing to after the loop.
    let mut cmd_explicitly_set = false;

    for line in lines {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        let Some((key, value)) = trimmed.split_once(':') else {
            continue;
        };

        let key = key.trim();
        let value = value.trim();

        match key {
            "auto_start" => config.auto_start = value != "false",
            "shell" => config.shell = value.to_owned(),
            "cmd" => {
                config.cmd = value.to_owned();
                cmd_explicitly_set = true;
            }
            "stdin" => config.stdin = value.to_owned(),
            "ignore_stderr" => config.ignore_stderr = value != "false",
            "cwd" => config.cwd = value.to_owned(),
            "file_watcher" => {
                if !value.is_empty() {
                    config.file_watcher = value.to_owned();
                }
            }
            // start_args takes everything after "start_args:" with whitespace
            // trimmed. Use .trim() to match bash's awk field-splitting which
            // normalizes leading whitespace. Without this, extra spaces after
            // the colon leave residual whitespace that causes split(/\s+/) to
            // produce empty array elements.
            "start_args" => config.start_args = value.to_owned(),
            "wait_for_recheck" => {
                config.wait_for_recheck =
                    if value == "false" { "false" } else { "true" }.to_owned();
            }
            "skip_saved_state" => config.skip_saved_state = value == "true",
            "saved_state_only" => config.saved_state_only = value == "true",
            "git" => config.git = value == "true",
            "skip_windows" => config.skip_windows = value == "true",
            _ => {}
        }
    }

    if !config.shell.is_empty() && !cmd_explicitly_set {
        config.cmd.clear();
    }

    Ok(config)
}
