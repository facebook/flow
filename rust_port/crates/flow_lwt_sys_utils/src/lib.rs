/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::process::ExitStatus;

#[derive(Debug)]
pub struct CommandResult {
    pub stdout: String,
    pub stderr: String,
    pub status: ExitStatus,
}

#[cfg(target_arch = "wasm32")]
pub async fn exec(
    _env: Option<&[(String, String)]>,
    _cwd: Option<&str>,
    _cmd: &str,
    _args: &[&str],
) -> CommandResult {
    panic!("LwtSysUtils.exec is not supported on wasm targets");
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn exec(
    env: Option<&[(String, String)]>,
    cwd: Option<&str>,
    cmd: &str,
    args: &[&str],
) -> CommandResult {
    let mut command = tokio::process::Command::new(cmd);
    command.args(args);
    if let Some(envs) = env {
        for (k, v) in envs {
            command.env(k, v);
        }
    }
    if let Some(cwd) = cwd {
        command.current_dir(cwd);
    }
    let output = command
        .output()
        .await
        .expect("LwtSysUtils.exec spawn failed; OCaml propagates as rejected Lwt promise");
    CommandResult {
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        status: output.status,
    }
}
