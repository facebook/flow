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
) -> std::io::Result<CommandResult> {
    panic!("LwtSysUtils.exec is not supported on wasm targets");
}

/// Fails only if `cmd` could not be spawned (e.g. it is not on `PATH`). A command that runs and
/// exits nonzero is still `Ok`, with the exit code in `status`.
#[cfg(not(target_arch = "wasm32"))]
pub async fn exec(
    env: Option<&[(String, String)]>,
    cwd: Option<&str>,
    cmd: &str,
    args: &[&str],
) -> std::io::Result<CommandResult> {
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
    let output = command.output().await?;
    Ok(CommandResult {
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        status: output.status,
    })
}
