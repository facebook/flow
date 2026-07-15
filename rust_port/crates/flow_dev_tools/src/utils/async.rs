/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::process::Output;
use std::process::Stdio;

pub(crate) fn exec_manual(
    bin: &str,
    args: &[String],
    cwd: Option<&Path>,
    stdin: Option<&str>,
) -> io::Result<Output> {
    let mut command = Command::new(bin);
    command.args(args);
    if let Some(cwd) = cwd {
        command.current_dir(cwd);
    }
    if stdin.is_some() {
        command.stdin(Stdio::piped());
    }
    command.stdout(Stdio::piped()).stderr(Stdio::piped());

    let mut child = command.spawn()?;
    if let Some(input) = stdin {
        let Some(mut child_stdin) = child.stdin.take() else {
            return Err(io::Error::other("failed to open child stdin"));
        };
        child_stdin.write_all(input.as_bytes())?;
    }

    child.wait_with_output()
}
