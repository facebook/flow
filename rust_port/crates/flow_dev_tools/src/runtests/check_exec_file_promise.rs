/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::fs;
use std::io;
#[cfg(windows)]
use std::io::Read;
#[cfg(windows)]
use std::io::Seek;
use std::path::PathBuf;
use std::process::Stdio;
use std::time::Duration;

use tokio::io::AsyncWriteExt;
use tokio::process::Command;

#[derive(Debug, Eq, PartialEq)]
pub(super) struct ExecResult {
    pub(super) code: i32,
    pub(super) stdout: String,
    pub(super) stderr: String,
}

const DEFAULT_TIMEOUT: Duration = Duration::from_secs(10 * 60);

#[derive(Clone, Debug, Default)]
pub(super) struct ExecOptions {
    // JS: options?: Object,
    pub(super) cwd: Option<PathBuf>, // JS: ...options,
    pub(super) env: Option<HashMap<String, String>>, // JS: ...options,
    pub(super) timeout: Option<Duration>, // JS: timeout: (options && options.timeout) || DEFAULT_TIMEOUT_MS,
    pub(super) max_buffer: Option<usize>, // JS: maxBuffer: (options && options.maxBuffer) || 100 * 1024 * 1024,
}

pub(super) fn exists(path: &std::path::Path) -> bool {
    let accessible = fs::metadata(path);
    accessible.is_ok()
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Quote {
    Single,
    Double,
}

pub(super) fn split_shell_args(arg_string: &str) -> io::Result<Vec<String>> {
    let mut args = Vec::new();
    let mut current = String::new();
    let mut quote = None;
    let mut had_quote = false;
    let mut chars = arg_string.chars().peekable();

    while let Some(ch) = chars.next() {
        if quote == Some(Quote::Single) {
            if ch == '\'' {
                quote = None;
            } else {
                current.push(ch);
            }
            continue;
        }

        if quote == Some(Quote::Double) {
            if ch == '"' {
                quote = None;
            } else if ch == '\\' {
                if let Some(next) = chars.next() {
                    if matches!(next, '$' | '`' | '"' | '\\' | '\n') {
                        current.push(next);
                    } else {
                        current.push('\\');
                        current.push(next);
                    }
                } else {
                    current.push('\\');
                }
            } else {
                current.push(ch);
            }
            continue;
        }

        if ch == '\'' {
            quote = Some(Quote::Single);
            had_quote = true;
        } else if ch == '"' {
            quote = Some(Quote::Double);
            had_quote = true;
        } else if ch.is_whitespace() {
            if !current.is_empty() || had_quote {
                args.push(std::mem::take(&mut current));
                had_quote = false;
            }
        } else if ch == '\\' {
            if let Some(next) = chars.next() {
                current.push(next);
            } else {
                current.push('\\');
            }
        } else {
            current.push(ch);
        }
    }

    if quote.is_some() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("Unterminated quote in argument string: {arg_string}"),
        ));
    }
    if !current.is_empty() || had_quote {
        args.push(current);
    }
    Ok(args)
}

pub(super) fn exec_file(
    cmd: &str,
    args: &[String],
    options: &ExecOptions,
    stdin_data: Option<&str>,
) -> io::Result<ExecResult> {
    flow_tokio_runtime::block_on(async {
        let mut command = Command::new(cmd);
        command.args(args);
        command
            .stdin(if stdin_data.is_some() {
                Stdio::piped()
            } else {
                Stdio::null()
            })
            .kill_on_drop(true);
        #[cfg(windows)]
        let (mut stdout_file, mut stderr_file) = {
            let stdout_file = tempfile::tempfile()?;
            let stderr_file = tempfile::tempfile()?;
            command
                .stdout(Stdio::from(stdout_file.try_clone()?))
                .stderr(Stdio::from(stderr_file.try_clone()?));
            (stdout_file, stderr_file)
        };
        #[cfg(not(windows))]
        command.stdout(Stdio::piped()).stderr(Stdio::piped());
        if let Some(cwd) = &options.cwd {
            command.current_dir(cwd);
        }
        if let Some(env) = &options.env {
            command.env_clear().envs(env);
        }
        let max_buffer = options
            .max_buffer
            .filter(|max_buffer| *max_buffer > 0)
            .unwrap_or(100 * 1024 * 1024);
        let timeout = options
            .timeout
            .filter(|timeout| !timeout.is_zero())
            .unwrap_or(DEFAULT_TIMEOUT);

        let mut child = match command.spawn() {
            Ok(child) => child,
            Err(_spawn_error) => {
                return Ok(ExecResult {
                    code: 1,
                    stdout: String::new(),
                    stderr: String::new(),
                });
            }
        };
        let stdin = child.stdin.take();
        #[cfg(windows)]
        let status = match tokio::time::timeout(timeout, async move {
            let write_stdin = async move {
                if let (Some(mut stdin), Some(stdin_data)) = (stdin, stdin_data) {
                    match stdin.write_all(stdin_data.as_bytes()).await {
                        Ok(()) => {}
                        Err(_ignored_by_stream_error_handler) => {}
                    }
                }
            };
            let ((), status) = tokio::join!(write_stdin, child.wait());
            status
        })
        .await
        {
            Ok(status) => status?,
            Err(_elapsed) => {
                return Ok(ExecResult {
                    code: 1,
                    stdout: String::new(),
                    stderr: String::new(),
                });
            }
        };
        #[cfg(windows)]
        let (stdout, stderr) = {
            let mut stdout = Vec::new();
            let mut stderr = Vec::new();
            stdout_file.rewind()?;
            stderr_file.rewind()?;
            stdout_file.read_to_end(&mut stdout)?;
            stderr_file.read_to_end(&mut stderr)?;
            (stdout, stderr)
        };
        #[cfg(not(windows))]
        let output = match tokio::time::timeout(timeout, async move {
            let write_stdin = async move {
                if let (Some(mut stdin), Some(stdin_data)) = (stdin, stdin_data) {
                    match stdin.write_all(stdin_data.as_bytes()).await {
                        Ok(()) => {}
                        Err(_ignored_by_stream_error_handler) => {}
                    }
                }
            };
            let ((), output) = tokio::join!(write_stdin, child.wait_with_output());
            output
        })
        .await
        {
            Ok(output) => output?,
            Err(_elapsed) => {
                return Ok(ExecResult {
                    code: 1,
                    stdout: String::new(),
                    stderr: String::new(),
                });
            }
        };
        #[cfg(not(windows))]
        let (status, stdout, stderr) = (output.status, output.stdout, output.stderr);
        let code = if stdout.len() > max_buffer || stderr.len() > max_buffer {
            1
        } else {
            status.code().unwrap_or(1)
        };
        Ok(ExecResult {
            code,
            stdout: String::from_utf8_lossy(&stdout).into_owned(),
            stderr: String::from_utf8_lossy(&stderr).into_owned(),
        })
    })
}

pub(super) fn exec_shell(
    cmd_string: &str,         // JS: cmdString: string,
    options: &ExecOptions,    // JS: options?: Object,
    stdin_data: Option<&str>, // JS: stdinData?: string,
) -> io::Result<ExecResult> {
    flow_tokio_runtime::block_on(async {
        #[cfg(windows)]
        let mut command = {
            let mut command =
                Command::new(std::env::var_os("ComSpec").unwrap_or_else(|| "cmd.exe".into()));
            command.args(["/D", "/S", "/C", cmd_string]);
            command
        };
        #[cfg(not(windows))]
        let mut command = {
            let mut command = Command::new("/bin/sh");
            command.args(["-c", cmd_string]);
            command
        };
        command
            .stdin(if stdin_data.is_some() {
                Stdio::piped()
            } else {
                Stdio::null()
            })
            .kill_on_drop(true);
        #[cfg(windows)]
        let (mut stdout_file, mut stderr_file) = {
            let stdout_file = tempfile::tempfile()?;
            let stderr_file = tempfile::tempfile()?;
            command
                .stdout(Stdio::from(stdout_file.try_clone()?))
                .stderr(Stdio::from(stderr_file.try_clone()?));
            (stdout_file, stderr_file)
        };
        #[cfg(not(windows))]
        command.stdout(Stdio::piped()).stderr(Stdio::piped());
        if let Some(cwd) = &options.cwd {
            command.current_dir(cwd);
        }
        if let Some(env) = &options.env {
            command.env_clear().envs(env);
        }
        let max_buffer = options
            .max_buffer
            .filter(|max_buffer| *max_buffer > 0)
            .unwrap_or(100 * 1024 * 1024);
        let timeout = options
            .timeout
            .filter(|timeout| !timeout.is_zero())
            .unwrap_or(DEFAULT_TIMEOUT);

        let mut child = match command.spawn() {
            Ok(child) => child,
            Err(_spawn_error) => {
                return Ok(ExecResult {
                    code: 1,
                    stdout: String::new(),
                    stderr: String::new(),
                });
            }
        };
        let stdin = child.stdin.take();
        #[cfg(windows)]
        let status = match tokio::time::timeout(timeout, async move {
            let write_stdin = async move {
                if let (Some(mut stdin), Some(stdin_data)) = (stdin, stdin_data) {
                    match stdin.write_all(stdin_data.as_bytes()).await {
                        Ok(()) => {}
                        Err(_ignored_by_stream_error_handler) => {}
                    }
                }
            };
            let ((), status) = tokio::join!(write_stdin, child.wait());
            status
        })
        .await
        {
            Ok(status) => status?,
            Err(_elapsed) => {
                return Ok(ExecResult {
                    code: 1,
                    stdout: String::new(),
                    stderr: String::new(),
                });
            }
        };
        #[cfg(windows)]
        let (stdout, stderr) = {
            let mut stdout = Vec::new();
            let mut stderr = Vec::new();
            stdout_file.rewind()?;
            stderr_file.rewind()?;
            stdout_file.read_to_end(&mut stdout)?;
            stderr_file.read_to_end(&mut stderr)?;
            (stdout, stderr)
        };
        #[cfg(not(windows))]
        let output = match tokio::time::timeout(timeout, async move {
            let write_stdin = async move {
                if let (Some(mut stdin), Some(stdin_data)) = (stdin, stdin_data) {
                    match stdin.write_all(stdin_data.as_bytes()).await {
                        Ok(()) => {}
                        Err(_ignored_by_stream_error_handler) => {}
                    }
                }
            };
            let ((), output) = tokio::join!(write_stdin, child.wait_with_output());
            output
        })
        .await
        {
            Ok(output) => output?,
            Err(_elapsed) => {
                return Ok(ExecResult {
                    code: 1,
                    stdout: String::new(),
                    stderr: String::new(),
                });
            }
        };
        #[cfg(not(windows))]
        let (status, stdout, stderr) = (output.status, output.stdout, output.stderr);
        let code = if stdout.len() > max_buffer || stderr.len() > max_buffer {
            1
        } else {
            status.code().unwrap_or(1)
        };
        Ok(ExecResult {
            code,
            stdout: String::from_utf8_lossy(&stdout).into_owned(),
            stderr: String::from_utf8_lossy(&stderr).into_owned(),
        })
    })
}
