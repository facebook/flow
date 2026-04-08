/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug)]
pub enum ErrorStatus {
    NotInstalled { path: String },
    Errored(String),
}

pub(crate) enum Env {
    Extend(Vec<(String, String)>),
}

pub(crate) async fn exec(
    env: Option<Env>,
    cwd: Option<&str>,
    cmd: &str,
    args: &[&str],
) -> Result<String, ErrorStatus> {
    let mut command = tokio::process::Command::new(cmd);
    command.args(args);
    if let Some(cwd) = cwd {
        command.current_dir(cwd);
    }
    if let Some(Env::Extend(vars)) = env {
        let mut env_map: std::collections::HashMap<String, String> = std::env::vars().collect();
        for (k, v) in vars {
            env_map.insert(k, v);
        }
        command.envs(env_map);
    }
    let output = match command.output().await {
        Ok(output) => output,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            let path = std::env::var("PATH").unwrap_or_else(|_| "(not set)".to_string());
            return Err(ErrorStatus::NotInstalled { path });
        }
        Err(e) => {
            return Err(ErrorStatus::Errored(format!(
                "{} failed to execute: {}",
                cmd, e
            )));
        }
    };
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let status = output.status;

    if status.success() {
        return Ok(stdout);
    }
    if status.code() == Some(127) {
        let path = std::env::var("PATH").unwrap_or_else(|_| "(not set)".to_string());
        return Err(ErrorStatus::NotInstalled { path });
    }
    if let Some(code) = status.code() {
        let msg = format!("{} exited code {}, stderr = {:?}", cmd, code, stderr);
        return Err(ErrorStatus::Errored(msg));
    }
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(signal) = status.signal() {
            let signal_name = string_of_signal(signal);
            let msg = format!("{} signaled with {} signal", cmd, signal_name);
            return Err(ErrorStatus::Errored(msg));
        }
    }
    let msg = format!("{} terminated abnormally", cmd);
    Err(ErrorStatus::Errored(msg))
}

fn string_of_signal(signal: i32) -> String {
    match signal {
        1 => "SIGHUP".to_string(),
        2 => "SIGINT".to_string(),
        3 => "SIGQUIT".to_string(),
        4 => "SIGILL".to_string(),
        5 => "SIGTRAP".to_string(),
        6 => "SIGABRT".to_string(),
        7 => "SIGBUS".to_string(),
        8 => "SIGFPE".to_string(),
        9 => "SIGKILL".to_string(),
        10 => "SIGUSR1".to_string(),
        11 => "SIGSEGV".to_string(),
        12 => "SIGUSR2".to_string(),
        13 => "SIGPIPE".to_string(),
        14 => "SIGALRM".to_string(),
        15 => "SIGTERM".to_string(),
        _ => format!("signal {}", signal),
    }
}

pub fn split_null_terminated_lines(content: &str) -> Vec<&str> {
    let lines: Vec<&str> = content.split('\x00').collect();
    match lines.last() {
        Some(&"") => lines[..lines.len() - 1].to_vec(),
        _ => lines,
    }
}
