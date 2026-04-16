/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use flow_server_files::server_files_js;

pub(crate) enum FailedToKill {
    Message(Option<String>),
}

fn canonical_root(root: &Path) -> PathBuf {
    root.canonicalize().unwrap_or_else(|_| root.to_path_buf())
}

fn server_lock_is_held(lock_path: &str) -> bool {
    if !Path::new(lock_path).exists() {
        return false;
    }
    let file = match std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open(lock_path)
    {
        Ok(file) => file,
        Err(_) => return true,
    };
    matches!(file.try_lock(), Err(std::fs::TryLockError::WouldBlock))
}

pub(crate) fn server_exists(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> bool {
    let root = canonical_root(root);
    let lock_path = server_files_js::lock_file(flowconfig_name, tmp_dir, &root);
    server_lock_is_held(&lock_path)
}

fn server_may_still_be_alive_after_kill(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> bool {
    server_exists(flowconfig_name, tmp_dir, root)
}

pub(crate) fn mean_kill(
    flowconfig_name: &str,
    tmp_dir: &str,
    root: &Path,
) -> Result<(), FailedToKill> {
    let root = canonical_root(root);
    let pids_file = server_files_js::pids_file(flowconfig_name, tmp_dir, &root);
    let pids = match std::fs::read_to_string(&pids_file) {
        Ok(contents) => contents
            .lines()
            .filter_map(|row| {
                let (pid, reason) = row.split_once('\t')?;
                let pid = pid.parse::<u32>().ok()?;
                Some((pid, reason.to_owned()))
            })
            .collect::<Vec<_>>(),
        Err(_) => {
            let exe_name = std::env::args()
                .next()
                .and_then(|arg| {
                    Path::new(&arg)
                        .file_name()
                        .map(|name| name.to_string_lossy().to_string())
                })
                .unwrap_or_else(|| "flow".to_string());
            let msg = format!(
                "Unable to figure out pids of running Flow server. Try manually killing it with 'pkill {}' (be careful on shared devservers)",
                exe_name
            );
            return Err(FailedToKill::Message(Some(msg)));
        }
    };

    for (pid, _) in pids {
        #[cfg(unix)]
        {
            let output = Command::new("/bin/kill")
                .arg("-9")
                .arg(pid.to_string())
                .output()
                .map_err(|err| FailedToKill::Message(Some(err.to_string())))?;
            if !output.status.success() {
                let exists_output = Command::new("ps")
                    .arg("-p")
                    .arg(pid.to_string())
                    .arg("-o")
                    .arg("pid=")
                    .output()
                    .map_err(|err| FailedToKill::Message(Some(err.to_string())))?;
                if exists_output.status.success()
                    && !String::from_utf8_lossy(&exists_output.stdout)
                        .trim()
                        .is_empty()
                {
                    let stderr = String::from_utf8_lossy(&output.stderr).trim().to_owned();
                    let msg = if stderr.is_empty() {
                        format!("Unable to kill Flow server pid {}", pid)
                    } else {
                        stderr
                    };
                    return Err(FailedToKill::Message(Some(msg)));
                }
            }
            // no such process
        }

        #[cfg(windows)]
        {
            let output = Command::new("taskkill")
                .arg("/PID")
                .arg(pid.to_string())
                .arg("/F")
                .output()
                .map_err(|err| FailedToKill::Message(Some(err.to_string())))?;
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr).to_ascii_lowercase();
                if !stderr.contains("not found")
                    && !stderr.contains("no running instance")
                    && !stderr.contains("not running")
                {
                    let msg = String::from_utf8_lossy(&output.stderr).trim().to_owned();
                    let msg = if msg.is_empty() {
                        format!("Unable to kill Flow server pid {}", pid)
                    } else {
                        msg
                    };
                    return Err(FailedToKill::Message(Some(msg)));
                }
            }
        }
    }

    std::thread::sleep(std::time::Duration::from_secs(1));

    if server_may_still_be_alive_after_kill(flowconfig_name, tmp_dir, &root) {
        return Err(FailedToKill::Message(None));
    }

    Ok(())
}
