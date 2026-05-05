/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::OnceLock;

fn read_proc_file(filename: &str, pid: u32) -> Result<String, String> {
    let file = format!("/proc/{}/{}", pid, filename);
    std::fs::read_to_string(&file).map_err(|e| format!("{}", e))
}

fn parse_cgroup(raw_cgroup_contents: &str) -> Result<String, String> {
    let lines: Vec<&str> = raw_cgroup_contents.split('\n').collect();
    match lines.first() {
        None => Err("Expected at least one cgroup in /proc/<PID>/cgroup file".to_string()),
        Some(first_line) => {
            let parts: Vec<&str> = first_line.split(':').collect();
            if parts.len() == 3 {
                Ok(parts[2].to_string())
            } else {
                Err(
                    "First line of  /proc/<PID>/cgroup file was not correctly formatted"
                        .to_string(),
                )
            }
        }
    }
}

fn asset_procfs_supported() -> Result<(), String> {
    static MEMOIZED_RESULT: OnceLock<Result<(), String>> = OnceLock::new();
    MEMOIZED_RESULT
        .get_or_init(|| {
            if cfg!(unix) && std::path::Path::new("/proc").exists() {
                Ok(())
            } else {
                Err("Proc filesystem not supported".to_string())
            }
        })
        .clone()
}

/// In cgroup v1 a pid can be in multiple cgroups.
/// In cgroup v2 it will only be in a single cgroup.
// let first_cgroup_for_pid pid =
pub fn first_cgroup_for_pid(pid: u32) -> Result<String, String> {
    asset_procfs_supported()?;
    let contents = read_proc_file("cgroup", pid)?;
    parse_cgroup(&contents)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_cgroup_returns_cgroup_from_first_line() {
        assert_eq!(
            Ok("/user.slice/user-123.slice/session.scope".to_string()),
            parse_cgroup("0::/user.slice/user-123.slice/session.scope\n")
        );
    }

    #[test]
    fn parse_cgroup_rejects_malformed_first_line() {
        assert_eq!(
            Err("First line of  /proc/<PID>/cgroup file was not correctly formatted".to_string()),
            parse_cgroup("not:a:cgroup:line\n0::/ignored")
        );
    }
}
