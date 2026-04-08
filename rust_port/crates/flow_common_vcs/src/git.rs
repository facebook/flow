/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub use crate::vcs_utils::ErrorStatus;
use crate::vcs_utils::exec;
use crate::vcs_utils::split_null_terminated_lines;

async fn git(cwd: Option<&str>, args: &[&str]) -> Result<String, ErrorStatus> {
    exec(None, cwd, "git", args).await
}

pub async fn merge_base(cwd: Option<&str>, a: &str, b: &str) -> Result<String, ErrorStatus> {
    match git(cwd, &["merge-base", a, b]).await {
        Ok(stdout) => {
            if stdout.len() >= 40 {
                Ok(stdout[..40].to_string())
            } else {
                Err(ErrorStatus::Errored("Malformed merge-base".to_string()))
            }
        }
        Err(err) => Err(err),
    }
}

pub async fn merge_base_and_timestamp(
    cwd: Option<&str>,
    a: &str,
    b: &str,
) -> Result<(String, i64), ErrorStatus> {
    match merge_base(cwd, a, b).await {
        Err(err) => Err(err),
        Ok(hash) => match git(cwd, &["log", "--format=%ct", "-n1", &hash]).await {
            Ok(stdout) => match stdout.trim().parse::<i64>() {
                Ok(timestamp) => Ok((hash, timestamp)),
                Err(_) => Err(ErrorStatus::Errored(format!(
                    "Failed to convert timestamp {:?}",
                    stdout
                ))),
            },
            Err(err) => Err(err),
        },
    }
}

pub async fn files_changed_since(
    cwd: Option<&str>,
    hash: &str,
) -> Result<Vec<String>, ErrorStatus> {
    match git(cwd, &["diff", "-z", "--name-only", hash]).await {
        Ok(stdout) => Ok(split_null_terminated_lines(&stdout)
            .into_iter()
            .map(|s| s.to_string())
            .collect()),
        Err(err) => Err(err),
    }
}

pub async fn files_changed_since_mergebase_with(
    cwd: Option<&str>,
    commit: &str,
) -> Result<(String, Vec<String>), ErrorStatus> {
    match merge_base(cwd, commit, "HEAD").await {
        Err(err) => Err(err),
        Ok(mergebase) => match files_changed_since(cwd, &mergebase).await {
            Ok(changes) => Ok((mergebase, changes)),
            Err(err) => Err(err),
        },
    }
}
