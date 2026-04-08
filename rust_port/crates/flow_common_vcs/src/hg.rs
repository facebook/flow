/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::vcs_utils::Env;
pub use crate::vcs_utils::ErrorStatus;
use crate::vcs_utils::exec;
use crate::vcs_utils::split_null_terminated_lines;

async fn hg(cwd: Option<&str>, args: &[&str]) -> Result<String, ErrorStatus> {
    let env = Env::Extend(vec![
        ("NOSCMLOG".to_string(), "1".to_string()),
        ("HGPLAIN".to_string(), "1".to_string()),
    ]);
    exec(Some(env), cwd, "hg", args).await
}

pub async fn merge_base(cwd: Option<&str>, a: &str, b: &str) -> Result<String, ErrorStatus> {
    let revset = format!("ancestor({},{})", a, b);
    match hg(cwd, &["log", "-T", "{node}", "-r", &revset]).await {
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
    let revset = format!("ancestor({},{})", a, b);
    match hg(cwd, &["log", "-T", "{node} {date}", "-r", &revset]).await {
        Ok(stdout) => {
            let parts: Vec<&str> = stdout.split(' ').collect();
            match parts.as_slice() {
                [hash, timestamp] => match timestamp.parse::<f64>() {
                    Ok(ts) => Ok((hash.to_string(), ts as i64)),
                    Err(_) => Err(ErrorStatus::Errored("Malformed log response".to_string())),
                },
                _ => Err(ErrorStatus::Errored("Malformed log response".to_string())),
            }
        }
        Err(err) => Err(err),
    }
}

pub async fn files_changed_since(
    cwd: Option<&str>,
    hash: &str,
) -> Result<Vec<String>, ErrorStatus> {
    match hg(cwd, &["status", "--print0", "-n", "--rev", hash]).await {
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
    match merge_base(cwd, ".", commit).await {
        Err(err) => Err(err),
        Ok(mergebase) => match files_changed_since(cwd, &mergebase).await {
            Ok(changes) => Ok((mergebase, changes)),
            Err(err) => Err(err),
        },
    }
}
