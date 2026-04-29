/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::path::PathBuf;
use std::process::Command;

use flow_common::options::Options;
use flow_common_vcs::vcs;
use regex::Regex;

use crate::saved_state_fetcher::FetchResult;

fn string_of_vcs_error(vcs_kind: vcs::Vcs, msg: impl ToString) -> String {
    format!("{}: {}", vcs::name(vcs_kind), msg.to_string())
}

fn run(cwd: &PathBuf, prog: &str, args: &[&str]) -> Result<String, String> {
    let output = Command::new(prog)
        .args(args)
        .current_dir(cwd)
        .output()
        .map_err(|err| err.to_string())?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).trim().to_string())
    }
}

fn merge_base_and_timestamp(
    vcs_kind: vcs::Vcs,
    vcs_root: &PathBuf,
) -> Result<(String, i64), String> {
    // TODO: use file_watcher_mergebase_with from flowconfig
    match vcs_kind {
        vcs::Vcs::Hg => {
            let stdout = run(
                vcs_root,
                "hg",
                &["log", "-T", "{node} {date}", "-r", "ancestor(master,.)"],
            )?;
            let mut parts = stdout.split_whitespace();
            let hash = parts
                .next()
                .ok_or_else(|| "Malformed hg log response".to_string())?;
            let timestamp = parts
                .next()
                .ok_or_else(|| "Malformed hg log response".to_string())?
                .parse::<f64>()
                .map_err(|err| err.to_string())? as i64;
            Ok((hash.to_string(), timestamp))
        }
        vcs::Vcs::Git => {
            let hash = run(vcs_root, "git", &["merge-base", "master", "HEAD"])?;
            let timestamp = run(vcs_root, "git", &["show", "-s", "--format=%ct", &hash])?
                .parse::<i64>()
                .map_err(|err| err.to_string())?;
            Ok((hash, timestamp))
        }
    }
}

fn get_changes_since(
    vcs_kind: vcs::Vcs,
    vcs_root: &PathBuf,
    hash: &str,
) -> Result<Vec<String>, String> {
    match vcs_kind {
        vcs::Vcs::Hg => {
            let output = Command::new("hg")
                .args(["status", "--print0", "-n", "--rev", hash])
                .current_dir(vcs_root)
                .output()
                .map_err(|err| err.to_string())?;
            if !output.status.success() {
                return Err(String::from_utf8_lossy(&output.stderr).trim().to_string());
            }
            Ok(output
                .stdout
                .split(|b| *b == 0)
                .filter(|s| !s.is_empty())
                .map(|s| {
                    vcs_root
                        .join(String::from_utf8_lossy(s).as_ref())
                        .to_string_lossy()
                        .into_owned()
                })
                .collect())
        }
        vcs::Vcs::Git => {
            let stdout = run(vcs_root, "git", &["diff", "--name-only", hash])?;
            Ok(stdout
                .lines()
                .filter(|line| !line.is_empty())
                .map(|line| vcs_root.join(line).to_string_lossy().into_owned())
                .collect())
        }
    }
}

fn saved_states_dir(options: &Options, root: &std::path::Path) -> PathBuf {
    let dir = root.join(".flow.saved_states");
    match &options.root_name {
        Some(name) => dir.join(flow_common::string_utils::filename_escape(name.as_str())),
        None => dir,
    }
}

fn saved_states(
    options: &Options,
    root: &std::path::Path,
) -> Option<(PathBuf, Vec<(i64, String, String)>)> {
    let dir = saved_states_dir(options, root);
    let entries = std::fs::read_dir(&dir).ok()?;
    let regex = Regex::new(r"^([0-9]+)_([a-f0-9]+)$").ok()?;
    let mut parsed = Vec::new();
    for entry in entries.flatten() {
        let filename = entry.file_name().to_string_lossy().into_owned();
        if let Some(captures) = regex.captures(&filename) {
            if let (Some(ts_match), Some(commit_match)) = (captures.get(1), captures.get(2)) {
                if let Ok(timestamp) = ts_match.as_str().parse::<i64>() {
                    let commit = commit_match.as_str().to_string();
                    parsed.push((timestamp, commit, filename));
                }
            }
        }
    }
    Some((dir, parsed))
}

fn pick_saved_state(
    options: &Options,
    root: &std::path::Path,
    merge_base: &str,
    timestamp: i64,
) -> Option<(String, PathBuf)> {
    let (dir, mut entries) = saved_states(options, root)?;
    entries.sort_by(|(t1, c1, _), (t2, c2, _)| match t1.cmp(t2) {
        std::cmp::Ordering::Equal => {
            // if two saved states have the same timestamp, then we have to worry
            // about whether one of them is the merge base. normally it doesn't
            // really matter which one ACTUALLY comes first so we just sort the
            // hashes lexographically. but if one of them is the mergebase itself,
            // we want it to be the first one. below, when we binary search for
            // the best saved state, we will pessimistically assume any saved state
            // with the same timestamp but different hash as the mergebase is
            // newer than the mergebase and can't be used.
            if *t1 == timestamp {
                if c1 == merge_base {
                    std::cmp::Ordering::Less
                } else if c2 == merge_base {
                    std::cmp::Ordering::Greater
                } else {
                    c1.cmp(c2)
                }
            } else {
                c1.cmp(c2)
            }
        }
        order => order,
    });
    // TODO: use timestamp to enforce a max age. it can take longer to load
    // from an extremely stale saved state and then incrementally handle a ton
    // of changes.
    let idx = entries
        .iter()
        .enumerate()
        .filter(|(_, (t, c, _))| *t < timestamp || (*t == timestamp && c == merge_base))
        .map(|(i, _)| i)
        .next_back()?;
    let (_, hash, filename) = entries.get(idx)?.clone();
    Some((hash, dir.join(filename)))
}

/* This saved state fetcher loads saved states associated with source control
 * revisions. It selects the most recent saved state based on the public commit
 * hash. The saved states are stored in the .flow.saved_states folder in the
 * project root directory and are indexed by timestamp and commit hash.
 *
 *   /path/to/root/.flow.saved_states/1666400734_faceb00c
 *   /path/to/root/.flow.saved_states/1666400738_deadbeef
 *
 * If the public ancestor (e.g. the mergebase with the main branch) of the
 * currently checked-out commit was committed at 1666400736, then the most
 * recent saved state is 1666400734_faceb00c. Source control will be queried
 * for changes since commit faceb00c, instead of needing a "files_changed"
 * file like the "local" fetcher requires.
 *
 * If multiple commits have the same timestamp, the chosen one is stable but
 * unspecified (currently, it's the first one lexographically, but this is
 * subject to change). However, if one of them is the current mergebase, it
 * is selected.
 *
 * If the [name] option is set in .flowconfig, then saved states should
 * be stored in a correspondingly-named subdirectory of .flow.saved_states.
 * If [name=foo], then saved states should be stored in
 * /path/to/root/.flow.saved_states/foo/TIMESTAMP_HASH
 *
 * How to distribute saved states to clients is left as an exercise for the
 * reader. But the benefit of this fetcher compared to the "local" fetcher
 * is that multiple states can be prepopulated. If you want to be fancy,
 * you could even imagine a virtual file system that fetches the states
 * from a remote source on demand. */
pub fn fetch(options: &Options) -> FetchResult {
    let root = &*options.root;
    let Some((vcs_kind, vcs_root)) = vcs::find_root(None, root) else {
        return FetchResult::Saved_state_error(
            "Unable to detect a source control root: no .git or .hg folder".to_string(),
        );
    };
    let (merge_base, timestamp) = match merge_base_and_timestamp(vcs_kind, &vcs_root) {
        Ok(result) => result,
        Err(msg) => {
            return FetchResult::Saved_state_error(format!(
                "Failed to compute saved state mergebase: {}",
                string_of_vcs_error(vcs_kind, msg)
            ));
        }
    };
    eprintln!("Saved state merge base hash is {:?}", merge_base);
    let Some((saved_state_merge_base, saved_state_file)) =
        pick_saved_state(options, root, &merge_base, timestamp)
    else {
        return FetchResult::No_saved_state;
    };
    match get_changes_since(vcs_kind, &vcs_root, &saved_state_merge_base) {
        Ok(changed_files) => {
            eprintln!("Saved state path is {}", saved_state_file.display());
            eprintln!(
                "{} files changed since saved state was created",
                changed_files.len()
            );
            FetchResult::Saved_state {
                saved_state_filename: saved_state_file,
                changed_files: changed_files.into_iter().collect::<BTreeSet<_>>(),
            }
        }
        Err(msg) => FetchResult::Saved_state_error(format!(
            "Failed to fetch files changed since the saved state: {}",
            string_of_vcs_error(vcs_kind, msg)
        )),
    }
}

pub fn output_filename(options: &Options) -> Result<PathBuf, String> {
    let root = &*options.root;
    let Some((vcs_kind, vcs_root)) = vcs::find_root(None, root) else {
        return Err("Unable to detect a source control root: no .git or .hg folder".to_string());
    };
    let (merge_base, timestamp) = merge_base_and_timestamp(vcs_kind, &vcs_root)
        .map_err(|msg| format!("Failed to compute saved state mergebase: {msg}"))?;
    Ok(saved_states_dir(options, root).join(format!("{timestamp:010}_{merge_base}")))
}
