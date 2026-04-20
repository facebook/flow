/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path;
use std::path::Path;
use std::path::PathBuf;

use flow_common::string_utils;
use md5::Digest;

pub const DEFAULT_FLOWCONFIG_NAME: &str = ".flowconfig";

pub fn default_temp_dir() -> PathBuf {
    std::env::temp_dir().join("flow")
}

fn add_dir_sep(dir: &str) -> String {
    let dir_sep = path::MAIN_SEPARATOR;
    if dir.ends_with(dir_sep) {
        dir.to_string()
    } else {
        format!("{}{}", dir, dir_sep)
    }
}

fn mk_root(flowconfig_name: &str, root: &Path) -> PathBuf {
    if flowconfig_name == DEFAULT_FLOWCONFIG_NAME {
        root.to_path_buf()
    } else {
        root.join(flowconfig_name)
    }
}

fn digest_root_part(root_part: &str, max_len: usize) -> String {
    let len = root_part.len();
    if len <= max_len {
        root_part.to_string()
    } else {
        let prefix = &root_part[..5];
        let suffix = &root_part[len - 5..];
        let digest = format!("{:x}", md5::Md5::digest(root_part.as_bytes()));
        // 5 char prefix + 5 char suffix + 2 underscores
        let max_digest_length = max_len - 12;
        let digest_part = if digest.len() > max_digest_length {
            &digest[..max_digest_length]
        } else {
            &digest
        };
        format!("{}_{}_{}", prefix, digest_part, suffix)
    }
}

fn file_of_root(
    max_root_part_len: Option<usize>,
    extension: &str,
    flowconfig_name: &str,
    tmp_dir: &str,
    root: &Path,
) -> String {
    let tmp_dir = add_dir_sep(tmp_dir);
    let root = mk_root(flowconfig_name, root);
    let root_part = string_utils::filename_escape(&root.to_string_lossy());
    let root_part = match max_root_part_len {
        None => root_part,
        Some(max_root_part_len) => digest_root_part(&root_part, max_root_part_len),
    };
    format!("{}{}.{}", tmp_dir, root_part, extension)
}

pub fn config_file(flowconfig_name: &str, root: &Path) -> String {
    root.join(flowconfig_name).to_string_lossy().into_owned()
}

// Generating really long filenames can hit some limits, which can cause
// ENAMETOOLONG or similar errors. So let's cap our filenames (without
// extensions) at 200 characters.
const MAX_ROOT_PART_LEN: usize = 200;

pub fn log_file(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> String {
    file_of_root(
        Some(MAX_ROOT_PART_LEN),
        "log",
        flowconfig_name,
        tmp_dir,
        root,
    )
}

pub fn dfind_log_file(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> String {
    file_of_root(
        Some(MAX_ROOT_PART_LEN),
        "dfind_log",
        flowconfig_name,
        tmp_dir,
        root,
    )
}

pub fn monitor_log_file(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> String {
    file_of_root(
        Some(MAX_ROOT_PART_LEN),
        "monitor_log",
        flowconfig_name,
        tmp_dir,
        root,
    )
}

pub fn lock_file(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> String {
    file_of_root(
        Some(MAX_ROOT_PART_LEN),
        "lock",
        flowconfig_name,
        tmp_dir,
        root,
    )
}

pub fn pids_file(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> String {
    file_of_root(
        Some(MAX_ROOT_PART_LEN),
        "pids",
        flowconfig_name,
        tmp_dir,
        root,
    )
}

pub fn recheck_stats_file(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> String {
    file_of_root(
        Some(MAX_ROOT_PART_LEN),
        "recheck_stats",
        flowconfig_name,
        tmp_dir,
        root,
    )
}

pub fn ready_file(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> String {
    file_of_root(
        Some(MAX_ROOT_PART_LEN),
        "ready",
        flowconfig_name,
        tmp_dir,
        root,
    )
}

// Socket files don't care about length. socket.ml will worry about abridging those
pub fn socket_file(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> String {
    file_of_root(None, "sockv3", flowconfig_name, tmp_dir, root)
}

pub fn legacy2_socket_file(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> String {
    file_of_root(None, "sockv2", flowconfig_name, tmp_dir, root)
}

pub fn legacy1_socket_file(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> String {
    file_of_root(None, "sock", flowconfig_name, tmp_dir, root)
}
