/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs;
use std::path::Path;
use std::path::PathBuf;

use flow_common::sys_utils;

mod flowlib_contents;
mod prelude_contents;

fn hash(no_flowlib: bool) -> String {
    if no_flowlib {
        prelude_contents::HASH.to_string()
    } else {
        flowlib_contents::HASH.to_string()
    }
}

fn contents(no_flowlib: bool) -> &'static [(&'static str, &'static str)] {
    if no_flowlib {
        prelude_contents::CONTENTS
    } else {
        flowlib_contents::CONTENTS
    }
}

pub fn contents_list(no_flowlib: bool) -> Vec<(&'static str, &'static str)> {
    contents(no_flowlib).to_vec()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LibDir {
    Flowlib(PathBuf),
    Prelude(PathBuf),
}

#[cfg(unix)]
fn get_euid() -> u32 {
    unsafe { libc::geteuid() }
}

#[cfg(not(unix))]
fn get_euid() -> u32 {
    0
}

/// [libdir ~no_flowlib parent_dir] returns the directory under [parent_dir]
/// within which the flowlib files will be extracted. This directory is
/// named uniquely based on the flowlib contents, as well as the effective
/// user ID (euid) of the current process. The euid is used to ensure that
/// the directory is writable by the current user.
pub fn libdir(no_flowlib: bool, parent_dir: &Path) -> LibDir {
    let euid = get_euid();
    let basename = format!("flowlib_{}_{}", hash(no_flowlib), euid);
    let path = parent_dir.join(&basename);
    if no_flowlib {
        LibDir::Prelude(path)
    } else {
        LibDir::Flowlib(path)
    }
}

pub fn path_of_libdir(libdir: &LibDir) -> &Path {
    match libdir {
        LibDir::Prelude(path) => path,
        LibDir::Flowlib(path) => path,
    }
}

fn mkdir(libdir: &LibDir) {
    let path = path_of_libdir(libdir);
    let parent_dir = path.parent().expect("libdir path should have a parent");
    sys_utils::mkdir_no_fail(parent_dir)
        .unwrap_or_else(|e| panic!("mkdir_no_fail({:?}): {}", parent_dir, e));
    sys_utils::mkdir_no_fail(path).unwrap_or_else(|e| panic!("mkdir_no_fail({:?}): {}", path, e));
}

fn write_flowlib(dir: &Path, (filename, contents): &(&str, &str)) {
    let file = dir.join(filename);
    fs::write(&file, contents).expect("failed to write flowlib file");
}

pub fn extract(libdir: &LibDir) {
    mkdir(libdir);
    let (path, no_flowlib) = match libdir {
        LibDir::Prelude(path) => (path.as_path(), true),
        LibDir::Flowlib(path) => (path.as_path(), false),
    };
    for entry in contents(no_flowlib) {
        write_flowlib(path, entry);
    }
}

pub fn extract_if_missing(libdir: &LibDir) {
    let sentinel_name = match libdir {
        LibDir::Flowlib(_) => "core.js",
        LibDir::Prelude(_) => "prelude.js",
    };
    let libdir_path = path_of_libdir(libdir);
    let sentinel = libdir_path.join(sentinel_name);
    if !sentinel.exists() {
        extract(libdir);
    }
}

fn libdir_from_files_libdir(files_libdir: &flow_common::files::LibDir) -> LibDir {
    match files_libdir {
        flow_common::files::LibDir::Prelude(path) => LibDir::Prelude(path.clone()),
        flow_common::files::LibDir::Flowlib(path) => LibDir::Flowlib(path.clone()),
    }
}

pub fn extract_if_missing_or_exit(files_libdir_opt: Option<&flow_common::files::LibDir>) {
    let Some(files_libdir) = files_libdir_opt else {
        return;
    };
    let libdir = libdir_from_files_libdir(files_libdir);
    match std::panic::catch_unwind(|| extract_if_missing(&libdir)) {
        Ok(()) => {}
        Err(err) => {
            let panic_msg = err
                .downcast_ref::<&str>()
                .copied()
                .or_else(|| err.downcast_ref::<String>().map(|s| s.as_str()))
                .unwrap_or("unknown error");
            let libdir_path = path_of_libdir(&libdir);
            eprintln!(
                "Could not extract flowlib files into {}: {}",
                libdir_path.display(),
                panic_msg
            );
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::CouldNotExtractFlowlibs,
            )
        }
    }
}
