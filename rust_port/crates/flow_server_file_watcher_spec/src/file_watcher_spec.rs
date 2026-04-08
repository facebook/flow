/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;

use flow_common::files;
use flow_common::files::FileOptions;
use flow_common::options::Options;
use flow_server_files::server_files_js;

pub fn get_suffixes(file_options: &FileOptions) -> Vec<String> {
    let mut exts: Vec<String> = files::get_all_watched_extensions(file_options)
        .into_iter()
        .collect();
    exts.sort();
    let mut all_exts = vec![files::FLOW_EXT.to_string()];
    all_exts.append(&mut exts);
    all_exts
        .into_iter()
        .map(|ext| {
            let fake = format!("foo{}", ext);
            Path::new(&fake)
                .extension()
                .map(|e| e.to_string_lossy().into_owned())
                .unwrap_or_default()
        })
        .map(|ext| {
            if !ext.is_empty() && ext.starts_with('.') {
                ext[1..].to_string()
            } else {
                ext
            }
        })
        .collect()
}

pub fn get_file_names(options: &Options) -> Vec<String> {
    let flowconfig_name = &options.flowconfig_name;
    let flowconfig_path = server_files_js::config_file(flowconfig_name, &options.root);
    let basename = Path::new(&flowconfig_path)
        .file_name()
        .map(|f| f.to_string_lossy().into_owned())
        .unwrap_or_default();
    vec!["package.json".to_string(), basename]
}

pub fn get_include_dirs_absolute(options: &Options) -> Vec<String> {
    let file_options = &options.file_options;
    files::watched_paths(file_options)
        .into_iter()
        .map(|p| p.to_string_lossy().into_owned())
        .collect()
}

pub const EXCLUDE_DIRS: &[&str] = &[".hg", ".git", ".svn"];
