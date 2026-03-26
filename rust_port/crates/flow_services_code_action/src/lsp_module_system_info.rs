/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_common::files::FileOptions;
use flow_common_modulename::HasteModuleInfo;
use flow_parser::file_key::FileKey;
use flow_parser_utils::package_json::PackageJson;

pub struct LspModuleSystemInfo {
    pub file_options: Arc<FileOptions>,
    pub haste_module_system: bool,
    pub get_haste_module_info: Box<dyn Fn(&FileKey) -> Option<HasteModuleInfo>>,
    pub get_package_info: Box<dyn Fn(&FileKey) -> Option<Result<PackageJson, ()>>>,
    pub is_package_file: Box<dyn Fn(&str, &str) -> bool>,
    pub node_resolver_root_relative_dirnames: Vec<(Option<String>, String)>,
    pub resolves_to_real_path: Box<dyn Fn(&str, &str) -> bool>,
}
