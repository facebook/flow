/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::Path;

use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;

use crate::bitset::Bitset;
use crate::files;
use crate::files::FileOptions;
use crate::flow_projects::FlowProjects;
use crate::flow_projects::ProjectsOptions;

/// Platform set is represented as a bitset where each bit corresponds to a platform
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PlatformSet(Bitset);

impl PlatformSet {
    pub fn is_subset(&self, other: &Self) -> bool {
        self.0.is_subset(&other.0)
    }

    pub fn no_overlap(&self, other: &Self) -> bool {
        self.0.no_overlap(&other.0)
    }

    pub fn to_platform_string_set(&self, file_options: &FileOptions) -> BTreeSet<FlowSmolStr> {
        file_options
            .multi_platform_extensions
            .iter()
            .enumerate()
            .filter_map(|(i, ext)| {
                if self.0.mem(i) {
                    Some(FlowSmolStr::new(ext.strip_prefix('.').unwrap()))
                } else {
                    None
                }
            })
            .collect()
    }
}

fn available_platforms_to_bitset(
    multi_platform_extensions: &[FlowSmolStr],
    available_platforms: &[FlowSmolStr],
) -> PlatformSet {
    let mut platform_set = Bitset::all_zero(multi_platform_extensions.len());
    for (i, ext) in multi_platform_extensions.iter().enumerate() {
        let platform = FlowSmolStr::new(ext.strip_prefix('.').unwrap_or(ext));
        if available_platforms.contains(&platform) {
            platform_set = platform_set.set(i)
        }
    }
    PlatformSet(platform_set)
}

pub fn available_platforms(
    file_options: &FileOptions,
    projects_options: &ProjectsOptions,
    filename: &str,
    explicit_available_platforms: Option<&[FlowSmolStr]>,
) -> Option<PlatformSet> {
    if !file_options.multi_platform {
        return None;
    }
    let multi_platform_extensions = &file_options.multi_platform_extensions;
    if let Some(result) =
        files::platform_specific_extensions_and_indices_opt(file_options, filename)
    {
        let mut platform_set = Bitset::all_zero(multi_platform_extensions.len());
        for (i, _) in result {
            platform_set = platform_set.set(i);
        }
        return Some(PlatformSet(platform_set));
    }

    match explicit_available_platforms {
        Some(explicit_platforms) => Some(available_platforms_to_bitset(
            multi_platform_extensions,
            explicit_platforms,
        )),
        None => {
            if let Some(supported_platforms_from_project_overrides) =
                FlowProjects::from_path(projects_options, filename).and_then(|projects| {
                    projects_options.multi_platform_ambient_supports_platform_for_project(projects)
                })
            {
                Some(available_platforms_to_bitset(
                    multi_platform_extensions,
                    &supported_platforms_from_project_overrides,
                ))
            } else {
                Some(PlatformSet(Bitset::all_one(
                    multi_platform_extensions.len(),
                )))
            }
        }
    }
}

/// Return a partitioned list of platform specific implementation mrefs to check existence,
/// of the form (unconditional_extensions, grouped_extensions_with_conditional_extensions).
///
/// - Every mref in unconditional_extensions must always exist.
/// - grouped_extensions_with_conditional_extensions is a list of (group_ext_mref, platform_ext_mref).
///   We first check if group_ext_mref exists. If that fails then platform_ext_mref must exist.
pub fn platform_specific_implementation_mrefs_of_possibly_interface_file(
    file_options: &FileOptions,
    platform_set: Option<&PlatformSet>,
    file: &FileKey,
) -> Option<(Vec<String>, Vec<(String, Vec<String>)>)> {
    if !file_options.multi_platform || !files::has_flow_ext(file) {
        return None;
    }
    let file = files::chop_flow_ext(file);
    let platform_set = platform_set.unwrap();
    file_options
        .module_file_exts
        .iter()
        .find_map(|module_file_ext| {
            if !file.check_suffix(module_file_ext) {
                return None;
            }
            let file_without_module_file_ext = file.chop_suffix(module_file_ext);
            let base = Path::new(file_without_module_file_ext.as_str())
                .file_name()
                .unwrap()
                .to_str()
                .unwrap();
            let platform_extensions: Vec<&FlowSmolStr> = file_options
                .multi_platform_extensions
                .iter()
                .enumerate()
                .filter_map(|(i, platform_ext)| {
                    if platform_set.0.mem(i) {
                        Some(platform_ext)
                    } else {
                        None
                    }
                })
                .collect();

            let mut unconditional_extensions: BTreeSet<&FlowSmolStr> = BTreeSet::new();
            let mut grouped_extensions_with_conditional_extensions: BTreeMap<
                &FlowSmolStr,
                BTreeSet<&FlowSmolStr>,
            > = BTreeMap::new();
            for ext in platform_extensions {
                match file_options
                    .multi_platform_extension_group_mapping
                    .iter()
                    .find(|(_, platforms)| {
                        platforms
                            .iter()
                            .any(|p| Some(p.as_str()) == ext.strip_prefix("."))
                    }) {
                    None => {
                        unconditional_extensions.insert(ext);
                    }
                    Some((group_ext, _)) => {
                        grouped_extensions_with_conditional_extensions
                            .entry(group_ext)
                            .or_insert_with(BTreeSet::new)
                            .insert(ext);
                    }
                }
            }

            let implementation_mref_of_platform_extension =
                |platform_ext: &str| format!("./{}{}", base, platform_ext);
            let unconditional_list: Vec<String> = unconditional_extensions
                .into_iter()
                .map(|ext| implementation_mref_of_platform_extension(ext))
                .collect();

            let grouped_list: Vec<(String, Vec<String>)> =
                grouped_extensions_with_conditional_extensions
                    .into_iter()
                    .map(|(group_ext, conditional_exts)| {
                        let group_mref = implementation_mref_of_platform_extension(group_ext);
                        let conditional_mrefs: Vec<String> = conditional_exts
                            .into_iter()
                            .map(|ext| implementation_mref_of_platform_extension(ext))
                            .collect();
                        (group_mref, conditional_mrefs)
                    })
                    .collect();

            Some((unconditional_list, grouped_list))
        })
}
