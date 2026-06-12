/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use regex::Regex;
use vec1::Vec1;

use crate::bitset::Bitset;

#[derive(Debug, Clone)]
pub struct ProjectsOptions {
    pub projects: Vec1<FlowSmolStr>,
    pub projects_overlap_mapping: BTreeMap<usize, Bitset>,
    pub projects_path_mapping: Vec<(Regex, Bitset)>,
    pub projects_strict_boundary: bool,
    pub multi_platform_ambient_supports_platform_project_overrides: Vec<(Bitset, Vec<FlowSmolStr>)>,
}

impl Default for ProjectsOptions {
    fn default() -> Self {
        Self {
            projects: Vec1::new(FlowSmolStr::new_inline("default")),
            projects_overlap_mapping: BTreeMap::new(),
            projects_path_mapping: vec![],
            projects_strict_boundary: false,
            multi_platform_ambient_supports_platform_project_overrides: vec![],
        }
    }
}

fn index_of(projects: &Vec1<FlowSmolStr>, name: &str) -> usize {
    projects.iter().position(|n| n == name).unwrap()
}

impl ProjectsOptions {
    #[allow(clippy::too_many_arguments)]
    pub fn mk(
        projects: Vec1<FlowSmolStr>,
        projects_overlap_mapping: BTreeMap<FlowSmolStr, Vec<FlowSmolStr>>,
        map_path: impl Fn(String) -> Regex,
        projects_path_mapping: Vec<(String, Vec<FlowSmolStr>)>,
        projects_strict_boundary: bool,
        multi_platform_ambient_supports_platform_project_overrides: Vec<(
            FlowSmolStr,
            Vec<FlowSmolStr>,
        )>,
    ) -> Self {
        fn list_to_bitset(projects: &Vec1<FlowSmolStr>, names: &[FlowSmolStr]) -> Bitset {
            let mut bitset = Bitset::all_zero(projects.len());
            for name in names {
                bitset = bitset.set(index_of(projects, name));
            }
            bitset
        }

        let projects_overlap_mapping: BTreeMap<usize, Bitset> = projects_overlap_mapping
            .into_iter()
            .map(|(k, ns)| (index_of(&projects, &k), list_to_bitset(&projects, &ns)))
            .collect();

        let projects_path_mapping = projects_path_mapping
            .into_iter()
            .map(|(path, ns)| (map_path(path), list_to_bitset(&projects, &ns)))
            .collect();

        let multi_platform_ambient_supports_platform_project_overrides = {
            // Given config
            // ```
            // experimental.multi_platform.ambient_supports_platform.project_overrides='web_project' -> 'web'
            // experimental.multi_platform.ambient_supports_platform.project_overrides='native_project' -> 'native'
            // ```
            // and knowing that web project and native project can overlap,
            // we want to generate a mapping of
            //
            // {web_project: [web], native_project: [native], web_project+native_project: [web, native]}
            let size = projects.len();

            let single_project_overrides: Vec<(usize, Bitset, Vec<FlowSmolStr>)> =
                multi_platform_ambient_supports_platform_project_overrides
                    .iter()
                    .map(|(project_str, platforms)| {
                        let i = index_of(&projects, project_str);
                        let project = Bitset::all_zero(size).set(i);
                        (i, project, platforms.clone())
                    })
                    .collect();

            let mut composite_overrides: Vec<(Bitset, Vec<FlowSmolStr>)> = vec![];

            for (i, _project, platforms) in single_project_overrides.iter().rev() {
                if let Some(composite_project) = projects_overlap_mapping.get(i) {
                    if let Some(idx) = composite_overrides
                        .iter()
                        .position(|(p, _)| p == composite_project)
                    {
                        composite_overrides[idx].1.extend(platforms.clone());
                    } else {
                        composite_overrides.push((*composite_project, platforms.clone()));
                    }
                }
            }
            single_project_overrides
                .into_iter()
                .map(|(_i, project, platforms)| (project, platforms))
                .chain(composite_overrides)
                .collect()
        };

        Self {
            projects,
            projects_overlap_mapping,
            projects_path_mapping,
            projects_strict_boundary,
            multi_platform_ambient_supports_platform_project_overrides,
        }
    }

    pub fn is_common_code_path(&self, path: &str) -> bool {
        if let Some(projects_bitset) = FlowProjects::from_path(self, path) {
            self.projects_overlap_mapping
                .values()
                .any(|common_project_bitset| common_project_bitset == &projects_bitset.0)
        } else {
            false
        }
    }

    pub fn multi_platform_ambient_supports_platform_for_project(
        &self,
        p: FlowProjects,
    ) -> Option<Vec<FlowSmolStr>> {
        let p = if self.projects_strict_boundary {
            p
        } else if self.projects_overlap_mapping.values().any(|b| *b == p.0) {
            let size = self.projects.len();
            self.projects
                .iter()
                .enumerate()
                .find_map(|(i, _)| {
                    if p.0.mem(i) {
                        Some(FlowProjects(Bitset::all_zero(size).set(i)))
                    } else {
                        None
                    }
                })
                .unwrap_or(p)
        } else {
            p
        };

        self.multi_platform_ambient_supports_platform_project_overrides
            .iter()
            .find_map(|(project, platforms)| {
                if *project == p.0 {
                    Some(platforms.clone()) // clone needed: Vec doesn't implement Dupe
                } else {
                    None
                }
            })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FlowProjects(Bitset);

impl FlowProjects {
    pub fn from_bitset_unchecked(v: Bitset) -> Self {
        Self(v)
    }

    pub fn to_bitset(self) -> Bitset {
        self.0
    }

    pub fn from_project_str(opts: &ProjectsOptions, project: &str) -> Self {
        Self(Bitset::all_zero(opts.projects.len()).set(index_of(&opts.projects, project)))
    }

    pub fn from_path(opts: &ProjectsOptions, path: &str) -> Option<Self> {
        if opts.projects.len() == 1 {
            Some(Self(Bitset::all_one(1)))
        } else {
            let normalized_path = path.replace('\\', "/");
            opts.projects_path_mapping
                .iter()
                .find(|(r, _)| r.is_match(&normalized_path))
                .map(|(_, bitset)| Self(*bitset))
        }
    }
}
