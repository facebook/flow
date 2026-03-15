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
    pub projects_strict_boundary_validate_import_pattern_opt_outs: bool,
    pub projects_strict_boundary_import_pattern_opt_outs: Vec<Regex>,
    pub multi_platform_ambient_supports_platform_project_overrides: Vec<(Bitset, Vec<FlowSmolStr>)>,
}

impl Default for ProjectsOptions {
    fn default() -> Self {
        Self {
            projects: Vec1::new(FlowSmolStr::new_inline("default")),
            projects_overlap_mapping: BTreeMap::new(),
            projects_path_mapping: vec![],
            projects_strict_boundary: false,
            projects_strict_boundary_validate_import_pattern_opt_outs: false,
            projects_strict_boundary_import_pattern_opt_outs: vec![],
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
        projects_strict_boundary_validate_import_pattern_opt_outs: bool,
        projects_strict_boundary_import_pattern_opt_outs: Vec<Regex>,
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
            projects_strict_boundary_validate_import_pattern_opt_outs,
            projects_strict_boundary_import_pattern_opt_outs,
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

    pub fn projects_strict_boundary_validate_import_pattern_opt_outs(&self) -> bool {
        self.projects_strict_boundary_validate_import_pattern_opt_outs
    }

    pub fn is_import_specifier_that_opt_out_of_strict_boundary(
        &self,
        import_specifier: &str,
    ) -> bool {
        self.projects_strict_boundary_import_pattern_opt_outs
            .iter()
            .any(|pattern| pattern.is_match(import_specifier))
    }

    /// Suppose we have web and native project, and some paths that can be part of both web and native.
    /// Then this function will return which projects' files can be accessed by the given project.
    ///
    /// This is used to enforce that web code can use both web and web+native code, while web+native code
    /// can only import web+native code. However, the latter is temporarily allowed for experimentation.
    pub fn reachable_projects_bitsets_from_projects_bitset(
        &self,
        import_specifier: &str,
        p: FlowProjects,
    ) -> Vec<FlowProjects> {
        let size = self.projects.len();

        // 1-project code can reach into common code.
        // e.g. Suppose that we have two projects web and native.
        // Web code can use common code (web+native). *)
        let additional_from_common_code = self.projects.iter().enumerate().find_map(|(i, _)| {
            let single_project = Bitset::all_zero(size).set(i);
            if p.0 == single_project {
                self.projects_overlap_mapping
                    .get(&i)
                    .map(|p| FlowProjects(*p))
            } else {
                None
            }
        });

        // Normally, we do not allow common code importing one-project code. However, when we decide
        // to allow it for compatibility purposes, we will pick the one project declared first in flowconfig.
        let one_project_reachable_from_common_code = || -> Option<FlowProjects> {
            if self.projects_overlap_mapping.values().any(|b| *b == p.0) {
                self.projects.iter().enumerate().find_map(|(i, _)| {
                    if p.0.mem(i) {
                        Some(FlowProjects(Bitset::all_zero(size).set(i)))
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        };

        // Unlike the one below, these imports from common code into 1-project code is explicitly allowed.
        // To maintain sanity, we will ensure that the files from different namespaces will have the same
        // signature as the chosen one here.
        let additional_from_1_project_code_allowed_with_strict_boundary_import_pattern_opt_outs =
            match (
                additional_from_common_code,
                self.is_import_specifier_that_opt_out_of_strict_boundary(import_specifier),
            ) {
                (Some(_), _) | (_, false) => None,
                (None, true) => one_project_reachable_from_common_code(),
            };

        let additional_from_1_project_code_unsafe = if self.projects_strict_boundary {
            None
        } else {
            // Temporary hack: common code can reach into 1-project code.
            // e.g. Suppose that we have two projects web and native.
            // We temporarily allow common code (web+native) to use web-only code.
            // This is of course incorrect, and we should move these web-only code into common code instead.
            // However, the temporary measure exists so that we can still have good type coverage during
            // experimentation before we can lock down the boundary.
            match (
                additional_from_common_code,
                additional_from_1_project_code_allowed_with_strict_boundary_import_pattern_opt_outs,
            ) {
                (Some(_), _) | (_, Some(_)) => None,
                (None, None) => one_project_reachable_from_common_code(),
            }
        };

        let mut result = vec![p];
        if let Some(p) = additional_from_common_code {
            result.push(p);
        }
        if let Some(p) =
            additional_from_1_project_code_allowed_with_strict_boundary_import_pattern_opt_outs
        {
            result.push(p);
        }
        if let Some(p) = additional_from_1_project_code_unsafe {
            result.push(p);
        }
        result
    }

    /// Suppose we have web and native project, and some paths that can be part of both web and native.
    /// Then this function will always return the bitset representation of web,native projects, if and
    /// only if it's given a web+native project.
    ///
    /// This is important to enforce in Haste that we have only one provider for a module name `N`. The
    /// module name might come from multiple projects, so we have to search for files that might provide
    /// all of the following haste_module_info: `web:N`, `native:N` to ensure that there is no module
    /// that will also provide `N` that's already provided in the common code.
    pub fn individual_projects_bitsets_from_common_project_bitset(
        &self,
        common: FlowProjects,
    ) -> Option<Vec<FlowProjects>> {
        let size = self.projects.len();

        if self
            .projects_overlap_mapping
            .values()
            .any(|b| *b == common.0)
        {
            Some(
                self.projects
                    .iter()
                    .enumerate()
                    .filter_map(|(i, _)| {
                        if common.0.mem(i) {
                            Some(FlowProjects(Bitset::all_zero(size).set(i)))
                        } else {
                            None
                        }
                    })
                    .collect(),
            )
        } else {
            None
        }
    }

    /// Same as above, but drop the first project.
    /// This is useful for projects_strict_boundary_import_pattern_opt_outs, where we pick the file from
    /// the first project to act as common interface, but then we validate that all the other corresponding
    /// files in other projects conform to the interface of the first file.
    pub fn individual_projects_bitsets_from_common_project_bitset_excluding_first(
        &self,
        common: FlowProjects,
    ) -> Option<Vec<FlowProjects>> {
        self.individual_projects_bitsets_from_common_project_bitset(common)
            .and_then(|mut projects| {
                if projects.is_empty() {
                    None
                } else {
                    projects.remove(0);
                    Some(projects)
                }
            })
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
