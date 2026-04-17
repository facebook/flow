/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;

use flow_aloc::ALoc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_lint_settings::lint_settings::LintSettings;
use flow_lint_settings::severity::Severity;
use flow_lint_settings::strict_mode_settings::StrictModeSettings;
use regex::Regex;

use crate::files::FileOptions;
use crate::flow_projects::ProjectsOptions;
use crate::slow_to_check_logging::SlowToCheckLogging;
use crate::verbose::Verbose;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ModuleSystem {
    #[default]
    Node,
    Haste,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SupportedOs {
    CentOS,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsxMode {
    /// JSX desugars into a `React.createElement(name, props, ...children)` call
    JsxReact,
    /// Specifies a function that should be invoked instead of React.createElement
    /// when interpreting JSX syntax. Otherwise, the usual rules of JSX are
    /// followed: children are varargs after a props argument.
    JsxPragma(String, flow_parser::ast::expression::Expression<ALoc, ALoc>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SavedStateFetcher {
    #[default]
    DummyFetcher,
    LocalFetcher,
    ScmFetcher,
    FbFetcher,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ReactRefAsProp {
    /// Only in component syntax: ban ref prop in spread
    #[default]
    Legacy,
    /// Implement full React 19 behavior
    FullSupport,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ReactRuntime {
    #[default]
    Automatic,
    Classic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReactRule {
    ValidateRefAccessDuringRender,
    DeepReadOnlyProps,
    DeepReadOnlyHookReturns,
    RulesOfHooks,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AssertOperator {
    Enabled,
    Specialized,
    #[default]
    Disabled,
    Unparsed,
}

impl AssertOperator {
    /// Returns true if the operator should be parsed
    pub fn parse(self) -> bool {
        match self {
            AssertOperator::Unparsed => false,
            AssertOperator::Enabled | AssertOperator::Specialized | AssertOperator::Disabled => {
                true
            }
        }
    }

    /// Returns true if the operator is usable
    pub fn usable(self) -> bool {
        match self {
            AssertOperator::Unparsed | AssertOperator::Disabled => false,
            AssertOperator::Enabled | AssertOperator::Specialized => true,
        }
    }

    /// Returns true if the operator is specialized
    pub fn specialized(self) -> bool {
        matches!(self, AssertOperator::Specialized)
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    serde::Serialize,
    serde::Deserialize
)]
pub enum CastingSyntax {
    #[default]
    As,
    Both,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ChannelMode {
    #[default]
    Pipe,
    Socket,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Format {
    pub bracket_spacing: bool,
    pub single_quotes: bool,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct GcControl {
    pub minor_heap_size: Option<u32>,
    pub major_heap_increment: Option<u32>,
    pub space_overhead: Option<u32>,
    pub window_size: Option<u32>,
    pub custom_major_ratio: Option<u32>,
    pub custom_minor_ratio: Option<u32>,
    pub custom_minor_max_size: Option<u32>,
}

#[derive(Debug, Clone, Copy)]
pub struct LogSaving {
    pub threshold_time_ms: i64,
    pub limit: Option<u64>,
    pub rate: f64,
}

/// Main options structure containing all Flow configuration
#[derive(Debug, Clone, Default)]
pub struct Options {
    pub abstract_classes: bool,
    pub all: bool,
    pub assert_operator: AssertOperator,
    pub autoimports: bool,
    pub autoimports_min_characters: i32,
    pub autoimports_ranked_by_usage: bool,
    pub autoimports_ranked_by_usage_boost_exact_match_min_length: i32,
    pub automatic_require_default: bool,
    pub babel_loose_array_spread: bool,
    pub ban_spread_key_props: bool,
    pub casting_syntax: CastingSyntax,
    pub casting_syntax_only_support_as_excludes: Arc<[Regex]>,
    pub channel_mode: ChannelMode,
    pub component_syntax: bool,
    pub async_component_syntax: bool,
    pub async_component_syntax_includes: Arc<[Regex]>,
    pub debug: bool,
    pub deprecated_utilities: Arc<BTreeMap<String, Vec<String>>>,
    pub deprecated_utilities_excludes: Arc<[Regex]>,
    pub dev_only_refinement_info_as_errors: bool,
    pub distributed: bool,
    pub enable_const_params: bool,
    pub enable_custom_error: bool,
    pub enable_jest_integration: bool,
    pub enable_pattern_matching: bool,
    pub enable_pattern_matching_instance_patterns: bool,
    pub enable_records: bool,
    pub enable_relay_integration: bool,
    pub enabled_rollouts: Arc<BTreeMap<String, String>>,
    pub enums: bool,
    pub estimate_recheck_time: bool,
    pub exact_by_default: bool,
    pub facebook_fbs: Option<FlowSmolStr>,
    pub facebook_fbt: Option<FlowSmolStr>,
    pub facebook_module_interop: bool,
    pub file_options: Arc<FileOptions>,
    pub flowconfig_hash: FlowSmolStr,
    pub flowconfig_name: FlowSmolStr,
    pub format: Format,
    pub gc_worker: GcControl,
    pub haste_module_ref_prefix: Option<FlowSmolStr>,
    pub hook_compatibility: bool,
    pub hook_compatibility_excludes: Arc<[Regex]>,
    pub hook_compatibility_includes: Arc<[Regex]>,
    pub ignore_non_literal_requires: bool,
    pub include_suppressions: bool,
    pub include_warnings: bool,
    pub instance_t_objkit_fix: bool,
    pub lazy_mode: bool,
    pub llm_context_include_imports: bool,
    pub lint_severities: LintSettings<Severity>,
    pub log_file: Arc<PathBuf>,
    pub log_saving: Arc<BTreeMap<String, LogSaving>>,
    pub long_lived_workers: bool,
    pub max_files_checked_per_worker: i32,
    pub max_header_tokens: i32,
    pub max_seconds_for_check_per_worker: f64,
    pub max_workers: i32,
    pub merge_timeout: Option<f64>,
    pub missing_module_generators: Arc<[(Regex, String)]>,
    pub module_system: ModuleSystem,
    pub module_name_mappers: Arc<[(Regex, String)]>,
    pub modules_are_use_strict: bool,
    pub munge_underscores: bool,
    pub no_unchecked_indexed_access: bool,
    pub node_modules_errors: bool,
    pub node_main_fields: Arc<[String]>,
    pub node_package_export_conditions: Arc<[String]>,
    pub node_resolver_allow_root_relative: bool,
    pub node_resolver_root_relative_dirnames: Arc<[(Option<String>, String)]>,
    pub opaque_type_new_bound_syntax: bool,
    pub profile: bool,
    pub projects_options: Arc<ProjectsOptions>,
    pub quiet: bool,
    pub records_includes: Arc<[String]>,
    pub react_custom_jsx_typing: bool,
    pub react_ref_as_prop: ReactRefAsProp,
    pub react_rules: Arc<[ReactRule]>,
    pub react_runtime: ReactRuntime,
    pub recursion_limit: i32,
    pub relay_integration_esmodules: bool,
    pub relay_integration_excludes: Arc<[Regex]>,
    pub relay_integration_module_prefix: Option<FlowSmolStr>,
    pub relay_integration_module_prefix_includes: Arc<[Regex]>,
    pub root: Arc<PathBuf>,
    pub root_name: Option<FlowSmolStr>,
    pub saved_state_direct_serialization: bool,
    pub saved_state_parallel_decompress: bool,
    pub saved_state_fetcher: SavedStateFetcher,
    pub saved_state_force_recheck: bool,
    pub saved_state_no_fallback: bool,
    pub saved_state_persist_export_index: bool,
    pub saved_state_reinit_on_lib_change: bool,
    pub saved_state_skip_version_check: bool,
    pub saved_state_verify: bool,
    pub slow_to_check_logging: SlowToCheckLogging,
    pub strict_es6_import_export: bool,
    pub strict_mode: StrictModeSettings,
    pub strip_root: bool,
    pub supported_operating_systems: Vec<SupportedOs>,
    pub stylex_shorthand_prop: Option<String>,
    pub temp_dir: FlowSmolStr,
    pub ts_syntax: bool,
    pub allow_readonly_variance: bool,
    pub tslib_syntax: bool,
    pub typescript_library_definition_support: bool,
    pub deprecated_colon_extends: Arc<[String]>,
    pub deprecated_colon_extends_excludes: Arc<[Regex]>,
    pub ts_utility_syntax: bool,
    pub type_expansion_recursion_limit: i32,
    pub unsuppressable_error_codes: Arc<HashSet<FlowSmolStr>>,
    pub use_unknown_in_catch_variables: bool,
    pub verbose: Option<Arc<Verbose>>,
    pub vpn_less: bool,
    pub wait_for_recheck: bool,
}

impl Options {
    /// Checks if hook compatibility is enabled for a given file.
    /// Returns true if the file path matches any include pattern and does not match any exclude pattern.
    pub fn hook_compatibility_in_file(&self, file: &flow_parser::file_key::FileKey) -> bool {
        let path = file.to_absolute();
        let included = if self.hook_compatibility_includes.is_empty() {
            false
        } else {
            self.hook_compatibility_includes
                .iter()
                .any(|r| r.is_match(&path))
        };
        let excluded = if self.hook_compatibility_excludes.is_empty() {
            false
        } else {
            self.hook_compatibility_excludes
                .iter()
                .any(|r| r.is_match(&path))
        };
        included || (self.hook_compatibility && !excluded)
    }
}
