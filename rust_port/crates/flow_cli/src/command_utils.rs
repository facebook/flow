/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::io::BufRead;
use std::path::Path;
use std::sync::Arc;

use flow_common::options;
use flow_common::options::CastingSyntax;
use flow_common::options::Format;
use flow_common::options::GcControl;
use flow_common::options::LogSaving;
use flow_common::options::Options;
use flow_common::options::SavedStateFetcher;
use flow_common::path_matcher::PathMatcher;
use flow_common_exit_status::FlowExitStatus;
use flow_config::FlowConfig;
use flow_config::LazyMode;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_flowlib::LibDir as FlowlibDir;
use regex::Regex;

use crate::command_spec;
use crate::command_spec::Command;

fn remove_exclusion(pattern: &str) -> &str {
    pattern.strip_prefix('!').unwrap_or(pattern)
}

fn expand_and_make_regexp(root: &std::path::Path, pattern: &str) -> Regex {
    let pattern = remove_exclusion(pattern);
    let expanded = flow_common::files::expand_project_root_token(root, pattern);
    Regex::new(&expanded).unwrap()
}

fn expand_and_make_path(root: &std::path::Path, path: &str) -> std::path::PathBuf {
    let expanded = flow_common::files::expand_project_root_token(root, path);
    flow_common::files::make_path_absolute(root, &expanded)
}

fn make_includes(
    root: &std::path::Path,
    files_implicitly_include_root: bool,
    lib_paths: &[(Option<String>, std::path::PathBuf)],
    includes: Vec<String>,
) -> PathMatcher {
    let mut matcher = PathMatcher::empty();
    for include in includes {
        let expanded = flow_common::files::expand_project_root_token(root, &include);
        matcher.add_path(&flow_common::files::make_path_absolute(root, &expanded));
    }
    let mut implicit_paths = Vec::new();
    if files_implicitly_include_root {
        implicit_paths.push(root.to_path_buf());
    }
    implicit_paths.extend(lib_paths.iter().map(|(_, path)| path.clone()));
    implicit_paths.sort_by_key(|path| path.to_string_lossy().len());
    for path in implicit_paths {
        let path_str = path.to_string_lossy().to_string();
        if matcher.matches(&path_str) {
            continue;
        }
        matcher.add_path(&path);
    }
    matcher
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct MakeOptionsOverrides {
    pub(crate) autoimports: Option<bool>,
    pub(crate) lazy_mode: Option<bool>,
    pub(crate) saved_state_fetcher: Option<SavedStateFetcher>,
    pub(crate) saved_state_force_recheck: Option<bool>,
    pub(crate) saved_state_no_fallback: Option<bool>,
    pub(crate) saved_state_skip_version_check: Option<bool>,
    pub(crate) saved_state_verify: Option<bool>,
}

pub(super) fn make_options(
    flowconfig: FlowConfig,
    flowconfig_hash: String,
    flowconfig_name: String,
    root: std::path::PathBuf,
    temp_dir: String,
    cli_no_flowlib: bool,
    overrides: MakeOptionsOverrides,
) -> Options {
    let FlowConfig {
        rollouts,
        ignores,
        untyped,
        declarations,
        includes,
        libs,
        lint_severities,
        strict_mode,
        options:
            flow_config::opts::Opts {
                abstract_classes,
                all,
                autoimports,
                autoimports_min_characters,
                autoimports_ranked_by_usage,
                autoimports_ranked_by_usage_boost_exact_match_min_length,
                automatic_require_default,
                babel_loose_array_spread,
                ban_spread_key_props,
                casting_syntax,
                casting_syntax_only_support_as_excludes,
                channel_mode,
                component_syntax,
                async_component_syntax,
                dev_only_refinement_info_as_errors,
                emoji: _emoji,
                enable_const_params,
                enums,
                estimate_recheck_time,
                exact_by_default,
                facebook_fbs,
                facebook_fbt,
                facebook_module_interop,
                file_watcher: _file_watcher,
                file_watcher_mergebase_with: _file_watcher_mergebase_with,
                file_watcher_mergebase_with_git: _file_watcher_mergebase_with_git,
                file_watcher_mergebase_with_hg: _file_watcher_mergebase_with_hg,
                file_watcher_timeout: _file_watcher_timeout,
                files_implicitly_include_root,
                format_bracket_spacing,
                format_single_quotes,
                gc_worker_custom_major_ratio,
                gc_worker_custom_minor_max_size,
                gc_worker_custom_minor_ratio,
                gc_worker_major_heap_increment,
                gc_worker_minor_heap_size,
                gc_worker_space_overhead,
                gc_worker_window_size,
                haste_module_ref_prefix,
                haste_paths_excludes,
                haste_paths_includes,
                hook_compatibility,
                hook_compatibility_includes,
                hook_compatibility_excludes,
                ignore_non_literal_requires,
                instance_t_objkit_fix,
                include_warnings,
                jest_integration,
                lazy_mode,
                log_saving,
                long_lived_workers,
                max_files_checked_per_worker,
                max_files_checked_per_worker_rust_port,
                max_header_tokens,
                max_seconds_for_check_per_worker,
                max_workers,
                merge_timeout,
                missing_module_generators,
                module_declaration_dirnames,
                mut module_file_exts,
                module_name_mappers,
                module_resource_exts,
                module_system,
                modules_are_use_strict,
                multi_platform,
                multi_platform_extensions,
                multi_platform_extension_group_mapping,
                multi_platform_ambient_supports_platform_project_overrides,
                munge_underscores,
                no_flowlib,
                no_unchecked_indexed_access,
                node_modules_errors,
                node_main_fields,
                node_package_export_conditions,
                node_resolver_allow_root_relative,
                node_resolver_dirnames,
                node_resolver_root_relative_dirnames,
                opaque_type_new_bound_syntax,
                pattern_matching,
                pattern_matching_instance_patterns,
                projects,
                projects_overlap_mapping,
                projects_path_mapping,
                projects_strict_boundary,
                projects_strict_boundary_validate_import_pattern_opt_outs,
                projects_strict_boundary_import_pattern_opt_outs,
                react_custom_jsx_typing,
                stylex_shorthand_prop,
                react_ref_as_prop,
                react_rules,
                react_runtime,
                records,
                records_includes,
                recursion_limit,
                relay_integration,
                relay_integration_esmodules,
                relay_integration_excludes,
                relay_integration_module_prefix,
                relay_integration_module_prefix_includes,
                root_name,
                saved_state_direct_serialization,
                saved_state_fetcher,
                saved_state_persist_export_index,
                saved_state_reinit_on_lib_change,
                saved_state_skip_version_check,
                shm_hash_table_pow: _shm_hash_table_pow,
                shm_heap_size: _shm_heap_size,
                supported_operating_systems,
                strict_es6_import_export,
                ts_syntax,
                allow_readonly_variance,
                ts_utility_syntax,
                tslib_syntax,
                typescript_library_definition_support,
                deprecated_utilities,
                deprecated_utilities_excludes,
                deprecated_colon_extends,
                deprecated_colon_extends_excludes,
                enable_custom_error,
                assert_operator,
                type_expansion_recursion_limit,
                unsuppressable_error_codes,
                use_unknown_in_catch_variables,
                vpn_less,
                wait_for_recheck,
                watchman_defer_states: _watchman_defer_states,
                watchman_sync_timeout: _watchman_sync_timeout,
                max_workers_full_check,
            },
        version: _version,
    } = flowconfig;

    let MakeOptionsOverrides {
        autoimports: autoimports_override,
        lazy_mode: lazy_mode_override,
        saved_state_fetcher: saved_state_fetcher_override,
        saved_state_force_recheck,
        saved_state_no_fallback,
        saved_state_skip_version_check: saved_state_skip_version_check_override,
        saved_state_verify,
    } = overrides;

    let all = all.unwrap_or(false);
    let autoimports = autoimports_override.unwrap_or(autoimports.unwrap_or(true));
    let autoimports_min_characters = autoimports_min_characters.unwrap_or(0) as i32;
    let autoimports_ranked_by_usage_boost_exact_match_min_length =
        autoimports_ranked_by_usage_boost_exact_match_min_length as i32;
    let automatic_require_default = automatic_require_default.unwrap_or(false);
    let babel_loose_array_spread = babel_loose_array_spread.unwrap_or(false);
    let ban_spread_key_props = ban_spread_key_props.unwrap_or(false);
    let casting_syntax = casting_syntax.unwrap_or(CastingSyntax::Both);
    let llm_context_include_imports = false;
    let channel_mode = match channel_mode {
        Some(flow_config::ChannelMode::Pipe) => options::ChannelMode::Pipe,
        Some(flow_config::ChannelMode::Socket) => options::ChannelMode::Socket,
        None => options::ChannelMode::Pipe,
    };
    let enable_const_params = enable_const_params.unwrap_or(false);
    let enable_pattern_matching = pattern_matching.unwrap_or(false);
    let enable_pattern_matching_instance_patterns =
        pattern_matching_instance_patterns.unwrap_or(false);
    let enable_records = records.unwrap_or(false);
    let estimate_recheck_time = estimate_recheck_time.unwrap_or(true);
    let exact_by_default = exact_by_default.unwrap_or(true);
    let lazy_mode = lazy_mode_override.unwrap_or(matches!(
        lazy_mode,
        Some(LazyMode::Lazy) | Some(LazyMode::WatchmanDeprecated)
    ));
    let max_files_checked_per_worker =
        max_files_checked_per_worker_rust_port.unwrap_or(max_files_checked_per_worker) as i32;
    let max_header_tokens = max_header_tokens as i32;
    let merge_timeout = merge_timeout.map(|t| t as f64);
    let use_unknown_in_catch_variables = use_unknown_in_catch_variables.unwrap_or(false);
    let type_expansion_recursion_limit = type_expansion_recursion_limit as i32;

    let enable_jest_integration = jest_integration;
    let enable_relay_integration = relay_integration;

    let unsuppressable_error_codes: Arc<HashSet<FlowSmolStr>> = Arc::new(
        unsuppressable_error_codes
            .into_iter()
            .map(FlowSmolStr::new)
            .collect(),
    );

    let log_saving: Arc<BTreeMap<String, LogSaving>> = Arc::new(
        log_saving
            .into_iter()
            .map(|(k, v)| {
                (
                    k,
                    LogSaving {
                        threshold_time_ms: v.threshold_time_ms,
                        limit: v.limit,
                        rate: v.rate,
                    },
                )
            })
            .collect(),
    );

    let flowlib_dir = flow_flowlib::libdir(
        no_flowlib || cli_no_flowlib,
        &std::path::PathBuf::from(&temp_dir),
    );
    flow_flowlib::extract(&flowlib_dir);
    let flowlib_path = flow_flowlib::path_of_libdir(&flowlib_dir).to_path_buf();

    flow_parser::file_key::set_project_root(&root.to_string_lossy());
    flow_parser::file_key::set_flowlib_root(&flowlib_path.to_string_lossy());

    let casting_syntax_only_support_as_excludes: Arc<[Regex]> =
        casting_syntax_only_support_as_excludes
            .iter()
            .map(|s| expand_and_make_regexp(&root, s))
            .collect::<Vec<_>>()
            .into();

    let hook_compatibility_excludes: Arc<[Regex]> = hook_compatibility_excludes
        .iter()
        .map(|s| expand_and_make_regexp(&root, s))
        .collect::<Vec<_>>()
        .into();

    let hook_compatibility_includes: Arc<[Regex]> = hook_compatibility_includes
        .iter()
        .map(|s| expand_and_make_regexp(&root, s))
        .collect::<Vec<_>>()
        .into();

    let relay_integration_excludes: Arc<[Regex]> = relay_integration_excludes
        .iter()
        .map(|s| expand_and_make_regexp(&root, s))
        .collect::<Vec<_>>()
        .into();

    let relay_integration_module_prefix_includes: Arc<[Regex]> =
        relay_integration_module_prefix_includes
            .iter()
            .map(|s| expand_and_make_regexp(&root, s))
            .collect::<Vec<_>>()
            .into();

    let deprecated_utilities: Arc<BTreeMap<String, Vec<String>>> = Arc::new(
        deprecated_utilities
            .into_iter()
            .map(|(k, v)| {
                let v = v
                    .into_iter()
                    .map(|s| {
                        let s = flow_common::files::expand_project_root_token(&root, &s);
                        flow_common::files::expand_builtin_root_token(&flowlib_path, &s)
                    })
                    .collect();
                (k, v)
            })
            .collect(),
    );

    let deprecated_utilities_excludes: Arc<[Regex]> = deprecated_utilities_excludes
        .iter()
        .map(|s| expand_and_make_regexp(&root, s))
        .collect::<Vec<_>>()
        .into();

    let enabled_rollouts: Arc<BTreeMap<String, String>> = Arc::new(
        rollouts
            .into_iter()
            .map(|(rollout, config)| (rollout, config.enabled_group))
            .collect(),
    );
    let gc_worker = GcControl {
        minor_heap_size: gc_worker_minor_heap_size.or(Some(1024 * 1024 * 2)),
        major_heap_increment: gc_worker_major_heap_increment,
        space_overhead: gc_worker_space_overhead,
        window_size: gc_worker_window_size,
        custom_major_ratio: gc_worker_custom_major_ratio,
        custom_minor_ratio: gc_worker_custom_minor_ratio,
        custom_minor_max_size: gc_worker_custom_minor_max_size,
    };

    let format = Format {
        bracket_spacing: format_bracket_spacing.unwrap_or(true),
        single_quotes: format_single_quotes.unwrap_or(false),
    };

    let available = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let config_max_workers = if !lazy_mode {
        max_workers_full_check.or(max_workers)
    } else {
        max_workers
    };
    let max_workers = std::env::var("FLOW_MAX_WORKERS")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or_else(|| config_max_workers.map(|w| w as usize).unwrap_or(available))
        as i32;

    let log_file = std::path::PathBuf::from(&temp_dir).join(format!("{}.log", flowconfig_name));

    if typescript_library_definition_support && !module_file_exts.contains(&".d.ts".into()) {
        module_file_exts.push(".d.ts".into());
    }

    let node_package_export_conditions = if typescript_library_definition_support {
        let mut conditions = node_package_export_conditions;
        if !conditions.contains(&"types".to_string()) {
            conditions.insert(0, "types".to_string());
        }
        if !conditions.contains(&"import".to_string()) {
            conditions.insert(0, "import".to_string());
        }
        conditions
    } else {
        node_package_export_conditions
    };

    let file_options = {
        use flow_common::files::FileOptions;

        let ignores_processed = ignores
            .into_iter()
            .map(|(path, backup)| {
                let regex = expand_and_make_regexp(&root, &path);
                ((path, backup), regex)
            })
            .collect();

        let untyped_processed = untyped
            .into_iter()
            .map(|pattern| {
                let regex = expand_and_make_regexp(&root, &pattern);
                (pattern, regex)
            })
            .collect();

        let declarations_processed = declarations
            .into_iter()
            .map(|pattern| {
                let regex = expand_and_make_regexp(&root, &pattern);
                (pattern, regex)
            })
            .collect();

        let flowtyped_path = flow_common::files::make_path_absolute(&root, "flow-typed");
        let mut has_explicit_flowtyped_lib = false;
        let mut lib_paths_processed = libs
            .into_iter()
            .map(|(scope, path)| {
                let scope_str = scope.map(|s| s.to_string());
                let path_buf = expand_and_make_path(&root, &path);
                if path_buf == flowtyped_path {
                    has_explicit_flowtyped_lib = true;
                }
                (scope_str, path_buf)
            })
            .collect::<Vec<_>>();
        if !has_explicit_flowtyped_lib && flowtyped_path.exists() {
            lib_paths_processed.insert(0, (None, flowtyped_path));
        }

        let includes_processed = make_includes(
            &root,
            files_implicitly_include_root,
            &lib_paths_processed,
            includes,
        );

        let haste_paths_excludes_processed = haste_paths_excludes
            .into_iter()
            .map(|s| expand_and_make_regexp(&root, &s))
            .collect();

        let haste_paths_includes_processed = haste_paths_includes
            .into_iter()
            .map(|s| expand_and_make_regexp(&root, &s))
            .collect();

        Arc::new(FileOptions {
            default_lib_dir: Some(match flowlib_dir {
                FlowlibDir::Prelude(path) => flow_common::files::LibDir::Prelude(path),
                FlowlibDir::Flowlib(path) => flow_common::files::LibDir::Flowlib(path),
            }),
            ignores: ignores_processed,
            untyped: untyped_processed,
            declarations: declarations_processed,
            implicitly_include_root: files_implicitly_include_root,
            haste_paths_excludes: haste_paths_excludes_processed,
            haste_paths_includes: haste_paths_includes_processed,
            includes: includes_processed,
            lib_paths: lib_paths_processed,
            module_declaration_dirnames: module_declaration_dirnames
                .into_iter()
                .map(|dir| {
                    let expanded = flow_common::files::expand_project_root_token(&root, &dir);
                    flow_common::files::cached_canonicalize(std::path::Path::new(&expanded))
                        .map(|p| p.to_string_lossy().to_string())
                        .unwrap_or(expanded)
                })
                .collect(),
            module_file_exts,
            module_resource_exts: module_resource_exts.into_iter().collect(),
            multi_platform: multi_platform.unwrap_or(false),
            multi_platform_extensions: multi_platform_extensions.to_vec(),
            multi_platform_extension_group_mapping: multi_platform_extension_group_mapping.to_vec(),
            node_resolver_dirnames,
        })
    };

    let projects_options = {
        use flow_common::flow_projects::*;

        let projects_vec = vec1::Vec1::try_from_vec(projects)
            .unwrap_or_else(|_| vec1::vec1![FlowSmolStr::new_inline("default")]);

        let projects_overlap_mapping_vec: BTreeMap<FlowSmolStr, Vec<FlowSmolStr>> =
            projects_overlap_mapping
                .into_iter()
                .map(|(k, v)| (k, v.into_iter().collect()))
                .collect();
        let map_path = move |path: String| {
            let expanded = flow_common::files::expand_project_root_token_as_relative(&path);
            Regex::new(&format!("^{}", expanded)).unwrap()
        };

        Arc::new(ProjectsOptions::mk(
            projects_vec,
            projects_overlap_mapping_vec,
            map_path,
            projects_path_mapping,
            projects_strict_boundary,
            projects_strict_boundary_validate_import_pattern_opt_outs,
            projects_strict_boundary_import_pattern_opt_outs,
            multi_platform_ambient_supports_platform_project_overrides,
        ))
    };

    let deprecated_colon_extends: Arc<[String]> = deprecated_colon_extends
        .into_iter()
        .map(|s| {
            let s = flow_common::files::expand_project_root_token(&root, &s);
            flow_common::files::expand_builtin_root_token(&flowlib_path, &s)
        })
        .collect::<Vec<_>>()
        .into();
    let deprecated_colon_extends_excludes: Arc<[Regex]> = deprecated_colon_extends_excludes
        .iter()
        .map(|s| expand_and_make_regexp(&root, s))
        .collect::<Vec<_>>()
        .into();
    Options {
        abstract_classes,
        all,
        assert_operator,
        autoimports,
        autoimports_min_characters,
        autoimports_ranked_by_usage,
        autoimports_ranked_by_usage_boost_exact_match_min_length,
        automatic_require_default,
        babel_loose_array_spread,
        ban_spread_key_props,
        casting_syntax,
        casting_syntax_only_support_as_excludes,
        channel_mode,
        component_syntax,
        async_component_syntax,
        debug: false, // Not in flowconfig, set to false
        deprecated_utilities,
        deprecated_utilities_excludes,
        deprecated_colon_extends,
        deprecated_colon_extends_excludes,
        dev_only_refinement_info_as_errors,
        distributed: false, // Not in flowconfig, set to false
        enable_const_params,
        enable_custom_error,
        enable_jest_integration,
        enable_pattern_matching,
        enable_pattern_matching_instance_patterns,
        enable_records,
        enable_relay_integration,
        enabled_rollouts,
        enums,
        estimate_recheck_time,
        exact_by_default,
        facebook_fbs: facebook_fbs.map(FlowSmolStr::new),
        facebook_fbt: facebook_fbt.map(FlowSmolStr::new),
        facebook_module_interop,
        file_options,
        flowconfig_hash: FlowSmolStr::new(flowconfig_hash),
        flowconfig_name: FlowSmolStr::new(flowconfig_name),
        format,
        gc_worker,
        haste_module_ref_prefix: haste_module_ref_prefix.map(FlowSmolStr::new),
        hook_compatibility,
        hook_compatibility_excludes,
        hook_compatibility_includes,
        ignore_non_literal_requires,
        include_suppressions: false, // Not in flowconfig, set to false
        include_warnings,
        instance_t_objkit_fix,
        lazy_mode,
        llm_context_include_imports,
        lint_severities,
        log_file: Arc::new(log_file),
        log_saving,
        long_lived_workers,
        max_files_checked_per_worker,
        max_header_tokens,
        max_seconds_for_check_per_worker,
        max_workers,
        merge_timeout,
        missing_module_generators: Arc::from(missing_module_generators),
        module_system,
        module_name_mappers: Arc::from(module_name_mappers),
        modules_are_use_strict,
        munge_underscores,
        no_unchecked_indexed_access,
        node_modules_errors,
        node_main_fields: Arc::from(node_main_fields),
        node_package_export_conditions: Arc::from(node_package_export_conditions),
        node_resolver_allow_root_relative,
        node_resolver_root_relative_dirnames: Arc::from(
            node_resolver_root_relative_dirnames
                .into_iter()
                .map(|(applicable_dir_opt, dirname)| {
                    (
                        applicable_dir_opt
                            .map(|s| flow_common::files::expand_project_root_token_as_relative(&s)),
                        dirname,
                    )
                })
                .collect::<Vec<_>>(),
        ),
        opaque_type_new_bound_syntax,
        profile: false, // Not in flowconfig, set to false
        projects_options,
        quiet: false, // Not in flowconfig, set to false
        records_includes: Arc::from(
            records_includes
                .iter()
                .map(|s| flow_common::files::expand_project_root_token(&root, s))
                .collect::<Vec<_>>(),
        ),
        react_custom_jsx_typing,
        react_ref_as_prop,
        react_rules: Arc::from(react_rules),
        react_runtime,
        recursion_limit,
        relay_integration_esmodules,
        relay_integration_excludes,
        relay_integration_module_prefix: relay_integration_module_prefix.map(FlowSmolStr::new),
        relay_integration_module_prefix_includes,
        root: Arc::new(root),
        root_name: root_name.map(FlowSmolStr::new),
        saved_state_direct_serialization,
        saved_state_fetcher: saved_state_fetcher_override.unwrap_or(saved_state_fetcher),
        saved_state_force_recheck: saved_state_force_recheck.unwrap_or(false),
        saved_state_no_fallback: saved_state_no_fallback.unwrap_or(false),
        saved_state_persist_export_index,
        saved_state_reinit_on_lib_change,
        saved_state_skip_version_check: saved_state_skip_version_check_override.unwrap_or(false)
            || saved_state_skip_version_check,
        saved_state_verify: saved_state_verify.unwrap_or(false),
        slow_to_check_logging: Default::default(), // Not in flowconfig, use default
        strict_es6_import_export,
        strict_mode,
        strip_root: false, // Not in flowconfig, set to false
        supported_operating_systems,
        stylex_shorthand_prop,
        temp_dir: FlowSmolStr::new(temp_dir),
        ts_syntax,
        allow_readonly_variance,
        tslib_syntax,
        typescript_library_definition_support,
        ts_utility_syntax,
        type_expansion_recursion_limit,
        unsuppressable_error_codes,
        use_unknown_in_catch_variables,
        verbose: None, // Not in flowconfig, set to None
        vpn_less,
        wait_for_recheck,
    }
}

pub(super) fn expand_file_list(
    filenames: &[String],
    options: Option<&flow_common::files::FileOptions>,
) -> BTreeSet<String> {
    let paths: Vec<String> = filenames
        .iter()
        .map(|f| {
            flow_common::files::cached_canonicalize(std::path::Path::new(f))
                .map(|p| p.to_string_lossy().into_owned())
                .unwrap_or_else(|_| f.to_string())
        })
        .collect();
    let mut next_files: Box<dyn FnMut() -> Vec<String>> = match paths.as_slice() {
        [] => Box::new(Vec::new),
        _ => {
            let filter: Box<dyn Fn(&str) -> bool> = match options {
                Some(opts) => {
                    let opts = opts.clone();
                    Box::new(move |path: &str| flow_common::files::is_valid_path(&opts, path))
                }
                None => Box::new(|path: &str| path.ends_with(".js")),
            };
            let root = paths[0].clone();
            let others = paths[1..].to_vec();
            Box::new(flow_utils_find::make_next_files(filter, others, root))
        }
    };
    flow_common::files::get_all(&mut *next_files)
}

pub(super) fn get_filenames_from_input(
    input_file: Option<&str>,
    filenames: Option<&[String]>,
) -> Vec<String> {
    get_filenames_from_input_with_allow_imaginary(input_file, filenames, false)
}

pub(super) fn get_filenames_from_input_with_allow_imaginary(
    input_file: Option<&str>,
    filenames: Option<&[String]>,
    allow_imaginary: bool,
) -> Vec<String> {
    let cwd = std::env::current_dir()
        .expect("failed to get current directory")
        .to_string_lossy()
        .to_string();
    let handle_imaginary = |filename: &str| -> String {
        if allow_imaginary {
            flow_common::files::imaginary_realpath(filename)
        } else {
            eprintln!("File not found: {:?}", filename);
            flow_common_exit_status::exit(FlowExitStatus::NoInput);
        }
    };
    let input_file_filenames = match input_file {
        Some("-") => {
            let stdin = std::io::stdin();
            let lines: Vec<String> = stdin
                .lock()
                .lines()
                .map(|l| l.expect("failed to read stdin"))
                .collect();
            flow_common::files::canonicalize_filenames(&cwd, &handle_imaginary, &lines)
        }
        Some(input_file) => {
            let content = std::fs::read_to_string(input_file).expect("failed to read input file");
            let lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();
            let file_dir = Path::new(input_file)
                .parent()
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|| cwd.clone());
            flow_common::files::canonicalize_filenames(&file_dir, &handle_imaginary, &lines)
        }
        None => vec![],
    };
    let cli_filenames = match filenames {
        Some(filenames) => {
            let names: Vec<String> = filenames.to_vec();
            flow_common::files::canonicalize_filenames(&cwd, &handle_imaginary, &names)
        }
        None => vec![],
    };
    let mut result = cli_filenames;
    result.extend(input_file_filenames);
    result
}

pub(super) fn print_version() {
    println!(
        "Flow, a static type checker for JavaScript, version {}",
        flow_common::flow_version::VERSION
    );
}

pub(super) fn check_version(required_version: &Option<String>) -> Result<(), String> {
    match required_version {
        None => Ok(()),
        Some(version_constraint) => {
            match flow_common_semver::semver::satisfies(
                Some(true),
                version_constraint,
                flow_common::flow_version::VERSION,
            ) {
                Ok(true) => Ok(()),
                _ => {
                    let msg = format!(
                        "Wrong version of Flow. The config specifies version {} but this is version {}",
                        version_constraint,
                        flow_common::flow_version::VERSION
                    );
                    Err(msg)
                }
            }
        }
    }
}

pub(super) fn assert_version(flowconfig: &FlowConfig) {
    let required_version = &flowconfig.version;
    match check_version(required_version) {
        Ok(()) => {}
        Err(msg) => {
            eprintln!("{}", msg);
            flow_common_exit_status::exit(FlowExitStatus::InvalidFlowconfig);
        }
    }
}

pub(super) fn guess_root(flowconfig_name: &str, dir_or_file: Option<&str>) -> std::path::PathBuf {
    let dir_or_file = dir_or_file.unwrap_or(".");
    let path = Path::new(dir_or_file);
    if !path.exists() {
        eprintln!(
            "Could not find file or directory {}; canceling search for {}.\nSee \"flow init --help\" for more info",
            dir_or_file, flowconfig_name
        );
        flow_common_exit_status::exit(FlowExitStatus::CouldNotFindFlowconfig);
    }
    let dir = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent()
            .map(|p| {
                if p.as_os_str().is_empty() {
                    Path::new(".").to_path_buf()
                } else {
                    p.to_path_buf()
                }
            })
            .unwrap_or_else(|| Path::new(".").to_path_buf())
    };
    let dir = dir.canonicalize().unwrap_or_else(|_| dir.to_path_buf());
    let mut current = dir.clone();
    for _ in 0..50 {
        if current.join(flowconfig_name).exists() {
            return current;
        }
        if !current.pop() {
            break;
        }
    }
    eprintln!(
        "Could not find a {} in {} or any of its parent directories.\nSee \"flow init --help\" for more info",
        flowconfig_name,
        dir.display()
    );
    flow_common_exit_status::exit(FlowExitStatus::CouldNotFindFlowconfig);
}

pub(super) fn run_command(command: &Command, argv: &[String]) {
    match command_spec::parse_or_show_help(command, argv) {
        Ok(Err(_)) => {
            println!("{}", command.string_of_usage());
            flow_common_exit_status::exit(FlowExitStatus::NoError);
        }
        Ok(Ok(args)) => command.run(&args),
        Err(error) => {
            eprintln!("{}", command_spec::error_message(&error));
            flow_common_exit_status::exit(FlowExitStatus::CommandlineUsageError);
        }
    }
}

pub(super) fn get_check_or_status_exit_code(
    errors: &flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
    warnings: &flow_common_errors::error_utils::ConcreteLocPrintableErrorSet,
    max_warnings: Option<i32>,
) -> FlowExitStatus {
    if errors.is_empty() {
        match max_warnings {
            Some(x) if warnings.cardinal() as i32 > x => FlowExitStatus::TypeError,
            None | Some(_) => FlowExitStatus::NoError,
        }
    } else {
        FlowExitStatus::TypeError
    }
}
