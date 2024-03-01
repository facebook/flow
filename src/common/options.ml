(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type module_system =
  | Node
  | Haste

type jsx_mode =
  (* JSX desugars into a `React.createElement(name, props, ...children)` call *)
  | Jsx_react
  (*
   * Specifies a function that should be invoked instead of React.createElement
   * when interpreting JSX syntax. Otherwise, the usual rules of JSX are
   * followed: children are varargs after a props argument.
   *)
  | Jsx_pragma of (string * (ALoc.t, ALoc.t) Flow_ast.Expression.t)

type saved_state_fetcher =
  | Dummy_fetcher
  | Local_fetcher
  | Scm_fetcher
  | Fb_fetcher

type react_runtime =
  | ReactRuntimeAutomatic
  | ReactRuntimeClassic

type react_rules =
  | ValidateRefAccessDuringRender
  | DeepReadOnlyProps
  | DeepReadOnlyHookReturns
  | RulesOfHooks

type format = {
  opt_bracket_spacing: bool;
  opt_single_quotes: bool;
}

type gc_control = {
  gc_minor_heap_size: int option;
  gc_major_heap_increment: int option;
  gc_space_overhead: int option;
  gc_window_size: int option;
  gc_custom_major_ratio: int option;
  gc_custom_minor_ratio: int option;
  gc_custom_minor_max_size: int option;
}

type log_saving = {
  threshold_time_ms: int;
  limit: int option;
  rate: float;
}

module CastingSyntax = struct
  type t =
    | Colon
    | As
    | Both
end

type t = {
  opt_all: bool;
  opt_as_const: bool;
  opt_any_propagation: bool;
  opt_autoimports: bool;
  opt_autoimports_min_characters: int;
  opt_autoimports_ranked_by_usage: bool;
  opt_autoimports_ranked_by_usage_boost_exact_match_min_length: int option;
  opt_automatic_require_default: bool;
  opt_babel_loose_array_spread: bool;
  opt_blocking_worker_communication: bool;
  opt_casting_syntax: CastingSyntax.t;
  opt_channel_mode: [ `pipe | `socket ];
  opt_component_syntax: bool;
  opt_hooklike_functions_includes: string list;
  opt_hooklike_functions: bool;
  opt_react_rules: react_rules list;
  opt_debug: bool;
  opt_enable_const_params: bool;
  opt_enable_relay_integration: bool;
  opt_enabled_rollouts: string SMap.t;
  opt_enums: bool;
  opt_estimate_recheck_time: bool;
  opt_exact_by_default: bool;
  opt_facebook_fbs: string option;
  opt_facebook_fbt: string option;
  opt_facebook_module_interop: bool;
  opt_file_options: Files.options;
  opt_flowconfig_hash: string;
  opt_flowconfig_name: string;
  opt_format: format;
  opt_gc_worker: gc_control;
  opt_global_find_ref: bool;
  opt_global_rename: bool;
  opt_haste_module_ref_prefix: string option;
  opt_haste_module_ref_prefix_LEGACY_INTEROP: string option;
  opt_haste_name_reducers: (Str.regexp * string) list;
  opt_haste_paths_excludes: string list;
  opt_haste_paths_includes: string list;
  opt_ignore_non_literal_requires: bool;
  opt_include_suppressions: bool;
  opt_include_warnings: bool;
  opt_lazy_mode: bool;
  opt_lint_severities: Severity.severity LintSettings.t;
  opt_log_file: File_path.t;
  opt_log_saving: log_saving SMap.t;
  opt_long_lived_workers: bool;
  opt_max_files_checked_per_worker: int;
  opt_max_header_tokens: int;
  opt_max_literal_length: int;
  opt_max_seconds_for_check_per_worker: float;
  opt_max_workers: int;
  opt_merge_timeout: float option;
  opt_missing_module_generators: (Str.regexp * string) list;
  opt_module: module_system;
  opt_module_name_mappers: (Str.regexp * string) list;
  opt_modules_are_use_strict: bool;
  opt_munge_underscores: bool;
  opt_namespaces: bool;
  opt_node_main_fields: string list;
  opt_node_resolver_allow_root_relative: bool;
  opt_node_resolver_root_relative_dirnames: string list;
  opt_profile: bool;
  opt_quiet: bool;
  opt_react_runtime: react_runtime;
  opt_recursion_limit: int;
  opt_relay_integration_esmodules: bool;
  opt_relay_integration_excludes: Str.regexp list;
  opt_relay_integration_module_prefix: string option;
  opt_relay_integration_module_prefix_includes: Str.regexp list;
  opt_root: File_path.t;
  opt_root_name: string option;
  opt_saved_state_allow_reinit: bool;
  opt_saved_state_fetcher: saved_state_fetcher;
  opt_saved_state_force_recheck: bool;
  opt_saved_state_no_fallback: bool;
  opt_saved_state_skip_version_check: bool;
  opt_saved_state_verify: bool;
  opt_slow_to_check_logging: Slow_to_check_logging.t;
  opt_strict_es6_import_export: bool;
  opt_strict_es6_import_export_excludes: string list;
  opt_strict_mode: StrictModeSettings.t;
  opt_strip_root: bool;
  opt_suppress_types: SSet.t;
  opt_temp_dir: string;
  opt_traces: int;
  opt_ts_syntax: bool;
  opt_use_mixed_in_catch_variables: bool;
  opt_verbose: Verbose.t option;
  opt_wait_for_recheck: bool;
  opt_distributed: bool;
}

let all opts = opts.opt_all

let as_const opts = opts.opt_as_const

let any_propagation opts = opts.opt_any_propagation

let autoimports opts = opts.opt_autoimports

let autoimports_min_characters opts = opts.opt_autoimports_min_characters

let autoimports_ranked_by_usage opts = opts.opt_autoimports_ranked_by_usage

let autoimports_ranked_by_usage_boost_exact_match_min_length opts =
  opts.opt_autoimports_ranked_by_usage_boost_exact_match_min_length

let automatic_require_default opts = opts.opt_automatic_require_default

let babel_loose_array_spread opts = opts.opt_babel_loose_array_spread

let blocking_worker_communication opts = opts.opt_blocking_worker_communication

let casting_syntax opts = opts.opt_casting_syntax

let channel_mode opts = opts.opt_channel_mode

let component_syntax opts = opts.opt_component_syntax

let hooklike_functions opts = opts.opt_hooklike_functions

let hooklike_functions_includes opts = opts.opt_hooklike_functions_includes

let hooklike_functions_in_file opts file =
  hooklike_functions opts
  || begin
       match hooklike_functions_includes opts with
       | [] -> false
       | dirs ->
         let filename = File_key.to_string file in
         let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
         List.exists (fun str -> Base.String.is_prefix ~prefix:str normalized_filename) dirs
     end

let typecheck_component_syntax_in_file opts file =
  component_syntax opts || File_key.is_lib_file file

let react_rules opts = opts.opt_react_rules

let enable_const_params opts = opts.opt_enable_const_params

let enable_relay_integration opts = opts.opt_enable_relay_integration

let enabled_rollouts opts = opts.opt_enabled_rollouts

let enums opts = opts.opt_enums

let estimate_recheck_time opts = opts.opt_estimate_recheck_time

let exact_by_default opts = opts.opt_exact_by_default

let facebook_fbs opts = opts.opt_facebook_fbs

let facebook_fbt opts = opts.opt_facebook_fbt

let facebook_module_interop opts = opts.opt_facebook_module_interop

let file_options opts = opts.opt_file_options

let flowconfig_hash opts = opts.opt_flowconfig_hash

let flowconfig_name opts = opts.opt_flowconfig_name

let format_bracket_spacing opts = opts.opt_format.opt_bracket_spacing

let format_single_quotes opts = opts.opt_format.opt_single_quotes

let gc_worker opts = opts.opt_gc_worker

let global_find_ref opts = opts.opt_global_find_ref

let global_rename opts = opts.opt_global_rename

let haste_module_ref_prefix opts = opts.opt_haste_module_ref_prefix

let haste_module_ref_prefix_LEGACY_INTEROP opts = opts.opt_haste_module_ref_prefix_LEGACY_INTEROP

let haste_name_reducers opts = opts.opt_haste_name_reducers

let haste_paths_excludes opts = opts.opt_haste_paths_excludes

let haste_paths_includes opts = opts.opt_haste_paths_includes

let include_suppressions opts = opts.opt_include_suppressions

let is_debug_mode opts = opts.opt_debug

let is_quiet opts = opts.opt_quiet

let lazy_mode opts = opts.opt_lazy_mode

let lint_severities opts = opts.opt_lint_severities

let log_file opts = opts.opt_log_file

let log_saving opts = opts.opt_log_saving

let long_lived_workers opts = opts.opt_long_lived_workers

let max_files_checked_per_worker opts = opts.opt_max_files_checked_per_worker

let max_header_tokens opts = opts.opt_max_header_tokens

let max_literal_length opts = opts.opt_max_literal_length

let max_seconds_for_check_per_worker opts = opts.opt_max_seconds_for_check_per_worker

let max_trace_depth opts = opts.opt_traces

let max_workers opts = opts.opt_max_workers

let merge_timeout opts = opts.opt_merge_timeout

let missing_module_generators opts = opts.opt_missing_module_generators

let module_name_mappers opts = opts.opt_module_name_mappers

let module_system opts = opts.opt_module

let modules_are_use_strict opts = opts.opt_modules_are_use_strict

let namespaces opts = opts.opt_namespaces

let node_main_fields opts = opts.opt_node_main_fields

let node_resolver_allow_root_relative opts = opts.opt_node_resolver_allow_root_relative

let node_resolver_root_relative_dirnames opts = opts.opt_node_resolver_root_relative_dirnames

let react_runtime opts = opts.opt_react_runtime

let recursion_limit opts = opts.opt_recursion_limit

let relay_integration_esmodules opts = opts.opt_relay_integration_esmodules

let relay_integration_excludes opts = opts.opt_relay_integration_excludes

let relay_integration_module_prefix opts = opts.opt_relay_integration_module_prefix

let relay_integration_module_prefix_includes opts =
  opts.opt_relay_integration_module_prefix_includes

let root opts = opts.opt_root

let root_name opts = opts.opt_root_name

let saved_state_allow_reinit opts = opts.opt_saved_state_allow_reinit

let saved_state_fetcher opts = opts.opt_saved_state_fetcher

let saved_state_force_recheck opts = opts.opt_saved_state_force_recheck

let saved_state_no_fallback opts = opts.opt_saved_state_no_fallback

let saved_state_skip_version_check opts = opts.opt_saved_state_skip_version_check

let saved_state_verify opts = opts.opt_saved_state_verify

let should_ignore_non_literal_requires opts = opts.opt_ignore_non_literal_requires

let should_include_warnings opts = opts.opt_include_warnings

let should_munge_underscores opts = opts.opt_munge_underscores

let should_profile opts = opts.opt_profile && not opts.opt_quiet

let should_strip_root opts = opts.opt_strip_root

let slow_to_check_logging opts = opts.opt_slow_to_check_logging

let strict_es6_import_export opts = opts.opt_strict_es6_import_export

let strict_es6_import_export_excludes opts = opts.opt_strict_es6_import_export_excludes

let strict_mode opts = opts.opt_strict_mode

let suppress_types opts = opts.opt_suppress_types

let temp_dir opts = opts.opt_temp_dir

let ts_syntax opts = opts.opt_ts_syntax

let use_mixed_in_catch_variables opts = opts.opt_use_mixed_in_catch_variables

let verbose opts = opts.opt_verbose

let wait_for_recheck opts = opts.opt_wait_for_recheck

let distributed opts = opts.opt_distributed
