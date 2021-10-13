(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  | Fb_fetcher

type trust_mode =
  | NoTrust
  | CheckTrust
  | SilentTrust

type order_mode =
  | Lexical
  | Dependency
  | LexicalWithDependencyValidation

type env_option = ConstrainWrites

type env_mode =
  | ClassicEnv of env_option list
  | SSAEnv

type react_runtime =
  | ReactRuntimeAutomatic
  | ReactRuntimeClassic

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

type t = {
  opt_abstract_locations: bool;
  opt_all: bool;
  opt_autoimports: bool;
  opt_automatic_require_default: bool;
  opt_babel_loose_array_spread: bool;
  opt_debug: bool;
  opt_direct_dependent_files_fix: bool;
  opt_enable_const_params: bool;
  opt_enable_indexed_access: bool;
  opt_enabled_rollouts: string SMap.t;
  opt_enforce_local_inference_annotations: bool;
  opt_local_inference_annotation_dirs: string list;
  opt_experimental_infer_indexers: bool;
  opt_enforce_strict_call_arity: bool;
  opt_enums: bool;
  opt_exact_by_default: bool;
  opt_facebook_fbs: string option;
  opt_facebook_fbt: string option;
  opt_facebook_module_interop: bool;
  opt_file_options: Files.options;
  opt_flowconfig_name: string;
  opt_flowconfig_hash: string;
  opt_format: format;
  opt_gc_worker: gc_control;
  opt_enable_relay_integration: bool;
  opt_haste_module_ref_prefix: string option;
  opt_haste_name_reducers: (Str.regexp * string) list;
  opt_haste_paths_excludes: string list;
  opt_haste_paths_includes: string list;
  opt_haste_use_name_reducers: bool;
  opt_ignore_non_literal_requires: bool;
  opt_include_suppressions: bool;
  opt_include_warnings: bool;
  opt_lazy_mode: bool;
  opt_lint_severities: Severity.severity LintSettings.t;
  opt_log_file: Path.t;
  opt_log_saving: log_saving SMap.t;
  opt_max_files_checked_per_worker: int;
  opt_max_header_tokens: int;
  opt_max_literal_length: int;
  opt_max_rss_bytes_for_check_per_worker: int;
  opt_max_seconds_for_check_per_worker: float;
  opt_max_workers: int;
  opt_merge_timeout: float option;
  opt_module_name_mappers: (Str.regexp * string) list;
  opt_module: module_system;
  opt_modules_are_use_strict: bool;
  opt_munge_underscores: bool;
  opt_node_main_fields: string list;
  opt_new_merge: bool;
  opt_node_resolver_allow_root_relative: bool;
  opt_node_resolver_root_relative_dirnames: string list;
  opt_prioritize_dependency_checks: bool;
  opt_profile: bool;
  opt_quiet: bool;
  opt_react_runtime: react_runtime;
  opt_react_server_component_exts: SSet.t;
  opt_recursion_limit: int;
  opt_refactor: bool;
  opt_reorder_checking: order_mode;
  opt_root_name: string option;
  opt_root: Path.t;
  opt_run_post_inference_implicit_instantiation: bool;
  opt_saved_state_fetcher: saved_state_fetcher;
  opt_saved_state_load_sighashes: bool;
  opt_saved_state_force_recheck: bool;
  opt_saved_state_no_fallback: bool;
  opt_strict_es6_import_export_excludes: string list;
  opt_strict_es6_import_export: bool;
  opt_strict_mode: StrictModeSettings.t;
  opt_strip_root: bool;
  opt_suppress_types: SSet.t;
  opt_temp_dir: string;
  opt_env_mode: env_mode;
  opt_traces: int;
  opt_trust_mode: trust_mode;
  opt_type_asserts: bool;
  opt_verbose: Verbose.t option;
  opt_wait_for_recheck: bool;
  opt_weak: bool;
}

let env_option_enabled mode option =
  match mode with
  | SSAEnv -> false
  | ClassicEnv opts -> List.mem option opts

let abstract_locations opts = opts.opt_abstract_locations

let all opts = opts.opt_all

let autoimports opts = opts.opt_autoimports

let automatic_require_default opts = opts.opt_automatic_require_default

let babel_loose_array_spread opts = opts.opt_babel_loose_array_spread

let new_merge opts = opts.opt_new_merge

let max_literal_length opts = opts.opt_max_literal_length

let direct_dependent_files_fix opts = opts.opt_direct_dependent_files_fix

let enable_const_params opts = opts.opt_enable_const_params

let enable_indexed_access opts = opts.opt_enable_indexed_access

let enable_relay_integration opts = opts.opt_enable_relay_integration

let enabled_rollouts opts = opts.opt_enabled_rollouts

let enforce_strict_call_arity opts = opts.opt_enforce_strict_call_arity

let enforce_local_inference_annotations opts = opts.opt_enforce_local_inference_annotations

let local_inference_annotation_dirs opts = opts.opt_local_inference_annotation_dirs

let experimental_infer_indexers opts = opts.opt_experimental_infer_indexers

let enums opts = opts.opt_enums

let format_bracket_spacing opts = opts.opt_format.opt_bracket_spacing

let format_single_quotes opts = opts.opt_format.opt_single_quotes

let env_mode opts = opts.opt_env_mode

let exact_by_default opts = opts.opt_exact_by_default

let haste_module_ref_prefix opts = opts.opt_haste_module_ref_prefix

let haste_name_reducers opts = opts.opt_haste_name_reducers

let haste_paths_excludes opts = opts.opt_haste_paths_excludes

let haste_paths_includes opts = opts.opt_haste_paths_includes

let haste_use_name_reducers opts = opts.opt_haste_use_name_reducers

let flowconfig_name opts = opts.opt_flowconfig_name

let flowconfig_hash opts = opts.opt_flowconfig_hash

let file_options opts = opts.opt_file_options

let is_debug_mode opts = opts.opt_debug

let lazy_mode opts = opts.opt_lazy_mode

let is_quiet opts = opts.opt_quiet

let log_file opts = opts.opt_log_file

let log_saving opts = opts.opt_log_saving

let max_files_checked_per_worker opts = opts.opt_max_files_checked_per_worker

let max_header_tokens opts = opts.opt_max_header_tokens

let max_rss_bytes_for_check_per_worker opts = opts.opt_max_rss_bytes_for_check_per_worker

let max_seconds_for_check_per_worker opts = opts.opt_max_seconds_for_check_per_worker

let max_trace_depth opts = opts.opt_traces

let max_workers opts = opts.opt_max_workers

let merge_timeout opts = opts.opt_merge_timeout

let module_name_mappers opts = opts.opt_module_name_mappers

let module_system opts = opts.opt_module

let modules_are_use_strict opts = opts.opt_modules_are_use_strict

let node_main_fields opts = opts.opt_node_main_fields

let node_resolver_allow_root_relative opts = opts.opt_node_resolver_allow_root_relative

let node_resolver_root_relative_dirnames opts = opts.opt_node_resolver_root_relative_dirnames

let prioritize_dependency_checks opts = opts.opt_prioritize_dependency_checks

let react_runtime opts = opts.opt_react_runtime

let react_server_component_exts opts = opts.opt_react_server_component_exts

let recursion_limit opts = opts.opt_recursion_limit

let refactor opts = opts.opt_refactor

let root opts = opts.opt_root

let root_name opts = opts.opt_root_name

let facebook_fbs opts = opts.opt_facebook_fbs

let facebook_fbt opts = opts.opt_facebook_fbt

let facebook_module_interop opts = opts.opt_facebook_module_interop

let gc_worker opts = opts.opt_gc_worker

let reorder_checking opts = opts.opt_reorder_checking

let run_post_inference_implicit_instantiation opts =
  opts.opt_run_post_inference_implicit_instantiation

let saved_state_fetcher opts = opts.opt_saved_state_fetcher

let saved_state_load_sighashes opts = opts.opt_saved_state_load_sighashes

let saved_state_force_recheck opts = opts.opt_saved_state_force_recheck

let saved_state_no_fallback opts = opts.opt_saved_state_no_fallback

let should_ignore_non_literal_requires opts = opts.opt_ignore_non_literal_requires

let should_include_warnings opts = opts.opt_include_warnings

let should_munge_underscores opts = opts.opt_munge_underscores

let should_profile opts = opts.opt_profile && not opts.opt_quiet

let should_strip_root opts = opts.opt_strip_root

let strict_es6_import_export opts = opts.opt_strict_es6_import_export

let strict_es6_import_export_excludes opts = opts.opt_strict_es6_import_export_excludes

let suppress_types opts = opts.opt_suppress_types

let temp_dir opts = opts.opt_temp_dir

let verbose opts = opts.opt_verbose

let wait_for_recheck opts = opts.opt_wait_for_recheck

let weak_by_default opts = opts.opt_weak

let include_suppressions opts = opts.opt_include_suppressions

let lint_severities opts = opts.opt_lint_severities

let strict_mode opts = opts.opt_strict_mode

let trust_mode opts = opts.opt_trust_mode

let type_asserts opts = opts.opt_type_asserts
