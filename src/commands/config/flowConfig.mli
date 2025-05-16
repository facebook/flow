(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type file_watcher =
  | NoFileWatcher
  | DFind
  | Watchman

type lazy_mode =
  | Lazy
  | Non_lazy
  | Watchman_DEPRECATED  (** lazy_mode=watchman is deprecated, but implies file_watcher=Watchman *)

type config

type warning = int * string

type error = int * string

type config_result = (config * warning list, error) result

val get : ?allow_cache:bool -> string -> config_result

val get_hash : ?allow_cache:bool -> string -> Xx.hash

val get_with_hash : ?allow_cache:bool -> string -> config_result * Xx.hash

val empty_config : config

val init :
  ignores:string list ->
  untyped:string list ->
  declarations:string list ->
  includes:string list ->
  libs:string list ->
  options:string list ->
  lints:string list ->
  (config * warning list, error) result

val write : config -> out_channel -> unit

(* Accessors *)

(* completely ignored files (both module resolving and typing) *)
val ignores : config -> (string * string option) list

(* files that should be treated as untyped *)
val untyped : config -> string list

(* files that should be treated as declarations *)
val declarations : config -> string list

(* non-root include paths *)
val includes : config -> string list

(* library paths. no wildcards *)
val libs : config -> (string option * string) list

(* A map from the rollout's name to the enabled group's name *)
val enabled_rollouts : config -> string SMap.t

(* options *)
val all : config -> bool option

val autoimports : config -> bool option

val autoimports_min_characters : config -> int option

val autoimports_ranked_by_usage : config -> bool

val autoimports_ranked_by_usage_boost_exact_match_min_length : config -> int

val automatic_require_default : config -> bool option

val babel_loose_array_spread : config -> bool option

val ban_spread_key_props : config -> bool option

val casting_syntax : config -> Options.CastingSyntax.t option

val channel_mode : config -> [ `pipe | `socket ] option

val component_syntax : config -> bool

val dev_only_refinement_info_as_errors : config -> bool

val emoji : config -> bool option

val enable_const_params : config -> bool option

val enums : config -> bool

val estimate_recheck_time : config -> bool option

val exact_by_default : config -> bool option

val facebook_fbs : config -> string option

val facebook_fbt : config -> string option

val facebook_module_interop : config -> bool

val file_watcher : config -> file_watcher option

val file_watcher_mergebase_with : config -> string option

val file_watcher_mergebase_with_git : config -> string option

val file_watcher_mergebase_with_hg : config -> string option

val file_watcher_timeout : config -> int option

val files_implicitly_include_root : config -> bool

val format_bracket_spacing : config -> bool option

val format_single_quotes : config -> bool option

val gc_worker_custom_major_ratio : config -> int option

val gc_worker_custom_minor_max_size : config -> int option

val gc_worker_custom_minor_ratio : config -> int option

val gc_worker_major_heap_increment : config -> int option

val gc_worker_minor_heap_size : config -> int option

val gc_worker_space_overhead : config -> int option

val gc_worker_window_size : config -> int option

val haste_module_ref_prefix : config -> string option

val haste_namespaces_enabled : config -> bool

val haste_paths_excludes : config -> string list

val haste_paths_includes : config -> string list

val hook_compatibility_excludes : config -> string list

val hook_compatibility_includes : config -> string list

val hook_compatibility : config -> bool

val ignore_non_literal_requires : config -> bool

val include_warnings : config -> bool

val jest_integration : config -> bool

val lazy_mode : config -> lazy_mode option

(* global defaults for lint suppressions and strict mode *)
val lint_severities : config -> Severity.severity LintSettings.t

val log_saving : config -> Options.log_saving SMap.t

val long_lived_workers : config -> bool

val max_files_checked_per_worker : config -> int

val max_header_tokens : config -> int

val max_literal_length : config -> int

val max_seconds_for_check_per_worker : config -> float

val max_workers : config -> int option

val merge_timeout : config -> int option

val missing_module_generators : config -> (Str.regexp * string) list

val module_declaration_dirnames : config -> string list

val module_file_exts : config -> string list

val module_name_mappers : config -> (Str.regexp * string) list

val module_resource_exts : config -> SSet.t

val module_system : config -> Options.module_system

val modules_are_use_strict : config -> bool

val multi_platform : config -> bool option

val multi_platform_extensions : config -> string list

val multi_platform_extension_group_mapping : config -> (string * string list) list

val multi_platform_ambient_supports_platform_directory_overrides :
  config -> (string * string list) list

val munge_underscores : config -> bool

val natural_inference_local_primitive_literals_full : config -> bool

val natural_inference_local_primitive_literals_full_includes : config -> string list

val no_flowlib : config -> bool

val no_unchecked_indexed_access : config -> bool

val node_main_fields : config -> string list

val node_package_export_conditions : config -> string list

val node_resolver_allow_root_relative : config -> bool

val node_resolver_dirnames : config -> string list

val pattern_matching : config -> bool option

val pattern_matching_includes : config -> string list

val projects : config -> string Nel.t

val projects_overlap_mapping : config -> SSet.t SMap.t

val projects_path_mapping : config -> (string * string list) list

val projects_strict_boundary : config -> bool

val node_resolver_root_relative_dirnames : config -> (string option * string) list

val react_custom_jsx_typing : config -> bool

val react_ref_as_prop : config -> Options.ReactRefAsProp.t

val react_rules : config -> Options.react_rules list

val react_runtime : config -> Options.react_runtime

val recursion_limit : config -> int

val relay_integration : config -> bool

val relay_integration_esmodules : config -> bool

val relay_integration_excludes : config -> string list

val relay_integration_module_prefix : config -> string option

val relay_integration_module_prefix_includes : config -> string list

val required_version : config -> string option

val root_name : config -> string option

val saved_state_fetcher : config -> Options.saved_state_fetcher

val shm_hash_table_pow : config -> int

val shm_heap_size : config -> int

val strict_es6_import_export : config -> bool

val strict_mode : config -> StrictModeSettings.t

val suppress_types : config -> SSet.t

val ts_syntax : config -> bool

val assert_operator : config -> Options.AssertOperator.t

val type_expansion_recursion_limit : config -> int

val unsuppressable_error_codes : config -> SSet.t

val use_mixed_in_catch_variables : config -> bool option

val wait_for_recheck : config -> bool

val watchman_defer_states : config -> string list

val watchman_sync_timeout : config -> int option
