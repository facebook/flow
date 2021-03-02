(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type file_watcher =
  | NoFileWatcher
  | DFind
  | Watchman

type config

type warning = int * string

type error = int * string

val get : ?allow_cache:bool -> string -> (config * warning list, error) result

val get_hash : ?allow_cache:bool -> string -> Xx.hash

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
val ignores : config -> string list

(* files that should be treated as untyped *)
val untyped : config -> string list

(* files that should be treated as declarations *)
val declarations : config -> string list

(* non-root include paths *)
val includes : config -> string list

(* library paths. no wildcards *)
val libs : config -> string list

(* A map from the rollout's name to the enabled group's name *)
val enabled_rollouts : config -> string SMap.t

(* options *)
val abstract_locations : config -> bool

val all : config -> bool

val autoimports : config -> bool option

val automatic_require_default : config -> bool

val babel_loose_array_spread : config -> bool

val disable_live_non_parse_errors : config -> bool

val emoji : config -> bool

val enable_const_params : config -> bool

val enforce_local_inference_annotations : config -> bool

val enforce_strict_call_arity : config -> bool

val enums : config -> bool

val enums_with_unknown_members : config -> bool

val this_annot : config -> bool

val exact_by_default : config -> bool

val facebook_fbs : config -> string option

val facebook_fbt : config -> string option

val facebook_module_interop : config -> bool

val file_watcher : config -> file_watcher option

val file_watcher_timeout : config -> int option

val format_single_quotes : config -> bool option

val haste_module_ref_prefix : config -> string option

val haste_name_reducers : config -> (Str.regexp * string) list

val haste_paths_excludes : config -> string list

val haste_paths_includes : config -> string list

val haste_use_name_reducers : config -> bool

val ignore_non_literal_requires : config -> bool

val include_warnings : config -> bool

val indexed_access : config -> bool

val lazy_mode : config -> Options.lazy_mode option

val log_file : config -> Path.t option

val max_files_checked_per_worker : config -> int

val max_header_tokens : config -> int

val max_literal_length : config -> int

val max_rss_bytes_for_check_per_worker : config -> int

val max_seconds_for_check_per_worker : config -> float

val max_workers : config -> int

val merge_timeout : config -> int option

val module_file_exts : config -> SSet.t

val module_name_mappers : config -> (Str.regexp * string) list

val module_resource_exts : config -> SSet.t

val module_system : config -> Options.module_system

val modules_are_use_strict : config -> bool

val munge_underscores : config -> bool

val no_flowlib : config -> bool

val node_main_fields : config -> string list

val node_resolver_allow_root_relative : config -> bool

val node_resolver_dirnames : config -> string list

val node_resolver_root_relative_dirnames : config -> string list

val required_version : config -> string option

val react_runtime : config -> Options.react_runtime

val react_server_component_exts : config -> SSet.t

val recursion_limit : config -> int

val root_name : config -> string option

val run_post_inference_implicit_instantiation : config -> bool

val saved_state_fetcher : config -> Options.saved_state_fetcher

val saved_state_load_sighashes : config -> bool

val shm_hash_table_pow : config -> int

val shm_heap_size : config -> int

val shm_log_level : config -> int

val strict_es6_import_export : config -> bool

val strict_es6_import_export_excludes : config -> string list

val suppress_types : config -> SSet.t

val temp_dir : config -> string

val traces : config -> int

val trust_mode : config -> Options.trust_mode

val type_asserts : config -> bool

val new_signatures : config -> bool

val watchman_sync_timeout : config -> int option

val watchman_defer_states : config -> string list

val watchman_mergebase_with : config -> string option

val wait_for_recheck : config -> bool

val weak : config -> bool

(* global defaults for lint suppressions and strict mode *)
val lint_severities : config -> Severity.severity LintSettings.t

val strict_mode : config -> StrictModeSettings.t
