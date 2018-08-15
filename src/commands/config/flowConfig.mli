(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type config

val get: ?allow_cache:bool -> string -> config
val get_hash: ?allow_cache:bool -> string -> Xx.hash
val empty_config: config

val init:
  ignores: string list ->
  untyped: string list ->
  declarations: string list ->
  includes: string list ->
  libs: string list ->
  options: string list ->
  lints: string list ->
  config
val write: config -> out_channel -> unit

(* Accessors *)

(* completely ignored files (both module resolving and typing) *)
val ignores: config -> string list
(* files that should be treated as untyped *)
val untyped: config -> string list
(* files that should be treated as declarations *)
val declarations: config -> string list
(* non-root include paths *)
val includes: config -> string list
(* library paths. no wildcards *)
val libs: config -> string list

(* options *)
val all: config -> bool
val emoji: config -> bool
val enable_cancelable_rechecks: config -> bool
val enable_const_params: config -> bool
val enforce_strict_call_arity: config -> bool
val enforce_well_formed_exports: config -> bool
val esproposal_class_instance_fields: config -> Options.esproposal_feature_mode
val esproposal_class_static_fields: config -> Options.esproposal_feature_mode
val esproposal_decorators: config -> Options.esproposal_feature_mode
val esproposal_export_star_as: config -> Options.esproposal_feature_mode
val esproposal_nullish_coalescing: config -> Options.esproposal_feature_mode
val esproposal_optional_chaining: config -> Options.esproposal_feature_mode
val facebook_fbt: config -> string option
val file_watcher: config -> Options.file_watcher option
val haste_name_reducers: config -> (Str.regexp * string) list
val haste_paths_blacklist: config -> string list
val haste_paths_whitelist: config -> string list
val haste_use_name_reducers: config -> bool
val ignore_non_literal_requires: config -> bool
val include_warnings: config -> bool
val log_file: config -> Path.t option
val max_header_tokens: config -> int
val max_literal_length: config -> int
val max_workers: config -> int
val merge_timeout: config -> int option
val module_file_exts: config -> SSet.t
val module_name_mappers: config -> (Str.regexp * string) list
val module_resolver: config -> Path.t option
val module_resource_exts: config -> SSet.t
val module_system: config -> Options.module_system
val modules_are_use_strict: config -> bool
val munge_underscores: config -> bool
val name: config -> string option
val no_flowlib: config -> bool
val node_resolver_dirnames: config -> string list
val required_version: config -> string option
val saved_state_load_script: config -> string option
val shm_dep_table_pow: config -> int
val shm_dirs: config -> string list
val shm_global_size: config -> int
val shm_hash_table_pow: config -> int
val shm_heap_size: config -> int
val shm_log_level: config -> int
val shm_min_avail: config -> int
val suppress_comments: config -> Str.regexp list
val suppress_types: config -> SSet.t
val temp_dir: config -> string
val traces: config -> int
val weak: config -> bool

(* global defaults for lint suppressions and strict mode *)
val lint_severities: config -> Severity.severity LintSettings.t
val strict_mode: config -> StrictModeSettings.t
