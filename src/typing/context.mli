(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocMap = Utils_js.LocMap

exception Props_not_found of Type.Properties.id
exception Exports_not_found of Type.Exports.id
exception Module_not_found of string
exception Tvar_not_found of Constraint.ident

type env = Scope.t list

type t
type cacheable_t

type local_metadata = {
  checked: bool;
  munge_underscores: bool;
  verbose: Verbose.t option;
  weak: bool;
  jsx: Options.jsx_mode option;
  strict: bool;
}
type global_metadata = {
  enable_const_params: bool;
  enable_unsafe_getters_and_setters: bool;
  enforce_strict_type_args: bool;
  enforce_strict_call_arity: bool;
  esproposal_class_static_fields: Options.esproposal_feature_mode;
  esproposal_class_instance_fields: Options.esproposal_feature_mode;
  esproposal_decorators: Options.esproposal_feature_mode;
  esproposal_export_star_as: Options.esproposal_feature_mode;
  facebook_fbt: string option;
  ignore_non_literal_requires: bool;
  max_trace_depth: int;
  root: Path.t;
  strip_root: bool;
  suppress_comments: Str.regexp list;
  suppress_types: SSet.t;
  max_workers: int;
}
type metadata = {
  local_metadata: local_metadata;
  global_metadata: global_metadata;
}
type module_kind =
  | CommonJSModule of Loc.t option
  | ESModule

val make: metadata -> File_key.t -> string -> t
val metadata_of_options: Options.t -> metadata

val to_cache: t -> cacheable_t
val from_cache: options:Options.t -> cacheable_t -> t

(* accessors *)
val all_unresolved: t -> ISet.t IMap.t
val annot_table: t -> (Loc.t, Type.t) Hashtbl.t
val declare_module_t: t -> Type.t option
val enable_const_params: t -> bool
val enable_unsafe_getters_and_setters: t -> bool
val enforce_strict_type_args: t -> bool
val enforce_strict_call_arity: t -> bool
val envs: t -> env IMap.t
val errors: t -> Errors.ErrorSet.t
val error_suppressions: t -> Error_suppressions.t
val esproposal_class_static_fields: t -> Options.esproposal_feature_mode
val esproposal_class_instance_fields: t -> Options.esproposal_feature_mode
val esproposal_decorators: t -> Options.esproposal_feature_mode
val esproposal_export_star_as: t -> Options.esproposal_feature_mode
val evaluated: t -> Type.t IMap.t
val file: t -> File_key.t
val find_props: t -> Type.Properties.id -> Type.Properties.t
val find_exports: t -> Type.Exports.id -> Type.Exports.t
val find_require: t -> string -> Type.t
val find_module: t -> string -> Type.t
val find_tvar: t -> Constraint.ident -> Constraint.node
val mem_nominal_id: t -> Constraint.ident -> bool
val globals: t -> SSet.t
val graph: t -> Constraint.node IMap.t
val import_stmts: t -> Loc.t Ast.Statement.ImportDeclaration.t list
val imported_ts: t -> Type.t SMap.t
val is_checked: t -> bool
val is_verbose: t -> bool
val is_weak: t -> bool
val severity_cover: t -> ExactCover.lint_severity_cover
val max_trace_depth: t -> int
val module_kind: t -> module_kind
val require_map: t -> Type.t SMap.t
val module_map: t -> Type.t SMap.t
val module_ref: t -> string
val property_maps: t -> Type.Properties.map
val refs_table: t -> (Loc.t, Loc.t) Hashtbl.t
val export_maps: t -> Type.Exports.map
val root: t -> Path.t
val facebook_fbt: t -> string option
val should_ignore_non_literal_requires: t -> bool
val should_munge_underscores: t -> bool
val should_strip_root: t -> bool
val suppress_comments: t -> Str.regexp list
val suppress_types: t -> SSet.t
val type_graph: t -> Graph_explorer.graph
val type_table: t -> Type_table.t
val verbose: t -> Verbose.t option
val max_workers: t -> int
val jsx: t -> Options.jsx_mode option
val exists_checks: t -> ExistsCheck.t LocMap.t
val exists_excuses: t -> ExistsCheck.t LocMap.t
val dep_map: t -> Dep_mapper.Dep.t Dep_mapper.DepMap.t
val use_def_map: t -> Loc.t LocMap.t
val pid_prefix: t -> string

val copy_of_context: t -> t
val merge_into: t -> t -> unit

(* mutators *)
val add_env: t -> int -> env -> unit
val add_error: t -> Errors.error -> unit
val add_error_suppression: t -> Loc.t -> unit
val add_global: t -> string -> unit
val add_import_stmt: t -> Loc.t Ast.Statement.ImportDeclaration.t -> unit
val add_imported_t: t -> string -> Type.t -> unit
val add_require: t -> string -> Type.t -> unit
val add_module: t -> string -> Type.t -> unit
val add_property_map: t -> Type.Properties.id -> Type.Properties.t -> unit
val add_export_map: t -> Type.Exports.id -> Type.Exports.t -> unit
val add_tvar: t -> Constraint.ident -> Constraint.node -> unit
val add_nominal_id: t -> Constraint.ident -> unit
val remove_all_errors: t -> unit
val remove_all_error_suppressions: t -> unit
val remove_all_lint_severities: t -> unit
val remove_tvar: t -> Constraint.ident -> unit
val set_declare_module_t: t -> Type.t option -> unit
val set_envs: t -> env IMap.t -> unit
val set_evaluated: t  -> Type.t IMap.t -> unit
val set_type_graph: t  -> Graph_explorer.graph -> unit
val set_all_unresolved: t  -> ISet.t IMap.t -> unit
val set_globals: t -> SSet.t -> unit
val set_graph: t -> Constraint.node IMap.t -> unit
val set_errors: t -> Errors.ErrorSet.t -> unit
val set_error_suppressions: t -> Error_suppressions.t -> unit
val set_severity_cover: t -> ExactCover.lint_severity_cover -> unit
val set_module_kind: t -> module_kind -> unit
val set_property_maps: t -> Type.Properties.map -> unit
val set_export_maps: t -> Type.Exports.map -> unit
val set_tvar: t -> Constraint.ident -> Constraint.node -> unit
val set_unused_lint_suppressions: t -> Loc.LocSet.t -> unit
val set_exists_checks: t -> ExistsCheck.t LocMap.t -> unit
val set_exists_excuses: t -> ExistsCheck.t LocMap.t -> unit
val set_dep_map: t -> Dep_mapper.Dep.t Dep_mapper.DepMap.t -> unit
val set_use_def_map: t -> Loc.t LocMap.t -> unit
val set_module_map: t -> Type.t SMap.t -> unit

val clear_intermediates: t -> unit

(* utils *)
val iter_props: t -> Type.Properties.id -> (string -> Type.Property.t -> unit) -> unit
val has_prop: t -> Type.Properties.id -> string -> bool
val get_prop: t -> Type.Properties.id -> string -> Type.Property.t option
val set_prop: t -> Type.Properties.id -> string -> Type.Property.t -> unit
val has_export: t -> Type.Exports.id -> string -> bool
val set_export: t -> Type.Exports.id -> string -> Type.t -> unit

(* constructors *)
val make_property_map: t -> Type.Properties.t -> Type.Properties.id
val make_export_map: t -> Type.Exports.t -> Type.Exports.id
val make_nominal: t -> int
