(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocMap = Utils_js.LocMap

exception Props_not_found of Type.Properties.id
exception Exports_not_found of Type.Exports.id
exception Require_not_found of string
exception Module_not_found of string
exception Tvar_not_found of Constraint.ident

type env = Scope.t list

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

module Global = struct
  type t = {
    metadata: global_metadata;
  }

  let enable_const_params t = t.metadata.enable_const_params
  let enable_unsafe_getters_and_setters t = t.metadata.enable_unsafe_getters_and_setters
  let enforce_strict_type_args t = t.metadata.enforce_strict_type_args
  let enforce_strict_call_arity t = t.metadata.enforce_strict_call_arity
  let esproposal_class_static_fields t = t.metadata.esproposal_class_static_fields
  let esproposal_class_instance_fields t = t.metadata.esproposal_class_instance_fields
  let esproposal_decorators t = t.metadata.esproposal_decorators
  let esproposal_export_star_as t = t.metadata.esproposal_export_star_as
  let max_trace_depth t = t.metadata.max_trace_depth
  let root t = t.metadata.root
  let facebook_fbt t = t.metadata.facebook_fbt
  let should_ignore_non_literal_requires t = t.metadata.ignore_non_literal_requires
  let should_strip_root t = t.metadata.strip_root
  let suppress_comments t = t.metadata.suppress_comments
  let suppress_types t = t.metadata.suppress_types
  let max_workers t = t.metadata.max_workers
end

type local_t = {
  file: File_key.t;
  module_ref: string;
  metadata: local_metadata;

  mutable module_kind: module_kind;

  mutable import_stmts: Loc.t Ast.Statement.ImportDeclaration.t list;
  mutable imported_ts: Type.t SMap.t;

  (* map from tvar ids to nodes (type info structures) *)
  mutable graph: Constraint.node IMap.t;

  (* set of "nominal" ids (created by Flow_js.mk_nominal_id) *)
  (** Nominal ids are used to identify classes and to check nominal subtyping
      between classes. They are different from other "structural" ids, used to
      identify type variables and property maps, where subtyping cares about the
      underlying types rather than the ids themselves. We track nominal ids in
      the context to help decide when the types exported by a module have
      meaningfully changed: see Merge_js.ContextOptimizer. **)
  mutable nominal_ids: ISet.t;

  (* obj types point to mutable property maps *)
  mutable property_maps: Type.Properties.map;

  (* modules point to mutable export maps *)
  mutable export_maps: Type.Exports.map;

  (* map from evaluation ids to types *)
  mutable evaluated: Type.t IMap.t;

  (* graph tracking full resolution of types *)
  mutable type_graph: Graph_explorer.graph;

  (* map of speculation ids to sets of unresolved tvars *)
  mutable all_unresolved: ISet.t IMap.t;

  (* map from frame ids to env snapshots *)
  mutable envs: env IMap.t;

  mutable require_map: Type.t SMap.t;

  (* map from module names to their types *)
  mutable module_map: Type.t SMap.t;

  mutable errors: Errors.ErrorSet.t;
  mutable globals: SSet.t;

  mutable error_suppressions: Error_suppressions.t;
  mutable severity_cover: ExactCover.lint_severity_cover;

  type_table: Type_table.t;
  annot_table: (Loc.t, Type.t) Hashtbl.t;
  refs_table: (Loc.t, Loc.t) Hashtbl.t;

  mutable declare_module_t: Type.t option;

  (* map from exists proposition locations to the types of values running through them *)
  mutable exists_checks: ExistsCheck.t LocMap.t;
  (* map from exists proposition locations to the types of excuses for them *)
  (* If a variable appears in something like `x || ''`, the existence check
   * is excused and not considered sketchy. (The program behaves identically to how it would
   * if the null check was made explicit (`x == null ? '' : x`), and this is a fairly
   * common pattern. Excusing it eliminates a lot of noise from the lint rule. *)
  (* The above example assumes that x is a string. If it were a different type
   * it wouldn't be excused. *)
  mutable exists_excuses: ExistsCheck.t LocMap.t;

  mutable dep_map: Dep_mapper.Dep.t Dep_mapper.DepMap.t;
  mutable use_def_map : Loc.t LocMap.t;
}

type cacheable_t = local_t

(* TODO this has a bunch of stuff in it that should be localized *)
type t = {
  global: Global.t;
  local: local_t;
}

let global_metadata_of_options options = {
  enable_const_params = Options.enable_const_params options;
  enable_unsafe_getters_and_setters = Options.enable_unsafe_getters_and_setters options;
  enforce_strict_call_arity = Options.enforce_strict_call_arity options;
  enforce_strict_type_args = Options.enforce_strict_type_args options;
  esproposal_class_instance_fields = Options.esproposal_class_instance_fields options;
  esproposal_class_static_fields = Options.esproposal_class_static_fields options;
  esproposal_decorators = Options.esproposal_decorators options;
  esproposal_export_star_as = Options.esproposal_export_star_as options;
  facebook_fbt = Options.facebook_fbt options;
  ignore_non_literal_requires = Options.should_ignore_non_literal_requires options;
  max_trace_depth = Options.max_trace_depth options;
  max_workers = Options.max_workers options;
  root = Options.root options;
  strip_root = Options.should_strip_root options;
  suppress_comments = Options.suppress_comments options;
  suppress_types = Options.suppress_types options;
}

let metadata_of_options options =
  let global_metadata = global_metadata_of_options options in
  let local_metadata = {
    checked = Options.all options;
    munge_underscores = Options.should_munge_underscores options;
    verbose = Options.verbose options;
    weak = Options.weak_by_default options;
    jsx = None;
    strict = false;
  } in
  { global_metadata; local_metadata; }

(* create a new context structure.
   Flow_js.fresh_context prepares for actual use.
 *)
let make metadata file module_ref = {
  global = {
    Global.metadata = metadata.global_metadata;
  };
  local = {
    file;
    module_ref;
    metadata = metadata.local_metadata;

    module_kind = CommonJSModule(None);

    import_stmts = [];
    imported_ts = SMap.empty;

    graph = IMap.empty;
    nominal_ids = ISet.empty;
    envs = IMap.empty;
    property_maps = Type.Properties.Map.empty;
    export_maps = Type.Exports.Map.empty;
    evaluated = IMap.empty;
    type_graph = Graph_explorer.new_graph ISet.empty;
    all_unresolved = IMap.empty;
    require_map = SMap.empty;
    module_map = SMap.empty;

    errors = Errors.ErrorSet.empty;
    globals = SSet.empty;

    error_suppressions = Error_suppressions.empty;
    severity_cover = ExactCover.empty;

    type_table = Type_table.create ();
    annot_table = Hashtbl.create 0;
    refs_table = Hashtbl.create 0;

    declare_module_t = None;

    exists_checks = LocMap.empty;
    exists_excuses = LocMap.empty;

    dep_map = Dep_mapper.DepMap.empty;
    use_def_map = LocMap.empty;
  }
}

(* accessors *)
let all_unresolved cx = cx.local.all_unresolved
let annot_table cx = cx.local.annot_table
let declare_module_t cx = cx.local.declare_module_t
let envs cx = cx.local.envs
let enable_const_params cx = Global.enable_const_params cx.global
let enable_unsafe_getters_and_setters cx = Global.enable_unsafe_getters_and_setters cx.global
let enforce_strict_type_args cx = Global.enforce_strict_type_args cx.global
let enforce_strict_call_arity cx = Global.enforce_strict_call_arity cx.global
let errors cx = cx.local.errors
let error_suppressions cx = cx.local.error_suppressions
let esproposal_class_static_fields cx = Global.esproposal_class_static_fields cx.global
let esproposal_class_instance_fields cx = Global.esproposal_class_instance_fields cx.global
let esproposal_decorators cx = Global.esproposal_decorators cx.global
let esproposal_export_star_as cx = Global.esproposal_export_star_as cx.global
let evaluated cx = cx.local.evaluated
let file cx = cx.local.file
let find_props cx id =
  try Type.Properties.Map.find_unsafe id cx.local.property_maps
  with Not_found -> raise (Props_not_found id)
let find_exports cx id =
  try Type.Exports.Map.find_unsafe id cx.local.export_maps
  with Not_found -> raise (Exports_not_found id)
let find_require cx r =
  try SMap.find_unsafe r cx.local.require_map
  with Not_found -> raise (Require_not_found r)
let find_module cx m =
  try SMap.find_unsafe m cx.local.module_map
  with Not_found -> raise (Module_not_found m)
let find_tvar cx id =
  try IMap.find_unsafe id cx.local.graph
  with Not_found -> raise (Tvar_not_found id)
let mem_nominal_id cx id = ISet.mem id cx.local.nominal_ids
let globals cx = cx.local.globals
let graph cx = cx.local.graph
let import_stmts cx = cx.local.import_stmts
let imported_ts cx = cx.local.imported_ts
let is_checked cx = cx.local.metadata.checked
let is_verbose cx = cx.local.metadata.verbose <> None
let is_weak cx = cx.local.metadata.weak
let severity_cover cx = cx.local.severity_cover
let max_trace_depth cx = Global.max_trace_depth cx.global
let module_kind cx = cx.local.module_kind
let require_map cx = cx.local.require_map
let module_map cx = cx.local.module_map
let module_ref cx = cx.local.module_ref
let property_maps cx = cx.local.property_maps
let refs_table cx = cx.local.refs_table
let export_maps cx = cx.local.export_maps
let root cx = Global.root cx.global
let facebook_fbt cx = Global.facebook_fbt cx.global
let should_ignore_non_literal_requires cx = Global.should_ignore_non_literal_requires cx.global
let should_munge_underscores cx  = cx.local.metadata.munge_underscores
let should_strip_root cx = Global.should_strip_root cx.global
let suppress_comments cx = Global.suppress_comments cx.global
let suppress_types cx = Global.suppress_types cx.global
let type_graph cx = cx.local.type_graph
let type_table cx = cx.local.type_table
let verbose cx = cx.local.metadata.verbose
let max_workers cx = Global.max_workers cx.global
let jsx cx = cx.local.metadata.jsx
let exists_checks cx = cx.local.exists_checks
let exists_excuses cx = cx.local.exists_excuses
let dep_map cx = cx.local.dep_map
let use_def_map cx = cx.local.use_def_map

let pid_prefix (cx: t) =
  if max_workers cx > 0
  then Printf.sprintf "[%d] " (Unix.getpid ())
  else ""

let copy_of_context (cx: t) =
  let local = { cx.local with
    graph = IMap.map Constraint.copy_node cx.local.graph;
    property_maps = cx.local.property_maps;
    type_table = Type_table.copy cx.local.type_table;
  } in
  { cx with local }

(* mutators *)
let add_env cx frame env =
  cx.local.envs <- IMap.add frame env cx.local.envs
let add_error cx error =
  cx.local.errors <- Errors.ErrorSet.add error cx.local.errors
let add_error_suppression cx loc =
  cx.local.error_suppressions <-
    Error_suppressions.add loc cx.local.error_suppressions
let add_global cx name =
  cx.local.globals <- SSet.add name cx.local.globals
let add_import_stmt cx stmt =
  cx.local.import_stmts <- stmt::cx.local.import_stmts
let add_imported_t cx name t =
  cx.local.imported_ts <- SMap.add name t cx.local.imported_ts
let add_require cx name tvar =
  cx.local.require_map <- SMap.add name tvar cx.local.require_map
let add_module cx name tvar =
  cx.local.module_map <- SMap.add name tvar cx.local.module_map
let add_property_map cx id pmap =
  cx.local.property_maps <- Type.Properties.Map.add id pmap cx.local.property_maps
let add_export_map cx id tmap =
  cx.local.export_maps <- Type.Exports.Map.add id tmap cx.local.export_maps
let add_tvar cx id bounds =
  cx.local.graph <- IMap.add id bounds cx.local.graph
let add_nominal_id cx id =
  cx.local.nominal_ids <- ISet.add id cx.local.nominal_ids
let remove_all_errors cx =
  cx.local.errors <- Errors.ErrorSet.empty
let remove_all_error_suppressions cx =
  cx.local.error_suppressions <- Error_suppressions.empty
let remove_all_lint_severities cx =
  cx.local.severity_cover <- ExactCover.empty
let remove_tvar cx id =
  cx.local.graph <- IMap.remove id cx.local.graph
let set_all_unresolved cx all_unresolved =
  cx.local.all_unresolved <- all_unresolved
let set_declare_module_t cx t =
  cx.local.declare_module_t <- t
let set_envs cx envs =
  cx.local.envs <- envs
let set_evaluated cx evaluated =
  cx.local.evaluated <- evaluated
let set_globals cx globals =
  cx.local.globals <- globals
let set_graph cx graph =
  cx.local.graph <- graph
let set_errors cx errors =
  cx.local.errors <- errors
let set_error_suppressions cx suppressions =
  cx.local.error_suppressions <- suppressions
let set_severity_cover cx severity_cover =
  cx.local.severity_cover <- severity_cover
let set_module_kind cx module_kind =
  cx.local.module_kind <- module_kind
let set_property_maps cx property_maps =
  cx.local.property_maps <- property_maps
let set_export_maps cx export_maps =
  cx.local.export_maps <- export_maps
let set_type_graph cx type_graph =
  cx.local.type_graph <- type_graph
let set_tvar cx id node =
  cx.local.graph <- IMap.add id node cx.local.graph
let set_unused_lint_suppressions cx suppressions = cx.local.error_suppressions <-
  Error_suppressions.set_unused_lint_suppressions suppressions cx.local.error_suppressions
let set_exists_checks cx exists_checks =
  cx.local.exists_checks <- exists_checks
let set_exists_excuses cx exists_excuses =
  cx.local.exists_excuses <- exists_excuses
let set_dep_map cx dep_map =
  cx.local.dep_map <- dep_map
let set_use_def_map cx use_def_map =
  cx.local.use_def_map <- use_def_map
let set_module_map cx module_map =
  cx.local.module_map <- module_map

let clear_intermediates cx =
  (* call reset instead of clear to also shrink the bucket tables *)
  Type_table.reset cx.local.type_table;
  Hashtbl.reset cx.local.annot_table;
  cx.local.all_unresolved <- IMap.empty;
  cx.local.exists_checks <- LocMap.empty;
  cx.local.exists_excuses <- LocMap.empty;
  cx.local.dep_map <- Dep_mapper.DepMap.empty;
  cx.local.use_def_map <- LocMap.empty;
  cx.local.require_map <- SMap.empty


(* utils *)
let iter_props cx id f =
  find_props cx id
  |> SMap.iter f

let has_prop cx id x =
  find_props cx id
  |> SMap.mem x

let get_prop cx id x =
  find_props cx id
  |> SMap.get x

let set_prop cx id x p =
  find_props cx id
  |> SMap.add x p
  |> add_property_map cx id

let has_export cx id name =
  find_exports cx id |> SMap.mem name

let set_export cx id name t =
  find_exports cx id
  |> SMap.add name t
  |> add_export_map cx id

(* constructors *)
let make_property_map cx pmap =
  let id = Type.Properties.mk_id () in
  add_property_map cx id pmap;
  id

let make_export_map cx tmap =
  let id = Type.Exports.mk_id () in
  add_export_map cx id tmap;
  id

let make_nominal cx =
  let nominal = Reason.mk_id () in
  add_nominal_id cx nominal;
  nominal

(* Copy context from cx_other to cx *)
let merge_into cx cx_other =
  set_envs cx (IMap.union (envs cx_other) (envs cx));
  set_property_maps cx (
    Type.Properties.Map.union (property_maps cx_other) (property_maps cx));
  set_export_maps cx (
    Type.Exports.Map.union (export_maps cx_other) (export_maps cx));
  set_evaluated cx (IMap.union (evaluated cx_other) (evaluated cx));
  set_type_graph cx (Graph_explorer.union (type_graph cx_other) (type_graph cx));
  set_all_unresolved cx (IMap.union (all_unresolved cx_other) (all_unresolved cx));
  set_globals cx (SSet.union (globals cx_other) (globals cx));
  set_graph cx (IMap.union (graph cx_other) (graph cx));
  set_errors cx (Errors.ErrorSet.union (errors cx_other) (errors cx));
  set_error_suppressions cx (Error_suppressions.union (error_suppressions cx_other) (error_suppressions cx));
  set_severity_cover cx (ExactCover.union (severity_cover cx_other) (severity_cover cx));
  set_exists_checks cx (LocMap.union (exists_checks cx_other) (exists_checks cx));
  set_exists_excuses cx (LocMap.union (exists_excuses cx_other) (exists_excuses cx))
  (* TODO: merge use_def_map and dep_map as well. *)

let to_cache cx = cx.local
let from_cache ~options local =
  {
    global = { Global.metadata = global_metadata_of_options options };
    local;
  }
