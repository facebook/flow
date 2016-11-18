(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast

type env = Scope.t list

type metadata = {
  checked: bool;
  enable_const_params: bool;
  enable_unsafe_getters_and_setters: bool;
  enforce_strict_type_args: bool;
  esproposal_class_static_fields: Options.esproposal_feature_mode;
  esproposal_class_instance_fields: Options.esproposal_feature_mode;
  esproposal_decorators: Options.esproposal_feature_mode;
  esproposal_export_star_as: Options.esproposal_feature_mode;
  facebook_fbt: string option;
  ignore_non_literal_requires: bool;
  max_trace_depth: int;
  munge_underscores: bool;
  output_graphml: bool;
  root: Path.t;
  strip_root: bool;
  suppress_comments: Str.regexp list;
  suppress_types: SSet.t;
  verbose: Verbose.t option;
  weak: bool;
  max_workers: int;
  jsx: (string * Spider_monkey_ast.Expression.t) option;
}

(* TODO this has a bunch of stuff in it that should be localized *)
type t = {
  file: Loc.filename;
  module_name: Modulename.t;
  metadata: metadata;

  (* required modules, and map to their locations *)
  mutable required: SSet.t;
  mutable require_loc: Loc.t SMap.t;
  mutable module_kind: module_kind;

  mutable import_stmts: Ast.Statement.ImportDeclaration.t list;
  mutable imported_ts: Type.t SMap.t;

  (* map from tvar ids to nodes (type info structures) *)
  mutable graph: Constraint.node IMap.t;

  (* map from tvar ids to reasons *)
  mutable tvar_reasons: Reason.t IMap.t;

  (* obj types point to mutable property maps *)
  mutable property_maps: Type.Properties.map;

  (* modules point to mutable export maps *)
  mutable export_maps: Type.Exports.map;

  (* map from evaluation ids to types *)
  mutable evaluated: Type.t IMap.t;

  (* graph tracking full resolution of types *)
  mutable type_graph: Graph_explorer.graph;

  (* map of speculation ids to sets of unresolved tvars *)
  mutable all_unresolved: Type.TypeSet.t IMap.t;

  (* map from frame ids to env snapshots *)
  mutable envs: env IMap.t;

  (* map from module names to their types *)
  mutable modulemap: Type.t SMap.t;

  mutable errors: Errors.ErrorSet.t;
  mutable globals: SSet.t;

  mutable error_suppressions: Errors.ErrorSuppressions.t;

  type_table: (Loc.t, Type.t) Hashtbl.t;
  annot_table: (Loc.t, Type.t) Hashtbl.t;

  mutable declare_module_t: Type.t option;
}

and module_kind =
  | CommonJSModule of Loc.t option
  | ESModule

let metadata_of_options options = {
  checked = Options.all options;
  enable_const_params = Options.enable_const_params options;
  enable_unsafe_getters_and_setters =
    Options.enable_unsafe_getters_and_setters options;
  enforce_strict_type_args =
    Options.enforce_strict_type_args options;
  esproposal_class_static_fields =
    Options.esproposal_class_static_fields options;
  esproposal_class_instance_fields =
    Options.esproposal_class_instance_fields options;
  esproposal_decorators = Options.esproposal_decorators options;
  esproposal_export_star_as = Options.esproposal_export_star_as options;
  facebook_fbt = Options.facebook_fbt options;
  ignore_non_literal_requires =
    Options.should_ignore_non_literal_requires options;
  max_trace_depth = Options.max_trace_depth options;
  munge_underscores = Options.should_munge_underscores options;
  output_graphml = Options.output_graphml options;
  root = Options.root options;
  strip_root = Options.should_strip_root options;
  suppress_comments = Options.suppress_comments options;
  suppress_types = Options.suppress_types options;
  verbose = Options.verbose options;
  weak = Options.weak_by_default options;
  max_workers = Options.max_workers options;
  jsx = None;
}

(* create a new context structure.
   Flow_js.fresh_context prepares for actual use.
 *)
let make metadata file module_name = {
  file;
  module_name;
  metadata;

  required = SSet.empty;
  require_loc = SMap.empty;
  module_kind = CommonJSModule(None);

  import_stmts = [];
  imported_ts = SMap.empty;

  graph = IMap.empty;
  tvar_reasons = IMap.empty;
  envs = IMap.empty;
  property_maps = Type.Properties.Map.empty;
  export_maps = Type.Exports.Map.empty;
  evaluated = IMap.empty;
  type_graph = Graph_explorer.new_graph ISet.empty;
  all_unresolved = IMap.empty;
  modulemap = SMap.empty;

  errors = Errors.ErrorSet.empty;
  globals = SSet.empty;

  error_suppressions = Errors.ErrorSuppressions.empty;

  type_table = Hashtbl.create 0;
  annot_table = Hashtbl.create 0;

  declare_module_t = None;
}

(* accessors *)
let all_unresolved cx = cx.all_unresolved
let annot_table cx = cx.annot_table
let declare_module_t cx = cx.declare_module_t
let envs cx = cx.envs
let enable_const_params cx = cx.metadata.enable_const_params
let enable_unsafe_getters_and_setters cx =
  cx.metadata.enable_unsafe_getters_and_setters
let enforce_strict_type_args cx = cx.metadata.enforce_strict_type_args
let errors cx = cx.errors
let error_suppressions cx = cx.error_suppressions
let esproposal_class_static_fields cx =
  cx.metadata.esproposal_class_static_fields
let esproposal_class_instance_fields cx =
  cx.metadata.esproposal_class_instance_fields
let esproposal_decorators cx = cx.metadata.esproposal_decorators
let esproposal_export_star_as cx = cx.metadata.esproposal_export_star_as
let evaluated cx = cx.evaluated
let file cx = cx.file
let find_props cx id = Type.Properties.Map.find_unsafe id cx.property_maps
let find_exports cx id = Type.Exports.Map.find_unsafe id cx.export_maps
let find_module cx m = SMap.find_unsafe m cx.modulemap
let find_tvar_reason cx id = IMap.find_unsafe id cx.tvar_reasons
let globals cx = cx.globals
let graph cx = cx.graph
let import_stmts cx = cx.import_stmts
let imported_ts cx = cx.imported_ts
let is_checked cx = cx.metadata.checked
let is_verbose cx = cx.metadata.verbose <> None
let is_weak cx = cx.metadata.weak
let max_trace_depth cx = cx.metadata.max_trace_depth
let module_kind cx = cx.module_kind
let module_map cx = cx.modulemap
let module_name cx = cx.module_name
let output_graphml cx = cx.metadata.output_graphml
let property_maps cx = cx.property_maps
let export_maps cx = cx.export_maps
let required cx = cx.required
let require_loc cx = cx.require_loc
let root cx = cx.metadata.root
let facebook_fbt cx = cx.metadata.facebook_fbt
let should_ignore_non_literal_requires cx =
  cx.metadata.ignore_non_literal_requires
let should_munge_underscores cx  = cx.metadata.munge_underscores
let should_strip_root cx = cx.metadata.strip_root
let suppress_comments cx = cx.metadata.suppress_comments
let suppress_types cx = cx.metadata.suppress_types
let type_graph cx = cx.type_graph
let type_table cx = cx.type_table
let verbose cx = cx.metadata.verbose
let max_workers cx = cx.metadata.max_workers
let jsx cx = cx.metadata.jsx

let pid_prefix cx =
  if max_workers cx > 0
  then Printf.sprintf "[%d] " (Unix.getpid ())
  else ""

let copy_of_context cx = { cx with
  graph = IMap.map Constraint.copy_node cx.graph;
  property_maps = cx.property_maps
}

(* mutators *)
let add_env cx frame env =
  cx.envs <- IMap.add frame env cx.envs
let add_error cx error =
  cx.errors <- Errors.ErrorSet.add error cx.errors
let add_error_suppression cx loc =
  cx.error_suppressions <-
    Errors.ErrorSuppressions.add loc cx.error_suppressions
let add_global cx name =
  cx.globals <- SSet.add name cx.globals
let add_import_stmt cx stmt =
  cx.import_stmts <- stmt::cx.import_stmts
let add_imported_t cx name t =
  cx.imported_ts <- SMap.add name t cx.imported_ts
let add_module cx name tvar =
  cx.modulemap <- SMap.add name tvar cx.modulemap
let add_property_map cx id pmap =
  cx.property_maps <- Type.Properties.Map.add id pmap cx.property_maps
let add_export_map cx id tmap =
  cx.export_maps <- Type.Exports.Map.add id tmap cx.export_maps
let add_require cx name loc =
  cx.required <- SSet.add name cx.required;
  cx.require_loc <- SMap.add name loc cx.require_loc
let add_tvar cx id bounds =
  cx.graph <- IMap.add id bounds cx.graph
let add_tvar_reason cx id reason =
  cx.tvar_reasons <- IMap.add id reason cx.tvar_reasons
let remove_all_errors cx =
  cx.errors <- Errors.ErrorSet.empty
let remove_all_error_suppressions cx =
  cx.error_suppressions <- Errors.ErrorSuppressions.empty
let remove_tvar cx id =
  cx.graph <- IMap.remove id cx.graph
let set_all_unresolved cx all_unresolved =
  cx.all_unresolved <- all_unresolved
let set_declare_module_t cx t =
  cx.declare_module_t <- t
let set_envs cx envs =
  cx.envs <- envs
let set_evaluated cx evaluated =
  cx.evaluated <- evaluated
let set_globals cx globals =
  cx.globals <- globals
let set_graph cx graph =
  cx.graph <- graph
let set_module_kind cx module_kind =
  cx.module_kind <- module_kind
let set_property_maps cx property_maps =
  cx.property_maps <- property_maps
let set_export_maps cx export_maps =
  cx.export_maps <- export_maps
let set_type_graph cx type_graph =
  cx.type_graph <- type_graph
let set_tvar cx id node =
  cx.graph <- IMap.add id node cx.graph

let clear_intermediates cx =
  Hashtbl.clear cx.type_table;
  Hashtbl.clear cx.annot_table;
  cx.all_unresolved <- IMap.empty

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

let set_export cx id x t =
  find_exports cx id
  |> SMap.add x t
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

(* Copy context from cx_other to cx *)
let merge_into cx cx_other =
  (* Map.union: which is faster, union M N or union N M when M > N?
     union X Y = fold add X Y which means iterate over X, adding to Y
     So running time is roughly X * log Y.

     Now, when M > N, we have M * log N > N * log M.
     So do union N M as long as N may override M for overlapping keys.
  *)
  set_envs cx (IMap.union (envs cx_other) (envs cx));
  set_property_maps cx (
    Type.Properties.Map.union (property_maps cx_other) (property_maps cx));
  set_export_maps cx (
    Type.Exports.Map.union (export_maps cx_other) (export_maps cx));
  set_evaluated cx (IMap.union (evaluated cx_other) (evaluated cx));
  set_type_graph cx (Graph_explorer.union_finished (type_graph cx_other) (type_graph cx));
  set_all_unresolved cx (IMap.union (all_unresolved cx_other) (all_unresolved cx));
  set_globals cx (SSet.union (globals cx_other) (globals cx));
  set_graph cx (IMap.union (graph cx_other) (graph cx));
