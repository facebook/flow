(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type env = Scope.t list

type metadata = {
  checked: bool;
  enable_const_params: bool;
  enable_unsafe_getters_and_setters: bool;
  esproposal_class_static_fields: Options.esproposal_feature_mode;
  esproposal_class_instance_fields: Options.esproposal_feature_mode;
  esproposal_decorators: Options.esproposal_feature_mode;
  esproposal_export_star_as: Options.esproposal_feature_mode;
  facebook_ignore_fbt: bool;
  ignore_non_literal_requires: bool;
  max_trace_depth: int;
  munge_underscores: bool;
  root: Path.t;
  strip_root: bool;
  suppress_comments: Str.regexp list;
  suppress_types: SSet.t;
  verbose: int option;
  weak: bool;
}

(* TODO this has a bunch of stuff in it that should be localized *)
type t = {
  file: Loc.filename;
  module_name: Modulename.t;
  metadata: metadata;

  (* required modules, and map to their locations *)
  mutable required: SSet.t;
  mutable require_loc: Loc.t SMap.t;
  mutable module_exports_type: module_exports_type;

  (* map from tvar ids to nodes (type info structures) *)
  mutable graph: Constraint_js.node IMap.t;

  (* obj types point to mutable property maps *)
  mutable property_maps: Type.properties IMap.t;

  (* map from evaluation ids to types *)
  mutable evaluated: Type.t IMap.t;

  (* map from frame ids to env snapshots *)
  mutable envs: env IMap.t;

  (* map from module names to their types *)
  mutable modulemap: Type.t SMap.t;

  mutable errors: Errors_js.ErrorSet.t;
  mutable globals: SSet.t;

  mutable error_suppressions: Errors_js.ErrorSuppressions.t;

  type_table: (Loc.t, Type.t) Hashtbl.t;
  annot_table: (Loc.t, Type.t) Hashtbl.t;
}

and module_exports_type =
  | CommonJSModule of Loc.t option
  | ESModule

let metadata_of_options options = {
  checked = Options.all options;
  enable_const_params = Options.enable_const_params options;
  enable_unsafe_getters_and_setters =
    Options.enable_unsafe_getters_and_setters options;
  esproposal_class_static_fields =
    Options.esproposal_class_static_fields options;
  esproposal_class_instance_fields =
    Options.esproposal_class_instance_fields options;
  esproposal_decorators = Options.esproposal_decorators options;
  esproposal_export_star_as = Options.esproposal_export_star_as options;
  facebook_ignore_fbt = Options.should_ignore_fbt options;
  ignore_non_literal_requires =
    Options.should_ignore_non_literal_requires options;
  max_trace_depth = Options.max_trace_depth options;
  munge_underscores = Options.should_munge_underscores options;
  root = Options.root options;
  strip_root = Options.should_strip_root options;
  suppress_comments = Options.suppress_comments options;
  suppress_types = Options.suppress_types options;
  verbose = Options.verbose options;
  weak = Options.weak_by_default options;
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
  module_exports_type = CommonJSModule(None);

  graph = IMap.empty;
  envs = IMap.empty;
  property_maps = IMap.empty;
  evaluated = IMap.empty;
  modulemap = SMap.empty;

  errors = Errors_js.ErrorSet.empty;
  globals = SSet.empty;

  error_suppressions = Errors_js.ErrorSuppressions.empty;

  type_table = Hashtbl.create 0;
  annot_table = Hashtbl.create 0;
}

(* accessors *)
let annot_table cx = cx.annot_table
let envs cx = cx.envs
let enable_const_params cx = cx.metadata.enable_const_params
let enable_unsafe_getters_and_setters cx =
  cx.metadata.enable_unsafe_getters_and_setters
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
let find_props cx id = IMap.find_unsafe id cx.property_maps
let find_module cx m = SMap.find_unsafe m cx.modulemap
let globals cx = cx.globals
let graph cx = cx.graph
let is_checked cx = cx.metadata.checked
let is_verbose cx = cx.metadata.verbose <> None
let is_weak cx = cx.metadata.weak
let max_trace_depth cx = cx.metadata.max_trace_depth
let module_exports_type cx = cx.module_exports_type
let module_map cx = cx.modulemap
let module_name cx = cx.module_name
let property_maps cx = cx.property_maps
let required cx = cx.required
let require_loc cx = cx.require_loc
let root cx = cx.metadata.root
let should_ignore_fbt cx = cx.metadata.facebook_ignore_fbt
let should_ignore_non_literal_requires cx =
  cx.metadata.ignore_non_literal_requires
let should_munge_underscores cx  = cx.metadata.munge_underscores
let should_strip_root cx = cx.metadata.strip_root
let suppress_comments cx = cx.metadata.suppress_comments
let suppress_types cx = cx.metadata.suppress_types
let type_table cx = cx.type_table
let verbose cx = cx.metadata.verbose

let copy_of_context cx = { cx with
  graph = IMap.map Constraint_js.copy_node cx.graph;
  property_maps = cx.property_maps
}

(* mutators *)
let add_env cx frame env =
  cx.envs <- IMap.add frame env cx.envs
let add_error cx error =
  cx.errors <- Errors_js.ErrorSet.add error cx.errors
let add_error_suppression cx loc =
  cx.error_suppressions <-
    Errors_js.ErrorSuppressions.add loc cx.error_suppressions
let add_global cx name =
  cx.globals <- SSet.add name cx.globals
let add_module cx name tvar =
  cx.modulemap <- SMap.add name tvar cx.modulemap
let add_property_map cx id pmap =
  cx.property_maps <- IMap.add id pmap cx.property_maps
let add_require cx name loc =
  cx.required <- SSet.add name cx.required;
  cx.require_loc <- SMap.add name loc cx.require_loc
let add_tvar cx id bounds =
  cx.graph <- IMap.add id bounds cx.graph
let remove_all_errors cx =
  cx.errors <- Errors_js.ErrorSet.empty
let remove_all_error_suppressions cx =
  cx.error_suppressions <- Errors_js.ErrorSuppressions.empty
let remove_tvar cx id =
  cx.graph <- IMap.remove id cx.graph
let set_envs cx envs =
  cx.envs <- envs
let set_evaluated cx evaluated =
  cx.evaluated <- evaluated
let set_globals cx globals =
  cx.globals <- globals
let set_graph cx graph =
  cx.graph <- graph
let set_module_exports_type cx module_exports_type =
  cx.module_exports_type <- module_exports_type
let set_property_maps cx property_maps =
  cx.property_maps <- property_maps
let set_tvar cx id node =
  cx.graph <- IMap.add id node cx.graph

(* constructors *)
let make_property_map cx pmap =
  let id = Reason_js.mk_id () in
  add_property_map cx id pmap;
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
  set_property_maps cx (IMap.union (property_maps cx_other) (property_maps cx));
  set_evaluated cx (IMap.union (evaluated cx_other) (evaluated cx));
  set_globals cx (SSet.union (globals cx_other) (globals cx));
  set_graph cx (IMap.union (graph cx_other) (graph cx))
