(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

type stack = int list
type closure = stack * Scope.t list

type metadata = {
  file: Loc.filename;
  _module: string;
  checked: bool;
  weak: bool;
  verbose: int option;
}

(* TODO this has a bunch of stuff in it that should be localized *)
type t = {
  metadata: metadata;

  (* required modules, and map to their locations *)
  mutable required: SSet.t;
  mutable require_loc: Loc.t SMap.t;
  mutable module_exports_type: module_exports_type;

  (* map from tvar ids to nodes (type info structures) *)
  mutable graph: Constraint_js.node IMap.t;

  (* obj types point to mutable property maps *)
  mutable property_maps: Type.properties IMap.t;

  (* map from closure ids to env snapshots *)
  mutable closures: closure IMap.t;

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

(* create a new context structure.
   Flow_js.fresh_context prepares for actual use.
 *)
let make metadata = {
  metadata;

  required = SSet.empty;
  require_loc = SMap.empty;
  module_exports_type = CommonJSModule(None);

  graph = IMap.empty;
  closures = IMap.empty;
  property_maps = IMap.empty;
  modulemap = SMap.empty;

  errors = Errors_js.ErrorSet.empty;
  globals = SSet.empty;

  error_suppressions = Errors_js.ErrorSuppressions.empty;

  type_table = Hashtbl.create 0;
  annot_table = Hashtbl.create 0;
}

(* accessors *)
let annot_table cx = cx.annot_table
let closures cx = cx.closures
let errors cx = cx.errors
let error_suppressions cx = cx.error_suppressions
let file cx = cx.metadata.file
let find_props cx id = IMap.find_unsafe id cx.property_maps
let find_module cx m = SMap.find_unsafe m cx.modulemap
let globals cx = cx.globals
let graph cx = cx.graph
let is_checked cx = cx.metadata.checked
let is_verbose cx = cx.metadata.verbose <> None
let is_weak cx = cx.metadata.weak
let module_exports_type cx = cx.module_exports_type
let module_map cx = cx.modulemap
let module_name cx = cx.metadata._module
let property_maps cx = cx.property_maps
let required cx = cx.required
let require_loc cx = cx.require_loc
let type_table cx = cx.type_table
let verbose cx = cx.metadata.verbose

let copy_of_context cx = { cx with
  graph = IMap.map Constraint_js.copy_node cx.graph;
  property_maps = cx.property_maps
}

(* mutators *)
let add_closure cx frame closure =
  cx.closures <- IMap.add frame closure cx.closures
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
let set_closures cx closures =
  cx.closures <- closures
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
