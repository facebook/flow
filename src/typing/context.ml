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

type t = {
  file: Loc.filename;
  _module: string;
  checked: bool;
  weak: bool;
  verbose: int option;

  (* required modules, and map to their locations *)
  mutable required: SSet.t;
  mutable require_loc: Loc.t SMap.t;
  mutable module_exports_type: module_exports_type;

  (* map from tvar ids to nodes (type info structures) *)
  mutable graph: Constraint_js.node IMap.t;

  (* obj types point to mutable property maps *)
  mutable property_maps: Type.properties IMap.t;

  (* map from closure ids to env snapshots *)
  mutable closures: (stack * Scope.t list) IMap.t;

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
let new_context ?(checked=false) ?(weak=false) ~verbose ~file ~_module = {
  file;
  _module;
  checked;
  weak;
  verbose;

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
