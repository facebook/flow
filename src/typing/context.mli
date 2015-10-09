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

(* TODO this has a bunch of stuff in it that should be localized *)
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

val new_context:
  ?checked:bool -> ?weak:bool -> verbose:int option ->
  file:Loc.filename -> _module:string ->
  t
