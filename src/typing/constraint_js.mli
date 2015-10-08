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
open Utils_js

type ident = int

(***************************************)

type node =
| Goto of ident
| Root of root

and root = {
  rank: int;
  constraints: constraints;
}

and constraints =
| Resolved of Type.t
| Unresolved of bounds

and bounds = {
  mutable lower: Trace.t Type.TypeMap.t;
  mutable upper: Trace.t Type.TypeMap.t;
  mutable lowertvars: Trace.t IMap.t;
  mutable uppertvars: Trace.t IMap.t;
}

val new_unresolved_root: unit -> node
val bounds_of_unresolved_root: node -> bounds

val copy_node: node -> node

(***************************************)

type stack = int list

(* TODO this has a bunch of stuff in it that should be localized *)
type context = {
  file: filename;
  _module: string;
  checked: bool;
  weak: bool;
  verbose: int option;

  (* required modules, and map to their locations *)
  mutable required: SSet.t;
  mutable require_loc: Loc.t SMap.t;
  mutable module_exports_type: module_exports_type;

  (* map from tvar ids to nodes (type info structures) *)
  mutable graph: node IMap.t;

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
  file:filename -> _module:string ->
  context

(**************************************)

(* printing *)

val string_of_t: context -> Type.t -> string
val json_of_t: ?depth:int -> context -> Type.t -> Hh_json.json
val jstr_of_t: ?depth:int -> context -> Type.t -> string
val json_of_graph: ?depth:int -> context -> Hh_json.json
val jstr_of_graph: ?depth:int -> context -> string

val parameter_name: context -> string -> Type.t -> string
val string_of_param_t: context -> Type.t -> string

val is_printed_type_parsable: ?weak:bool -> context -> Type.t -> bool
val is_printed_param_type_parsable: ?weak:bool -> context -> Type.t -> bool

val json_of_scope: ?depth:int -> context -> Scope.t -> Hh_json.json

class ['a] type_visitor: object
  (* Only exposing a few methods for now. *)
  method type_ : context -> 'a -> Type.t -> 'a
  method id_ : context -> 'a -> ident -> 'a
  method props : context -> 'a -> ident -> 'a
  method fun_type : context -> 'a -> Type.funtype -> 'a
end
