(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reason_js
open Constraint_js
open Utils_js

val infer_ast:
  ?module_name: string ->
  force_check: bool ->
  weak_by_default: bool ->
  verbose: int option ->
  Spider_monkey_ast.program ->
  filename ->
  context

val infer_module:
  force_check:bool ->
  weak_by_default: bool ->
  verbose: int option ->
  filename ->
  context

val merge_component_strict: context list ->
  context list -> (context * string * context) list ->
  context list Utils.SMap.t -> context ->
  unit

val restore: context ->
  context list -> context -> unit

val mk_object: context -> reason -> Type.t

val query_type: context -> Loc.t -> Loc.t * Type.t option * Type.t list

val dump_types:
  (Constraint_js.context -> Type.t -> string) ->
  (Constraint_js.context -> Type.t -> string option) ->
  context ->
  (Loc.t * string * string option * Reason_js.reason list) list

val fill_types: context -> (int * int * string) list

val init_lib_file:
  verbose: int option ->
  filename ->
  Spider_monkey_ast.Statement.t list ->
  Spider_monkey_ast.Comment.t list ->
  (Errors_js.ErrorSet.t -> unit) ->
  (Errors_js.ErrorSuppressions.t -> unit) ->
  unit
