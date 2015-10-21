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
open Utils_js

val apply_docblock_overrides:
  Context.metadata ->
  Docblock.t ->
  Context.metadata

val infer_ast:
  ?gc: bool ->
  metadata: Context.metadata ->
  filename: Loc.filename ->
  module_name: string ->
  Spider_monkey_ast.program ->
  Context.t

val infer_module: metadata: Context.metadata -> filename -> Context.t

val merge_component_strict: Context.t list ->
  Context.t list -> (Context.t * string * Context.t) list ->
  Context.t list Utils.SMap.t -> Context.t ->
  unit

val restore: Context.t ->
  Context.t list -> Context.t -> unit

val mk_object: Context.t -> reason -> Type.t

val query_type: Context.t -> Loc.t -> Loc.t * Type.t option * Type.t list

val dump_types:
  (Context.t -> Type.t -> string) ->
  (Context.t -> Type.t -> string option) ->
  Context.t ->
  (Loc.t * string * string option * Reason_js.reason list) list

val fill_types: Context.t -> (int * int * string) list

val init_lib_file:
  verbose: int option ->
  filename ->
  Spider_monkey_ast.Statement.t list ->
  Spider_monkey_ast.Comment.t list ->
  (Errors_js.ErrorSet.t -> unit) ->
  (Errors_js.ErrorSuppressions.t -> unit) ->
  unit
