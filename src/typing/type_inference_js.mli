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

val infer_ast:
  ?gc: bool ->
  metadata: Context.metadata ->
  filename: Loc.filename ->
  module_name: Modulename.t ->
  Spider_monkey_ast.program ->
  Context.t

val merge_component_strict: Context.t list ->
  Context.t list -> (Context.t * string * Modulename.t * Context.t) list ->
  (string * Modulename.t * Context.t) list -> Context.t ->
  unit

val restore: Context.t ->
  Context.t list -> Context.t -> unit

val mk_object: Context.t -> reason -> Type.t

val load_lib_file:
  verbose: int option ->
  exclude_syms:Utils.SSet.t ->
  filename ->
  Spider_monkey_ast.Statement.t list ->
  Spider_monkey_ast.Comment.t list ->
  (filename -> Errors_js.ErrorSet.t -> unit) ->
  (filename -> Errors_js.ErrorSuppressions.t -> unit) ->
  string list
