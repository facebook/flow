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

val infer_ast:
  Spider_monkey_ast.program ->
  string ->                     (* filename *)
  string ->                     (* module name *)
  bool ->                       (* force_check *)
  context

val infer_module: string -> context

val merge_module_list: context list -> unit

val merge_module_strict: context ->
  context list -> (context * context) list -> (context * string) list ->
  context -> unit

val mk_object: context -> reason -> Type.t

val body_loc: Spider_monkey_ast.Statement.FunctionDeclaration.body ->
    Spider_monkey_ast.Loc.t

val query_type: context -> Pos.t -> Pos.t * Type.t option * Type.t list

val fill_types: context -> (int * int * string) list

val init:
  string ->
  Spider_monkey_ast.Statement.t list ->
  (Errors_js.ErrorSet.t -> unit) ->
  unit
