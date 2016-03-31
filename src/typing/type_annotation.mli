(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** services for producing types from annotations,
    called during AST traversal.
  *)

val convert: Context.t ->
  Type.t SMap.t ->
  Spider_monkey_ast.Type.t ->
  Type.t

val convert_qualification: ?lookup_mode:Env_js.LookupMode.t ->
  Context.t ->
  string ->
  Spider_monkey_ast.Type.Generic.Identifier.t ->
  Type.t

val mk_rest: Context.t -> Type.t -> Type.t

val mk_type_annotation: Context.t ->
  Type.t SMap.t ->
  Reason_js.t ->
  (Loc.t * Spider_monkey_ast.Type.t) option ->
  Type.t

val mk_keys_type: Reason_js.t -> string list -> Type.t

val mk_nominal_type: ?for_type:bool ->
  Context.t ->
  Reason_js.t ->
  Type.t SMap.t ->
  (Type.t * Spider_monkey_ast.Type.t list) ->
  Type.t

val mk_type_param_declarations: Context.t ->
  Type.t SMap.t ->
  Spider_monkey_ast.Type.ParameterDeclaration.t option ->
  (Type.typeparam list * Type.t SMap.t)

val extract_type_param_instantiations:
  Spider_monkey_ast.Type.ParameterInstantiation.t option ->
  Spider_monkey_ast.Type.t list
