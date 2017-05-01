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
  Ast.Type.t ->
  Type.t

val convert_qualification: ?lookup_mode:Env.LookupMode.t ->
  Context.t ->
  string ->
  Ast.Type.Generic.Identifier.t ->
  Type.t

val mk_type_annotation: Context.t ->
  Type.t SMap.t ->
  Reason.t ->
  (Loc.t * Ast.Type.t) option ->
  Type.t

val mk_nominal_type: ?for_type:bool ->
  Context.t ->
  Reason.t ->
  Type.t SMap.t ->
  (Type.t * Ast.Type.t list option) ->
  Type.t

val mk_type_param_declarations: Context.t ->
  ?tparams_map:(Type.t SMap.t) ->
  Ast.Type.ParameterDeclaration.t option ->
  (Type.typeparam list * Type.t SMap.t)

val extract_type_param_instantiations:
  Ast.Type.ParameterInstantiation.t option ->
  Ast.Type.t list option

val polarity: Ast.Variance.t option -> Type.polarity
