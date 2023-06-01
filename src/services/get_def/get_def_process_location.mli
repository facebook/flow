(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type result =
  | OwnDef of ALoc.t * (* name *) string
  | Request of (ALoc.t, ALoc.t * Type.t) Get_def_request.t
  | Empty of string
  | LocNotFound

val process_type_request :
  Context.t -> ('M, 'T) Get_def_request.t -> Type.t -> (ALoc.t, string) Stdlib.result

val process_location_in_typed_ast :
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  is_legit_require:(ALoc.t * Type.t -> bool) ->
  Loc.t ->
  result
