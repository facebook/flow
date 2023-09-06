(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'loc result =
  | OwnDef of 'loc * (* name *) string
  | Request of ('loc, 'loc * (Type.t[@opaque])) Get_def_request.t
  | Empty of string
  | LocNotFound
[@@deriving show]

val process_type_request : Context.t -> Type.t -> (ALoc.t, string) Stdlib.result

val process_location_in_typed_ast :
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  is_legit_require:(ALoc.t * Type.t -> bool) ->
  purpose:Get_def_types.Purpose.t ->
  Loc.t ->
  ALoc.t result
