(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type result =
  | OwnDef of ALoc.t
  | Request of Get_def_request.t
  | LocNotFound

val process_location :
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  is_legit_require:(ALoc.t -> bool) ->
  module_ref_prefix:string option ->
  Loc.t ->
  result
