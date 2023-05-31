(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Get_def_result : sig
  type t =
    | Def of Loc.t list * string option
    | Partial of Loc.t list * string option * string
    | Bad_loc of string
    | Def_error of string
end

val get_def :
  options:Options.t ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  cx:Context.t ->
  file_sig:File_sig.t ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  Loc.t ->
  Get_def_result.t
