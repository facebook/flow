(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections

module Get_def_result : sig
  type t =
    | Def of LocSet.t * string option
    | Partial of LocSet.t * string option * string
    | Bad_loc of string
    | Def_error of string
end

val get_def :
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  cx:Context.t ->
  file_sig:File_sig.t ->
  ?file_content:string ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  available_ast:Typed_ast_utils.available_ast ->
  purpose:Get_def_types.Purpose.t ->
  Loc.t ->
  Get_def_result.t
