(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val func_details :
  jsdoc:Jsdoc.t option ->
  exact_by_default:bool ->
  (string option * Ty.t * Ty.fun_param) Base.List.t ->
  (string option * Ty.t) option ->
  Ty.t ->
  ServerProt.Response.func_details_result

val find_signatures :
  options:Options.t ->
  reader:State_reader.t ->
  cx:Context.t ->
  file_sig:File_sig.With_ALoc.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  Loc.t ->
  ((ServerProt.Response.func_details_result list * int) option, Ty_normalizer.error) result
