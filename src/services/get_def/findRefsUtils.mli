(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ast_info =
  (Loc.t, Loc.t) Flow_ast.Program.t
  * File_sig.With_Loc.t
  * Type_sig_collections.Locs.index Packed_type_sig.Module.t
  * Docblock.t

val compute_docblock : File_key.t -> string (* content *) -> Docblock.t

val compute_ast_result :
  Options.t -> File_key.t -> string (* content *) -> (ast_info, string) result

val get_ast_result : reader:State_reader.t -> File_key.t -> (ast_info, string) result

val get_all_dependents :
  reader:State_reader.t ->
  Options.t ->
  MultiWorkerLwt.worker list option ->
  ServerEnv.env ref ->
  File_key.t ->
  string (* content *) ->
  (* transitive dependents *)
  Utils_js.FilenameSet.t Lwt.t
