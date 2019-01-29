(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val compute_docblock: File_key.t -> string (* content *) -> Docblock.t

val compute_ast_result:
  Options.t ->
  File_key.t ->
  string (* content *) ->
  ((Loc.t, Loc.t) Flow_ast.program * File_sig.With_Loc.t * Docblock.t, string) result

val get_ast_result:
  reader:State_reader.t ->
  File_key.t ->
  ((Loc.t, Loc.t) Flow_ast.program * File_sig.With_Loc.t * Docblock.t, string) result

val get_dependents:
  reader:State_reader.t ->
  Options.t ->
  MultiWorkerLwt.worker list option ->
  ServerEnv.env ref ->
  File_key.t ->
  string (* content *) ->
  (* transitive dependents, direct dependents *)
  (Utils_js.FilenameSet.t * Utils_js.FilenameSet.t) Lwt.t
