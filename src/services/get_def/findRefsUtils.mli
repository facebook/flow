(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val compute_docblock: File_key.t -> string (* content *) -> Docblock.t

val compute_ast_result:
  module_ref_prefix: string option ->
  File_key.t ->
  string (* content *) ->
  ((Loc.t, Loc.t) Flow_ast.program * File_sig.t * Docblock.t, string) result

val get_ast_result:
  File_key.t ->
  ((Loc.t, Loc.t) Flow_ast.program * File_sig.t * Docblock.t, string) result

val get_dependents:
  Options.t ->
  MultiWorkerLwt.worker list option ->
  ServerEnv.env ref ->
  File_key.t ->
  string (* content *) ->
  (* transitive dependents, direct dependents *)
  (Utils_js.FilenameSet.t * Utils_js.FilenameSet.t) Lwt.t

val lazy_mode_focus:
  ServerEnv.genv ->
  ServerEnv.env ->
  string (* path *) ->
  ServerEnv.env Lwt.t
