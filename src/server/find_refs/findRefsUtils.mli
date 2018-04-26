(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val (%>>=):
  ('ok, 'err) result ->
  ('ok -> ('a, 'err) result Lwt.t) ->
  ('a, 'err) result Lwt.t

val (%>>|):
  ('ok, 'err) result ->
  ('ok -> 'a Lwt.t) ->
  ('a, 'err) result Lwt.t

val compute_docblock: File_key.t -> string (* content *) -> Docblock.t

val compute_ast_result:
  File_key.t ->
  string (* content *) ->
  (Loc.t Ast.program * File_sig.t * Docblock.t, string) result

val get_ast_result:
  File_key.t ->
  (Loc.t Ast.program * File_sig.t * Docblock.t, string) result

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
