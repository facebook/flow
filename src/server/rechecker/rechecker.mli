(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* filter and relativize updated file paths *)
val process_updates :
  ServerEnv.genv ->
  ServerEnv.env ->
  SSet.t ->
  Utils_js.FilenameSet.t

val recheck_single:
  ?files_to_recheck:Utils_js.FilenameSet.t ->
  ?files_to_focus:Utils_js.FilenameSet.t ->
  ServerEnv.genv ->
  ServerEnv.env ->
  (Profiling_js.finished * ServerEnv.env, ServerEnv.env) result Lwt.t

val recheck_loop:
  ?files_to_recheck:Utils_js.FilenameSet.t ->
  ?files_to_focus:Utils_js.FilenameSet.t ->
  ServerEnv.genv ->
  ServerEnv.env ->
  ServerEnv.env Lwt.t

val get_lazy_stats: ServerEnv.genv -> ServerEnv.env -> ServerProt.Response.lazy_stats
