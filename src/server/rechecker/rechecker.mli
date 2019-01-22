(**
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  ?files_to_force:CheckedSet.t ->
  ?file_watcher_metadata:MonitorProt.file_watcher_metadata ->
  ServerEnv.genv ->
  ServerEnv.env ->
  (Profiling_js.finished * ServerEnv.env, ServerEnv.env) result Lwt.t

val recheck_loop:
  ServerEnv.genv ->
  ServerEnv.env ->
  (Profiling_js.finished list * ServerEnv.env) Lwt.t

val run_but_cancel_on_file_changes:
  ?get_forced:(unit -> CheckedSet.t) ->
  ServerEnv.genv ->
  ServerEnv.env ->
  f:(unit -> 'a Lwt.t) ->
  on_cancel:(unit -> 'a Lwt.t) ->
  'a Lwt.t

val get_lazy_stats: ServerEnv.genv -> ServerEnv.env -> ServerProt.Response.lazy_stats
