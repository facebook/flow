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

val recheck:
  ServerEnv.genv ->
  ServerEnv.env ->
  ?force_focus:bool ->
  Utils_js.FilenameSet.t ->
  (Profiling_js.finished option * ServerEnv.env) Lwt.t

val get_lazy_stats: ServerEnv.genv -> ServerEnv.env -> ServerProt.Response.lazy_stats
