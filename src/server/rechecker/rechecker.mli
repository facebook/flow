(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* filter and relativize updated file paths *)
val process_updates :
  ?skip_incompatible:bool -> options:Options.t -> ServerEnv.env -> SSet.t -> Utils_js.FilenameSet.t

val recheck_loop :
  ServerEnv.genv -> ServerEnv.env -> (Profiling_js.finished list * ServerEnv.env) Lwt.t

val get_lazy_stats : options:Options.t -> ServerEnv.env -> ServerProt.Response.lazy_stats
