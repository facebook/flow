(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type workload = ServerEnv.env -> ServerEnv.env Lwt.t
type env_update = ServerEnv.env -> ServerEnv.env

(* APIs to add to the state *)
val push_new_workload: workload -> unit
val push_new_env_update: env_update -> unit
val push_files_to_recheck: ?callback:(Profiling_js.finished option -> unit) -> SSet.t -> unit
val push_files_to_focus: ?callback:(Profiling_js.finished option -> unit) -> SSet.t -> unit
val cancellation_requests: Lsp.IdSet.t ref

(* APIs to wait *)
val wait_for_anything:
  process_updates:(SSet.t -> Utils_js.FilenameSet.t) ->
  unit Lwt.t
val wait_for_updates_for_recheck:
  process_updates:(SSet.t -> Utils_js.FilenameSet.t) ->
  unit Lwt.t

(* APIs to consume *)
type recheck_workload = {
  files_to_recheck: Utils_js.FilenameSet.t;
  files_to_focus: Utils_js.FilenameSet.t;
  profiling_callbacks: (Profiling_js.finished option -> unit) list;
}
val pop_next_workload: unit -> workload option
val update_env: ServerEnv.env -> ServerEnv.env
val get_and_clear_recheck_workload:
  process_updates:(SSet.t -> Utils_js.FilenameSet.t) ->
  recheck_workload
