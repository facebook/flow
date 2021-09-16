(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type env_update = ServerEnv.env -> ServerEnv.env

(* APIs to add to the state *)
val push_new_workload : name:string -> WorkloadStream.workload -> unit

val push_new_parallelizable_workload : name:string -> WorkloadStream.parallelizable_workload -> unit

val defer_parallelizable_workload : name:string -> WorkloadStream.parallelizable_workload -> unit

val requeue_deferred_parallelizable_workloads : unit -> unit

val push_new_env_update : env_update -> unit

val push_files_to_recheck :
  ?metadata:MonitorProt.file_watcher_metadata ->
  ?callback:(Profiling_js.finished option -> unit) ->
  reason:LspProt.recheck_reason ->
  SSet.t ->
  unit

val push_files_to_force_focused_and_recheck :
  ?callback:(Profiling_js.finished option -> unit) ->
  reason:LspProt.recheck_reason ->
  SSet.t ->
  unit

val push_checked_set_to_force :
  ?callback:(Profiling_js.finished option -> unit) ->
  reason:LspProt.recheck_reason ->
  CheckedSet.t ->
  unit

val push_files_to_resync_after_file_watcher_restart :
  ?metadata:MonitorProt.file_watcher_metadata ->
  ?callback:(Profiling_js.finished option -> unit) ->
  reason:LspProt.recheck_reason ->
  SSet.t ->
  unit

val cancellation_requests : Lsp.IdSet.t ref

(* APIs to wait *)
val wait_for_anything :
  process_updates:(?skip_incompatible:bool -> SSet.t -> Utils_js.FilenameSet.t) ->
  get_forced:(unit -> CheckedSet.t) ->
  unit Lwt.t

val wait_for_updates_for_recheck :
  process_updates:(?skip_incompatible:bool -> SSet.t -> Utils_js.FilenameSet.t) ->
  get_forced:(unit -> CheckedSet.t) ->
  unit Lwt.t

(* APIs to consume *)
type recheck_workload = {
  files_to_recheck: Utils_js.FilenameSet.t;
  files_to_force: CheckedSet.t;
  profiling_callbacks: (Profiling_js.finished option -> unit) list;
  metadata: MonitorProt.file_watcher_metadata;
  recheck_reasons_rev: LspProt.recheck_reason list;
}

val pop_next_workload : unit -> WorkloadStream.workload option

val wait_and_pop_parallelizable_workload : unit -> WorkloadStream.parallelizable_workload Lwt.t

val update_env : ServerEnv.env -> ServerEnv.env

val requeue_workload : recheck_workload -> unit

val get_and_clear_recheck_workload :
  prioritize_dependency_checks:bool ->
  process_updates:(?skip_incompatible:bool -> SSet.t -> Utils_js.FilenameSet.t) ->
  get_forced:(unit -> CheckedSet.t) ->
  recheck_workload
