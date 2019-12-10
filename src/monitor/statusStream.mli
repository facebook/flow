(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val reset : Options.file_watcher -> ServerStatus.restart_reason option -> unit Lwt.t

val update : status:ServerStatus.status -> unit

val file_watcher_ready : unit -> unit

val call_on_free : f:(unit -> unit Lwt.t) -> unit Lwt.t

val get_status : unit -> ServerStatus.status * FileWatcherStatus.status

val wait_for_signficant_status :
  timeout:float -> (ServerStatus.status * FileWatcherStatus.status) Lwt.t

val ever_been_free : unit -> bool
