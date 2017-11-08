(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val reset: unit -> unit Lwt.t
val update: status:ServerStatus.status -> unit
val call_on_free: f:(unit -> unit Lwt.t) -> unit Lwt.t
val get_busy_status: unit -> ServerStatus.status option
