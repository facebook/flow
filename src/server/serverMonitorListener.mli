(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type workload = ServerEnv.env -> ServerEnv.env Lwt.t
val listen_for_messages: ServerEnv.genv -> unit Lwt.t
val wait_for_anything: ServerEnv.genv -> ServerEnv.env -> unit Lwt.t
val get_next_workload: unit -> workload option
val update_env: ServerEnv.env -> ServerEnv.env
val get_updates_for_recheck: ServerEnv.genv -> ServerEnv.env -> Utils_js.FilenameSet.t
val wait_for_updates_for_recheck: ServerEnv.genv -> ServerEnv.env -> unit Lwt.t
