(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type env

type event = string * string

val init : unit -> env

val add_watch : env -> string -> string

val get_event_fd : env -> Unix.file_descr

val read_events : env -> event list

(* Currently unimplemented
val rm_watch : env -> string -> string
*)
