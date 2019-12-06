(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type env

type event = string * string

external init : unit -> env = "stub_fsevents_init"

external add_watch : env -> string -> string = "stub_fsevents_add_watch"

external get_event_fd : env -> Unix.file_descr = "stub_fsevents_get_event_fd"

external read_events : env -> event list = "stub_fsevents_read_events"

(* glevi is lazy and didn't implement removing watches since hh_server never
 * actually does that at the moment 
external rm_watch : env -> string -> string = "stub_fsevents_rm_watch"
 *)
