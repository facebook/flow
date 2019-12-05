(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_core

(* Forking duplicates data in all buffers, so we flush them beforehand to avoid
 * writing the same thing twice.
 *
 * Note: by default, this only clears ocaml's internal buffers (via flush_all).
 * If your process has its own buffers in the program state, those must be
 * cleared by registering a callback with `on_fork` below to reliably avoid
 * writing those buffers twice as well. *)
let pre_fork_callbacks : (unit -> unit) list ref = ref [flush_all]

(** Sometimes it is more convenient to clear buffers in the children (to
 * avoid the double writing of data) instead of the parent on a successful
 * fork. We store those callbacks here. *)
let post_fork_child_callbacks : (unit -> unit) list ref = ref []

let on_fork f = pre_fork_callbacks := f :: !pre_fork_callbacks

let post_fork_child f = post_fork_child_callbacks := f :: !post_fork_child_callbacks

(* You should always use this instead of Unix.fork, so that the callbacks get
 * invoked *)
let fork () =
  List.iter !pre_fork_callbacks (fun f -> f ());
  match Unix.fork () with
  | 0 ->
    List.iter !post_fork_child_callbacks (fun f -> f ());
    0
  | i -> i

(* should only be called from hh_server, which initializes the PidLog *)
let fork_and_log ?reason () =
  let result = fork () in
  (match result with
  | -1 -> ()
  | 0 -> PidLog.close ()
  | pid -> PidLog.log ?reason pid);
  result

let fork_and_may_log ?reason () =
  match reason with
  | None -> fork ()
  | Some _ -> fork_and_log ?reason ()
