(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This `.mli` file was generated automatically. It may include extra
   definitions that should not actually be exposed to the caller. If you notice
   that this interface file is a poor interface, please take a few minutes to
   clean it up manually, and then delete this comment once the interface is in
   shape. *)

type t =
  | Socket_error
  | EventLogger_restart_out_of_retries
  | Watchman_failed
  | Watchman_fresh_instance

exception Exit_with of t

val exit_code : t -> int

val exit : t -> 'a

val to_string : t -> string

val unpack : Unix.process_status -> string * int
