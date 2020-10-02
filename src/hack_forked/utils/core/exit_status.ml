(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Socket_error
  | EventLogger_restart_out_of_retries
  | Watchman_failed
  | Watchman_fresh_instance

exception Exit_with of t

let exit_code = function
  | Socket_error -> 98
  | Watchman_failed -> 103
  | EventLogger_restart_out_of_retries -> 108
  | Watchman_fresh_instance -> 109

let exit t =
  let ec = exit_code t in
  Stdlib.exit ec

let to_string = function
  | Socket_error -> "Socket_error"
  | EventLogger_restart_out_of_retries -> "EventLogger_restart_out_of_retries"
  | Watchman_failed -> "Watchman_failed"
  | Watchman_fresh_instance -> "Watchman_fresh_instance"

let unpack = function
  | Unix.WEXITED n -> ("exit", n)
  | Unix.WSIGNALED n ->
    (*
     * Ocaml signal numbers are mapped from System signal numbers.
     * They are negative.
     * See caml_convert_signal_number byterun/signals.c in Ocaml system source code
     * to convert from Ocaml number to System number
     *)
    ("signaled", n)
  | Unix.WSTOPPED n -> ("stopped", n)
