(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t =
  | Ok
  | Build_error
  | Build_terminated
  | Checkpoint_error
  | Input_error
  | Kill_error
  | No_server_running
  | Out_of_time
  | Out_of_retries
  | Server_already_exists
  | Server_initializing
  | Type_error
  | Build_id_mismatch
  | Unused_server
  | Lock_stolen
  | Interrupted
  | Socket_error
  | Missing_hhi
  | Dfind_died
  | Dfind_unresponsive
  | EventLogger_Timeout

exception Exit_with of t

let exit t =
  let ec = match t with
    | Ok -> 0
    | Build_error -> 2
    | Build_terminated -> 1
    | Checkpoint_error -> 8
    | Input_error -> 10
    | Kill_error -> 1
    | No_server_running -> 6
    | Out_of_time -> 7
    | Out_of_retries -> 7
    | Server_already_exists -> 77
    | Server_initializing -> 1
    | Type_error -> 2
    | Build_id_mismatch -> 9
    | Unused_server -> 5
    | Lock_stolen -> 11
    | Interrupted -> -6
    | Missing_hhi -> 97
    | Socket_error -> 98
    | Dfind_died -> 99
    | Dfind_unresponsive -> 100
    | EventLogger_Timeout -> 101
  in
  Pervasives.exit ec

let to_string = function
  | Ok -> "Ok"
  | Build_error -> "Build_error"
  | Build_terminated -> "Build_terminated"
  | Checkpoint_error -> "Checkpoint_error"
  | Input_error -> "Input_error"
  | Kill_error -> "Kill_error"
  | No_server_running -> "No_server_running"
  | Out_of_time -> "Out_of_time"
  | Out_of_retries -> "Out_of_retries"
  | Server_already_exists -> "Server_already_exists"
  | Server_initializing -> "Server_initializing"
  | Type_error -> "Type_error"
  | Build_id_mismatch -> "Build_id_mismatch"
  | Unused_server -> "Unused_server"
  | Lock_stolen -> "Lock_stolen"
  | Interrupted -> "Interrupted"
  | Socket_error -> "Socket_error"
  | Missing_hhi -> "Missing_hhi"
  | Dfind_died -> "Dfind_died"
  | Dfind_unresponsive -> "Dfind_unresponsive"
  | EventLogger_Timeout -> "EventLogger_Timeout"

let unpack = function
  | Unix.WEXITED n -> "exit", n
  | Unix.WSIGNALED n -> "signaled", n
  | Unix.WSTOPPED n -> "stopped", n
