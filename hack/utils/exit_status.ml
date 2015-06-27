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
  | Server_already_exists
  | Type_error

exception Exit_with of t

let exit t =
  let ec = match t with
    | Ok -> 0
    | Build_error -> 2
    | Build_terminated -> 1
    | Checkpoint_error -> 7
    | Input_error -> 1
    | Kill_error -> 1
    | Server_already_exists -> 77
    | Type_error -> 2
  in
  Pervasives.exit ec

let to_string = function
  | Ok -> "Ok"
  | Build_error -> "Build_error"
  | Build_terminated -> "Build_terminated"
  | Checkpoint_error -> "Checkpoint_error"
  | Input_error -> "Input_error"
  | Kill_error -> "Kill_error"
  | Server_already_exists -> "Server_already_exists"
  | Type_error -> "Type_error"
