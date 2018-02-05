(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

type process_data =
  {
    (** Process ID. *)
    pid : int;
    name : string;
    start_t : float;
    (** Get occasional updates about status/busyness from typechecker here. *)
    in_fd: Unix.file_descr;
    (** Send client's File Descriptors to the typechecker over this. *)
    out_fd : Unix.file_descr;
    last_request_handoff : float ref;
  }

type server_process =
  | Not_yet_started
  | Alive of process_data
  | Informant_killed
  (** When the server crashes, we want to track that it has crashed and report
   * that crash info to the next hh_client that connects. We keep that info
   * here. *)
  | Died_unexpectedly of Unix.process_status * bool


let process_status_to_string (status: Unix.process_status) : string =
  match status with
  | Unix.WEXITED i -> Printf.sprintf "WEXITED(%d)" i
  | Unix.WSIGNALED i -> Printf.sprintf "WSIGNALED(%d)" i
  | Unix.WSTOPPED i -> Printf.sprintf "WSTOPPED(%d)" i

let process_data_to_string (process_data: process_data) : string =
  Printf.sprintf "%s, PID=%d" process_data.name process_data.pid

let server_process_to_string (server_process: server_process) : string =
  match server_process with
  | Not_yet_started -> "Not_yet_started"
  | Alive process_data -> Printf.sprintf "Alive(%s)" (process_data_to_string process_data)
  | Informant_killed -> "Informant_killed"
  | Died_unexpectedly (status, was_oom) -> Printf.sprintf "Died unexpectedly(%s, was_oom=%B)"
      (process_status_to_string status) was_oom
