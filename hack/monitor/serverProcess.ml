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
