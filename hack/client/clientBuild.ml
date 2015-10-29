(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

(* 800s was chosen because it was above most of the historical p95 of
 * hack server startup times as observed here:
 * https://fburl.com/48825801, see also https://fburl.com/29184831 *)
let num_build_retries = 800

type env = {
  root : Path.t;
  wait : bool;
  build_opts : ServerBuild.build_opts;
}

let handle_response env ic =
  let finished = ref false in
  let exit_code = ref Exit_status.Ok in
  HackEventLogger.client_build_begin_work
    (ServerBuild.build_type_of env.build_opts)
    env.build_opts.ServerBuild.id;
  try
    while true do
      let line:ServerBuild.build_progress = Timeout.input_value ic in
      match line with
      | ServerBuild.BUILD_PROGRESS s -> print_endline s
      | ServerBuild.BUILD_ERROR s ->
          exit_code := Exit_status.Build_error; print_endline s
      | ServerBuild.BUILD_FINISHED -> finished := true
    done;
    Exit_status.Ok
  with
  | End_of_file ->
    if not !finished then begin
      Printf.fprintf stderr ("Build unexpectedly terminated! "^^
        "You may need to do `hh_client restart`.\n");
      Exit_status.Build_terminated
    end else !exit_code
  | Failure _ as e ->
    (* We are seeing Failure "input value: bad object" which can
     * realistically only happen from Marshal.from_channel ic.
     * This admittedly won't help us root cause this, but at least
     * this will help us identify where it is occurring
     *)
    let backtrace = Printexc.get_backtrace () in
    let e_str = Printexc.to_string e in
    Printf.fprintf stderr "Unexpected error: %s\n%s%!" e_str backtrace;
    raise e

let main env =
  let build_type = ServerBuild.build_type_of env.build_opts in
  let request_id = env.build_opts.ServerBuild.id in
  HackEventLogger.client_build build_type request_id;
  let ic, oc = ClientConnect.connect { ClientConnect.
    root = env.root;
    autostart = true;
    retries = if env.wait then None else Some num_build_retries;
    retry_if_init = true;
    expiry = None;
    no_load = false;
  } in
  let old_svnrev = Option.try_with begin fun () ->
    Sys_utils.read_file ServerBuild.svnrev_path
  end in
  let exit_status = with_context
    ~enter:(fun () -> ())
    ~exit:(fun () ->
      Printf.eprintf "\nHack build id: %s\n%!" request_id)
    ~do_:(fun () ->
      ServerCommand.(stream_request oc (BUILD env.build_opts));
      handle_response env ic) in
  let svnrev = Option.try_with begin fun () ->
    Sys_utils.read_file ServerBuild.svnrev_path
  end in
  HackEventLogger.client_build_finish
    ~rev_changed:(svnrev <> old_svnrev) ~build_type ~request_id ~exit_status;
  exit_status
