(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* 800s was chosen because it was above most of the historical p95 of
 * hack server startup times as observed here:
 * https://fburl.com/48825801, see also https://fburl.com/29184831 *)
let num_build_retries = 800

type env = {
  root : Path.t;
  wait : bool;
  build_opts : ServerBuild.build_opts;
}

let build_kind_of build_opts =
  let module LC = ClientLogCommand in
  let {ServerBuild.steps; no_steps; is_push; incremental; _} = build_opts in
  if steps <> None || no_steps <> None then
    LC.Steps
  else if is_push then
    LC.Push
  else if incremental then
    LC.Incremental
  else
    LC.Full

let handle_response env ic =
  let finished = ref false in
  let exit_code = ref Exit_status.Ok in
  HackEventLogger.client_begin_work (ClientLogCommand.LCBuild
    (env.root, build_kind_of env.build_opts));
  try
    while true do
      let line:ServerBuild.build_progress = Marshal.from_channel ic in
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
  let ic, oc = ClientConnect.connect { ClientConnect.
    root = env.root;
    autostart = true;
    retries = if env.wait then None else Some num_build_retries;
    retry_if_init = true;
    expiry = None;
    no_load = false;
  } in
  ServerCommand.(stream_request oc (BUILD env.build_opts));
  handle_response env ic
