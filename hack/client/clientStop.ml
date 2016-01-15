(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
module MC = MonitorConnection
module SMUtils = ServerMonitorUtils

exception FailedToKill

type env = {
  root: Path.t;
}

let wait_for_death root secs =
  let i = ref 0 in
  try
    while MC.server_exists (ServerFiles.lock_file root) do
      incr i;
      if !i < secs then ignore @@ Unix.sleep 1
      else raise Exit
    done;
    true
  with Exit -> false

let nice_kill (ic, oc) env =
  let root_s = Path.to_string env.root in
  Printf.eprintf "Attempting to nicely kill server for %s\n%!" root_s;
  (** Read Server's hello. See also ClientConnect.wait_for_server_hello *)
  let readable, _, _  = Unix.select
    [Timeout.descr_of_in_channel ic] [] [Timeout.descr_of_in_channel ic] 1.0 in
  (match readable with
  | [_fd] ->
    (** input_line can timeout. *)
    let hello_msg = Timeout.with_timeout
      ~timeout:1
      ~on_timeout: begin fun _ ->
        Printf.eprintf "Server is busy, can't nicely kill";
        raise FailedToKill
      end
      ~do_:begin fun timeout ->
        Timeout.input_line ~timeout ic
      end
    in
    begin
    match hello_msg with
    | "Hello" ->
      let _response = ServerCommand.rpc (ic, oc) ServerRpc.KILL in
      let success = wait_for_death env.root 5 in
      if not success then begin
        Printf.eprintf "Failed to kill server nicely for %s\n%!" root_s;
        raise FailedToKill;
      end else
        Printf.eprintf "Successfully killed server for %s\n%!" root_s
    | _ ->
        Printf.eprintf "Server response invalid, can't nicely kill";
        raise FailedToKill
    end
  | _ ->
      Printf.eprintf "Server is busy, can't nicely kill";
      raise FailedToKill
  )

let mean_kill env =
  let root_s = Path.to_string env.root in
  Printf.eprintf "Attempting to meanly kill server for %s\n%!" root_s;
  let pids =
    try PidLog.get_pids (ServerFiles.pids_file env.root)
    with PidLog.FailedToGetPids ->
      Printf.eprintf "Unable to figure out pids of running Hack server. \
        Try manually killing it with `pkill hh_server`\n%!";
      raise FailedToKill
  in
  let success =
    try
      List.iter pids ~f:begin fun (pid, reason) ->
        try Sys_utils.terminate_process pid
        with Unix.Unix_error (Unix.ESRCH, "kill", _) ->
          (* no such process *)
          ()
      end;
      wait_for_death env.root 3
    with e ->
      print_endline (Printexc.to_string e);
      false
  in
  if not success then begin
    Printf.eprintf "Failed to kill server meanly for %s. \
      Try manually killing it with `pkill hh_server`\n%!" root_s;
    raise FailedToKill
  end else Printf.eprintf "Successfully killed server for %s\n%!" root_s

let do_kill env =
  let root_s = Path.to_string env.root in
  match ServerUtils.connect_to_monitor
    env.root HhServerMonitorConfig.Program.name with
  | Result.Ok conn ->
      begin
        try nice_kill conn env with FailedToKill ->
          try mean_kill env with FailedToKill ->
            raise Exit_status.(Exit_with Kill_error)
      end
  | Result.Error (SMUtils.Server_missing | SMUtils.Server_died) ->
      Printf.eprintf "Error: no server to kill for %s\n%!" root_s
  | Result.Error SMUtils.Build_id_mismatched ->
      Printf.eprintf "Successfully killed server for %s\n%!" root_s
  | Result.Error (SMUtils.Server_busy) ->
      try mean_kill env
      with FailedToKill ->
        raise Exit_status.(Exit_with Kill_error)

let main env =
  HackEventLogger.client_stop ();
  do_kill env;
  Exit_status.Ok

let kill_server root = do_kill {root}
