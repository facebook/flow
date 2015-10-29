(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
module CCS = ClientConnectSimple

exception FailedToKill

type env = {
  root: Path.t;
}

let wait_for_death root secs =
  let i = ref 0 in
  try
    while CCS.server_exists root do
      incr i;
      if !i < secs then ignore @@ Unix.sleep 1
      else raise Exit
    done;
    true
  with Exit -> false

let nice_kill (ic, oc) env =
  let root_s = Path.to_string env.root in
  Printf.eprintf "Attempting to nicely kill server for %s\n%!" root_s;
  let _response = ServerCommand.rpc (ic, oc) ServerRpc.KILL in
  let success = wait_for_death env.root 3 in
  if not success then begin
    Printf.eprintf "Failed to kill server nicely for %s\n%!" root_s;
    raise FailedToKill;
  end else
    Printf.eprintf "Successfully killed server for %s\n%!" root_s

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
  match CCS.connect_once env.root with
  | Result.Ok conn ->
      begin
        try nice_kill conn env with FailedToKill ->
          try mean_kill env with FailedToKill ->
            raise Exit_status.(Exit_with Kill_error)
      end
  | Result.Error CCS.Server_missing ->
      Printf.eprintf "Error: no server to kill for %s\n%!" root_s
  | Result.Error CCS.Build_id_mismatch ->
      Printf.eprintf "Successfully killed server for %s\n%!" root_s
  | Result.Error (CCS.Server_busy | CCS.Server_initializing) ->
      try mean_kill env
      with FailedToKill ->
        raise Exit_status.(Exit_with Kill_error)

let main env =
  HackEventLogger.client_stop ();
  do_kill env;
  Exit_status.Ok

let kill_server root = do_kill {root}
