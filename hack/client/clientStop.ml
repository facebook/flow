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

let nice_kill env =
  let root_s = Path.to_string env.root in
  Printf.eprintf "Attempting to nicely kill server for %s\n%!" root_s;
  try begin
    match ServerUtils.shut_down_server env.root with
    | Result.Ok shutdown_result ->
      begin match shutdown_result with
      | SMUtils.SHUTDOWN_VERIFIED ->
        Printf.eprintf "Successfully killed server for %s\n%!" root_s
      | SMUtils.SHUTDOWN_UNVERIFIED ->
        Printf.eprintf
          "Failed to kill server nicely for %s (Shutdown not verified)\n%!"
          root_s;
        raise FailedToKill
      end
    | Result.Error SMUtils.Build_id_mismatched ->
      Printf.eprintf "Successfully killed server for %s\n%!" root_s
    | Result.Error SMUtils.Server_missing ->
      Printf.eprintf "No server to kill for %s\n%!" root_s
    | Result.Error _ ->
      Printf.eprintf "Failed to kill server nicely for %s\n%!" root_s;
      raise FailedToKill
  end
  with
  | _ ->
    Printf.eprintf "Failed to kill server nicely for %s\n%!" root_s;
    raise FailedToKill

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
  try nice_kill env with FailedToKill ->
    try mean_kill env with FailedToKill ->
      raise Exit_status.(Exit_with Kill_error)

let main env =
  HackEventLogger.client_stop ();
  do_kill env;
  Exit_status.Ok

let kill_server root = do_kill {root}
