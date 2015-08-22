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

let nice_kill (ic, oc) env =
  let root = env.root in
  Printf.eprintf "Attempting to nicely kill server for %s\n%!"
    (Path.to_string root);
  let _response = ServerCommand.rpc (ic, oc) ServerRpc.KILL in
  let i = ref 0 in
  while CCS.server_exists root do
    incr i;
    if !i < 5 then ignore @@ Unix.sleep 1
    else raise FailedToKill
  done;
  Printf.eprintf "Successfully killed server for %s\n%!"
    (Path.to_string root)

let mean_kill env =
  Printf.fprintf stderr "Attempting to meanly kill server for %s\n%!"
    (Path.to_string env.root);
  let pids =
    try PidLog.get_pids (ServerFiles.pids_file env.root)
    with PidLog.FailedToGetPids ->
      Printf.eprintf "Unable to figure out pids of running Hack server. \
        Try manually killing it with 'pkill hh-server'\n%!";
      raise FailedToKill
  in
  List.iter pids ~f:begin fun (pid, reason) ->
    try Unix.kill pid 9
    with Unix.Unix_error (Unix.ESRCH, "kill", _) ->
      (* no such process *)
      ()
  end;
  ignore(Unix.sleep 1);
  if CCS.server_exists env.root
  then raise FailedToKill
  else Printf.fprintf stderr "Successfully killed server for %s\n%!"
    (Path.to_string env.root)

let do_kill env =
  let root_s = Path.to_string env.root in
  match CCS.connect_once env.root with
  | Result.Ok conn ->
      begin
        try nice_kill conn env
        with FailedToKill ->
          Printf.eprintf "Failed to kill server nicely for %s\n%!" root_s;
          exit 1
      end
  | Result.Error CCS.Server_missing ->
      Printf.eprintf "Error: no server to kill for %s\n%!" root_s
  | Result.Error CCS.Build_id_mismatch ->
      Printf.eprintf "Successfully killed server for %s\n%!" root_s
  | Result.Error (CCS.Server_busy | CCS.Server_initializing) ->
      try mean_kill env
      with FailedToKill ->
        Printf.eprintf "Failed to kill server meanly for %s\n%!" root_s;
        exit 1

let main env =
  do_kill env;
  Exit_status.Ok

let kill_server root = do_kill {root}
