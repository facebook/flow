(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(***********************************************************************)
(* flow stop command *)
(***********************************************************************)

exception FailedToKill

let spec = {
  CommandSpec.
  name = "stop";
  doc = "Stops a Flow server";
  usage = Printf.sprintf
    "Usage: %s stop [OPTION]... [ROOT]\n\
      Stops a flow server\n\n\
      Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
      ROOT is assumed to be current directory if unspecified\n"
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> CommandUtils.temp_dir_flag
    |> CommandUtils.from_flag
    |> anon "root" (optional string) ~doc:"Root directory"
  )
}

let is_expected = function
  | ServerProt.SERVER_DYING
  | ServerProt.SERVER_OUT_OF_DATE ->
      true
  | _ ->
      false

let kill (ic, oc) =
  ServerProt.cmd_to_channel oc ServerProt.KILL;
  ServerProt.response_from_channel ic

let nice_kill (ic, oc) ~tmp_dir root =
  Printf.eprintf "Attempting to nicely kill server for %s\n%!"
    (Path.to_string root);
  let response = kill (ic, oc) in
  if is_expected response then begin
    let i = ref 0 in
    while CommandUtils.server_exists ~tmp_dir root do
      incr i;
      if !i < 5 then ignore @@ Unix.sleep 1
      else raise FailedToKill
    done;
    Printf.eprintf "Successfully killed server for %s\n%!"
      (Path.to_string root)
  end else begin
    Printf.fprintf stderr "Unexpected response from the server: %s\n"
      (ServerProt.response_to_string response);
    raise FailedToKill
  end

let mean_kill ~tmp_dir root =
  Printf.fprintf stderr "Attempting to meanly kill server for %s\n%!"
    (Path.to_string root);
  let pids =
    try PidLog.get_pids (FlowConfig.pids_file ~tmp_dir root)
    with PidLog.FailedToGetPids -> Printf.fprintf stderr
        "Unable to figure out pids of running Flow server. \
        Try manually killing it with 'pkill %s' (be careful on shared \
        devservers)\n%!"
        CommandUtils.exe_name;
      raise FailedToKill
  in
  List.iter (fun (pid, reason) ->
    try Unix.kill pid 9
    with Unix.Unix_error (Unix.ESRCH, "kill", _) ->
      (* no such process *)
      ()
  ) pids;
  ignore(Unix.sleep 1);
  if CommandUtils.server_exists ~tmp_dir root
  then raise FailedToKill
  else Printf.fprintf stderr "Successfully killed server for %s\n%!"
    (Path.to_string root)

let main temp_dir from root () =
  let root = CommandUtils.guess_root root in
  let root_s = Path.to_string root in
  let tmp_dir = match temp_dir with
  | Some x -> x
  | None -> FlowConfig.default_temp_dir (* TODO: add flowconfig option *)
  in
  FlowEventLogger.set_from from;
  try
    let conn = CommandUtils.connect ~tmp_dir root in
    begin
      try nice_kill conn ~tmp_dir root
      with FailedToKill ->
        Printf.eprintf "Failed to kill server nicely for %s\n%!" root_s;
        exit 1
    end
  with
  | CommandExceptions.Server_missing ->
      Printf.eprintf "Error: no server to kill for %s\n%!" root_s
  | CommandExceptions.Server_out_of_date ->
      Printf.eprintf "Successfully killed server for %s\n%!" root_s
  | CommandExceptions.Server_busy
  | CommandExceptions.Server_initializing ->
      try mean_kill ~tmp_dir root
      with FailedToKill ->
        Printf.eprintf "Failed to kill server meanly for %s\n%!" root_s;
        exit 1

let command = CommandSpec.command spec main
