(**
 * Copyright (c) 2013-present, Facebook, Inc.
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

open Utils_js

exception FailedToKill of string option

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
    |> CommandUtils.quiet_flag
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
  let response = kill (ic, oc) in
  if is_expected response then begin
    let i = ref 0 in
    while CommandConnectSimple.server_exists ~tmp_dir root do
      incr i;
      if !i < 5 then ignore @@ Unix.sleep 1
      else raise (FailedToKill None)
    done;
  end else begin
    let msg = Printf.sprintf "Unexpected response from the server: %s"
      (ServerProt.response_to_string response) in
    raise (FailedToKill (Some msg))
  end

let mean_kill ~tmp_dir root =
  let pids =
    try PidLog.get_pids (Server_files_js.pids_file ~tmp_dir root)
    with PidLog.FailedToGetPids ->
      let msg = Printf.sprintf
        "Unable to figure out pids of running Flow server. \
        Try manually killing it with 'pkill %s' (be careful on shared \
        devservers)"
        CommandUtils.exe_name
      in
      raise (FailedToKill (Some msg))
  in
  List.iter (fun (pid, _) ->
    try
      pid
      |> Sys_utils.handle_of_pid_for_termination
      |> Sys_utils.terminate_process
    with Unix.Unix_error (Unix.ESRCH, "kill", _) ->
      (* no such process *)
      ()
  ) pids;
  ignore(Unix.sleep 1);
  if CommandConnectSimple.server_exists ~tmp_dir root
  then raise (FailedToKill None);
  ()

let main temp_dir from quiet root () =
  let root = CommandUtils.guess_root root in
  let config = FlowConfig.get (Server_files_js.config_file root) in
  let root_s = Path.to_string root in
  let tmp_dir = match temp_dir with
  | Some x -> x
  | None -> FlowConfig.(config.options.Opts.temp_dir)
  in
  let tmp_dir = Path.to_string (Path.make tmp_dir) in
  FlowEventLogger.set_from from;
  if not quiet then prerr_endlinef
    "Trying to connect to server for %s"
    (Path.to_string root);
  CommandConnectSimple.(
    match connect_once ~tmp_dir root with
    | Result.Ok conn ->
        begin try
          if not quiet then prerr_endlinef
            "Attempting to nicely kill server for %s"
            (Path.to_string root);
          nice_kill conn ~tmp_dir root;
          if not quiet then prerr_endlinef
            "Successfully killed server for %s"
            (Path.to_string root)
        with FailedToKill err ->
          if not quiet then match err with
          | Some err -> prerr_endline err
          | None -> ();
          let msg = spf "Failed to kill server nicely for %s" root_s in
          FlowExitStatus.(exit ~msg Kill_error)
        end
    | Result.Error Server_missing ->
        if not quiet then prerr_endlinef
          "Warning: no server to kill for %s" root_s
    | Result.Error Build_id_mismatch ->
        if not quiet then prerr_endlinef
          "Successfully killed server for %s" root_s
    | Result.Error Server_initializing
    | Result.Error Server_rechecking
    | Result.Error Server_gcollecting
    | Result.Error Server_busy ->
        begin try
          if not quiet then prerr_endlinef
            "Attempting to meanly kill server for %s"
            (Path.to_string root);
          mean_kill ~tmp_dir root;
          if not quiet then prerr_endlinef
            "Successfully killed server for %s"
            (Path.to_string root)
        with FailedToKill err ->
          if not quiet then match err with
          | Some err -> prerr_endline err
          | None -> ();
          let msg = spf "Failed to kill server meanly for %s" root_s in
          FlowExitStatus.(exit ~msg Kill_error)
        end
  )

let command = CommandSpec.command spec main
