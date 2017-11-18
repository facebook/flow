(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow stop command *)
(***********************************************************************)

open CommandUtils
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
      exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> temp_dir_flag
    |> from_flag
    |> quiet_flag
    |> anon "root" (optional string) ~doc:"Root directory"
  )
}

let mean_kill ~tmp_dir root =
  let pids =
    try PidLog.get_pids (Server_files_js.pids_file ~tmp_dir root)
    with PidLog.FailedToGetPids ->
      let msg = Printf.sprintf
        "Unable to figure out pids of running Flow server. \
        Try manually killing it with 'pkill %s' (be careful on shared \
        devservers)"
        exe_name
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
  let root = guess_root root in
  let config = FlowConfig.get (Server_files_js.config_file root) in
  let root_s = Path.to_string root in
  let tmp_dir = match temp_dir with
  | Some x -> x
  | None -> FlowConfig.temp_dir config
  in
  let tmp_dir = Path.to_string (Path.make tmp_dir) in
  FlowEventLogger.set_from from;
  if not quiet then prerr_endlinef
    "Trying to connect to server for `%s`"
    (Path.to_string root);
  CommandConnectSimple.(
    match connect_once ~client_type:SocketHandshake.StabbityStabStab ~tmp_dir root with
    | Ok _ ->
        begin try
          if not quiet then prerr_endlinef
            "Told server for `%s` to die. Waiting for confirmation..."
            (Path.to_string root);
          let i = ref 0 in
          while CommandConnectSimple.server_exists ~tmp_dir root do
            incr i;
            if !i < 5 then ignore @@ Unix.sleep 1
            else raise (FailedToKill None)
          done;
          if not quiet then prerr_endlinef
            "Successfully killed server for `%s`"
            (Path.to_string root)
        with FailedToKill err ->
          if not quiet then match err with
          | Some err -> prerr_endline err
          | None -> ();
          let msg = spf "Failed to kill server nicely for `%s`" root_s in
          FlowExitStatus.(exit ~msg Kill_error)
        end
    | Error Server_missing ->
        if not quiet then prerr_endlinef
          "Warning: no server to kill for `%s`" root_s
    | Error Build_id_mismatch ->
        if not quiet then prerr_endlinef
          "Successfully killed server for `%s`" root_s
    | Error Server_busy ->
        begin try
          if not quiet then prerr_endlinef
            "Attempting to meanly kill server for `%s`"
            (Path.to_string root);
          mean_kill ~tmp_dir root;
          if not quiet then prerr_endlinef
            "Successfully killed server for `%s`"
            (Path.to_string root)
        with FailedToKill err ->
          if not quiet then match err with
          | Some err -> prerr_endline err
          | None -> ();
          let msg = spf "Failed to kill server meanly for `%s`" root_s in
          FlowExitStatus.(exit ~msg Kill_error)
        end
  )

let command = CommandSpec.command spec main
