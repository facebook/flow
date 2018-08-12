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
    |> base_flags
    |> temp_dir_flag
    |> from_flag
    |> quiet_flag
    |> anon "root" (optional string)
  )
}

exception FailedToKillNicely

let main base_flags temp_dir from quiet root () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root = guess_root flowconfig_name root in
  let config = FlowConfig.get (Server_files_js.config_file flowconfig_name root) in
  let root_s = Path.to_string root in
  let tmp_dir = match temp_dir with
  | Some x -> x
  | None -> FlowConfig.temp_dir config
  in
  let tmp_dir = Path.to_string (Path.make tmp_dir) in
  FlowEventLogger.set_from from;
  if not quiet then prerr_endlinef "Trying to connect to server for `%s`" (Path.to_string root);
  let client_handshake = SocketHandshake.({
    client_build_id = build_revision;
    is_stop_request = true;
    server_should_hangup_if_still_initializing = false;
    server_should_exit_if_version_mismatch = true; }, {
    client_type = Ephemeral;
  }) in
  CommandConnectSimple.(
    match connect_once ~flowconfig_name ~client_handshake ~tmp_dir root with
    | Ok _ ->
        begin try
          if not quiet then prerr_endlinef
            "Told server for `%s` to die. Waiting for confirmation..."
            (Path.to_string root);
          let i = ref 0 in
          while CommandConnectSimple.server_exists ~flowconfig_name ~tmp_dir root do
            incr i;
            if !i < 5 then ignore @@ Unix.sleep 1
            else raise FailedToKillNicely
          done;
          if not quiet then prerr_endlinef
            "Successfully killed server for `%s`"
            (Path.to_string root)
        with FailedToKillNicely ->
          let msg = spf "Failed to kill server nicely for `%s`" root_s in
          FlowExitStatus.(exit ~msg Kill_error)
        end
    | Error Server_missing ->
        if not quiet then prerr_endlinef
          "Warning: no server to kill for `%s`" root_s
    | Error Build_id_mismatch ->
        if not quiet then prerr_endlinef
          "Successfully killed server for `%s`" root_s
    | Error Server_busy _
    | Error Server_socket_missing ->
        begin try
          if not quiet then prerr_endlinef
            "Attempting to meanly kill server for `%s`"
            (Path.to_string root);
          CommandMeanKill.mean_kill ~flowconfig_name ~tmp_dir root;
          if not quiet then prerr_endlinef
            "Successfully killed server for `%s`"
            (Path.to_string root)
        with CommandMeanKill.FailedToKill err ->
          if not quiet then match err with
          | Some err -> prerr_endline err
          | None -> ();
          let msg = spf "Failed to kill server meanly for `%s`" root_s in
          FlowExitStatus.(exit ~msg Kill_error)
        end
  )

let command = CommandSpec.command spec main
