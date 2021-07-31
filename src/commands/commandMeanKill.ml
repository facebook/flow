(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception FailedToKill of string option

let mean_kill ~flowconfig_name ~tmp_dir root =
  let pids =
    try PidLog.get_pids (Server_files_js.pids_file ~flowconfig_name ~tmp_dir root) with
    | PidLog.FailedToGetPids ->
      let msg =
        Printf.sprintf
          "Unable to figure out pids of running Flow server. Try manually killing it with 'pkill %s' (be careful on shared devservers)"
          Utils_js.exe_name
      in
      raise (FailedToKill (Some msg))
  in
  List.iter
    (fun (pid, _) ->
      try pid |> Sys_utils.handle_of_pid_for_termination |> Sys_utils.terminate_process with
      | Unix.Unix_error (Unix.ESRCH, "kill", _) ->
        (* no such process *)
        ())
    pids;
  ignore (Unix.sleep 1);
  if CommandConnectSimple.server_exists ~flowconfig_name ~tmp_dir root then
    raise (FailedToKill None);
  ()
