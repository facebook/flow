(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

exception FailedToKill

type env = {
  root: Path.path;
}

module type STOP_CONFIG = sig
  type response
  val server_desc : string
  val server_name : string
  val kill_cmd_to_channel : out_channel -> unit
  val response_from_channel : in_channel -> response
  val response_to_string : response -> string
  val is_expected : response -> bool
end

module type STOP_COMMAND = sig
  val kill_server : env -> unit
  val main : env -> unit
end

module StopCommand (Config : STOP_CONFIG) : STOP_COMMAND = struct

  let nice_kill env =
    let root = env.root in
    Printf.fprintf stderr "Attempting to nicely kill server for %s\n%!"
      (Path.string_of_path root);
    let response = try
      Sys.set_signal
        Sys.sigalrm
        (Sys.Signal_handle (fun _ -> raise ClientExceptions.Server_busy));
      ignore(Unix.alarm 6);

      let ic, oc = ClientUtils.connect root in
      Config.kill_cmd_to_channel oc;
      let response = Config.response_from_channel ic in
      ignore (Unix.alarm 0);
      response
    with e -> begin
      Printf.fprintf stderr "%s\n%!" (Printexc.to_string e);
      raise FailedToKill
    end in
    if Config.is_expected response then begin
      ignore(Unix.sleep 1);
      if ClientUtils.server_exists root
      then raise FailedToKill
      else Printf.fprintf stderr "Successfully killed server for %s\n%!"
        (Path.string_of_path root)
    end else begin
      Printf.fprintf stderr "Unexpected response from the server: %s\n"
        (Config.response_to_string response);
      raise FailedToKill
    end

  let mean_kill env =
    Printf.fprintf stderr "Attempting to meanly kill server for %s\n%!"
      (Path.string_of_path env.root);
    let pids =
      try PidLog.get_pids env.root
      with PidLog.FailedToGetPids ->
        Printf.fprintf stderr "Unable to figure out pids of running %s server. \
          Try manually killing it with 'pkill %s' (be careful on shared \
          devservers)\n%!"
          Config.server_desc
          Config.server_name;
        raise FailedToKill
    in
    List.iter (fun (pid, reason) -> Unix.kill pid 9) pids;
    ignore(Unix.sleep 1);
    if ClientUtils.server_exists env.root
    then raise FailedToKill
    else Printf.fprintf stderr "Successfully killed server for %s\n%!"
      (Path.string_of_path env.root)

  let kill_server env =
    Printf.fprintf stderr "Killing server for %s\n%!"
      (Path.string_of_path env.root);
    try nice_kill env
    with FailedToKill ->
      Printf.fprintf stderr "Failed to kill server nicely for %s\n%!"
        (Path.string_of_path env.root);
      try mean_kill env
      with FailedToKill ->
        Printf.fprintf stderr "Failed to kill server meanly for %s\n%!"
          (Path.string_of_path env.root);
        exit 1

  let main env =
    if ClientUtils.server_exists env.root
    then kill_server env
    else Printf.fprintf stderr "Error: no server to kill for %s\n%!"
      (Path.string_of_path env.root)

end
