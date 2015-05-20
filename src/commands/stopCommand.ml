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
    |> anon "root" (optional string) ~doc:"Root directory"
  )
}

module FlowConfig : ClientStop.STOP_CONFIG = struct
  type response = ServerProt.response

  let server_desc = "Flow"

  let server_name = "flow"

  let kill (ic, oc) =
    ServerProt.cmd_to_channel oc ServerProt.KILL;
    ServerProt.response_from_channel ic

  let response_to_string = ServerProt.response_to_string

  let is_expected = function
    | ServerProt.SERVER_DYING
    | ServerProt.SERVER_OUT_OF_DATE ->
        true
    | _ ->
        false

end

module FlowStopCommand = ClientStop.StopCommand (FlowConfig)

let main root () = FlowStopCommand.kill_server {
    ClientStop.root = CommandUtils.guess_root root;
  }

let command = CommandSpec.command spec main
