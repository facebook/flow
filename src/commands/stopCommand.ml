(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

(***********************************************************************)
(* flow stop command *)
(***********************************************************************)

let parse_args () =
  let usage =
      "Usage: flow stop [OPTION]... [ROOT]\n\
      Stops a flow server\n\n\
      Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
      ROOT is assumed to be current directory if unspecified\n" in
  let options = [] in
  let args = ClientArgs.parse_without_command options usage "stop" in
  let root =
    match args with
    | [] -> CommandUtils.guess_root None
    | [x] -> CommandUtils.guess_root (Some x)
    | _ ->
        prerr_endline "Error: please provide at most one root directory";
        exit 1
  in { ClientStop.root = root; }

module FlowConfig : ClientStop.STOP_CONFIG = struct
  type response = ServerProt.response

  let server_desc = "Flow"

  let server_name = "flow"

  let kill_cmd_to_channel oc =
    ServerProt.cmd_to_channel oc ServerProt.KILL

  let response_from_channel = ServerProt.response_from_channel

  let response_to_string = ServerProt.response_to_string

  let is_expected = function
    | ServerProt.SERVER_DYING
    | ServerProt.SERVER_OUT_OF_DATE ->
        true
    | _ ->
        false

end

module FlowStopCommand = ClientStop.StopCommand (FlowConfig)

let run () =
  FlowStopCommand.main (parse_args ())
