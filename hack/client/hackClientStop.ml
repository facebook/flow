(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module HackConfig : ClientStop.STOP_CONFIG = struct
  type response = ServerMsg.response

  let server_desc = "Hack"

  let server_name = "hh_server"

  let kill_cmd_to_channel oc =
    ServerMsg.cmd_to_channel oc ServerMsg.KILL

  let response_from_channel = ServerMsg.response_from_channel

  let response_to_string = ServerMsg.response_to_string

  let is_expected = function
    | ServerMsg.SERVER_OUT_OF_DATE
    | ServerMsg.SERVER_DYING ->
        true
    | _ ->
        false

end

module HackStopCommand = ClientStop.StopCommand (HackConfig)

let main = HackStopCommand.main

let kill_server root =
  HackStopCommand.kill_server { ClientStop.root }
