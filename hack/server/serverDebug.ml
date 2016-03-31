(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let log genv msg_thunk =
  Option.iter genv.ServerEnv.debug_channels begin fun (_ic, oc) ->
    (* The input is read using input_line, hence we append a trailing
     * newline *)
    (* We use a thunk so that expensive computations don't slow down the
     * server when the debug listener isn't attached *)
    let msg = msg_thunk () in
    try
      output_string oc ((Hh_json.json_to_string msg) ^ "\n");
      flush oc;
    with Sys_error "Broken pipe" -> begin
      Hh_logger.log "Debug listener has gone away.";
      genv.ServerEnv.debug_channels <- None;
    end
  end

let info genv msg =
  let open Hh_json in
  let msg = JSON_Object [
      "type", JSON_String "info";
      "data", JSON_String msg;
  ] in
  log genv (fun () -> msg)

let say_hello genv = info genv "hello"
