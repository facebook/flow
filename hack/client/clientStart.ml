(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open ClientExceptions
open Utils

let get_hhserver () =
  let server_next_to_client = (Filename.dirname Sys.argv.(0)) ^ "/hh_server" in
  if Sys.file_exists server_next_to_client
  then server_next_to_client
  else "hh_server"

type env = {
  root: Path.path;
  wait: bool;
  no_load : bool;
}

let rec wait env =
  begin try
    Unix.sleep(1);
    ignore(ClientUtils.connect env.root);
    Printf.fprintf stderr "Done waiting!\n%!"
  with
  | Server_initializing ->
    Printf.fprintf stderr "Waiting for server to initialize\n%!";
    wait env
  | e ->
    Printf.fprintf stderr
      "Error: something went wrong while waiting for the server to start up\n%s\n%!"
      (Printexc.to_string e);
    exit 77
  end

let start_server env =
  let hh_server = Printf.sprintf "%s -d %s %s"
    (Filename.quote (get_hhserver ()))
    (Filename.quote (Path.string_of_path env.root))
    (if env.no_load then "--no-load" else "") in
  Printf.fprintf stderr "Server launched with the following command:\n\t%s\n%!"
    hh_server;
  let () = match Unix.system hh_server with
    | Unix.WEXITED 0 -> ()
    | _ -> Printf.fprintf stderr "Could not start hh_server!\n"; exit 77 in
  if env.wait then wait env;
  ()

let should_start env =
  if ClientUtils.server_exists env.root
  then begin
    try
      (* Let's ping the server to make sure it's up and not out of date *)
      let response = with_context
        ~enter:(fun () ->
          Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ ->
            raise Server_busy));
          ignore (Unix.alarm 6))
        ~exit:(fun () ->
          ignore (Unix.alarm 0);
          Sys.set_signal Sys.sigalrm Sys.Signal_default)
        ~do_:(fun () ->
          let ic, oc = ClientUtils.connect env.root in
          ServerMsg.cmd_to_channel oc ServerMsg.PING;
          ServerMsg.response_from_channel ic) in
      match response with
      | ServerMsg.PONG -> false
      | ServerMsg.SERVER_OUT_OF_DATE ->
          Printf.fprintf
            stderr
            "Replacing out of date server for %s\n%!"
            (Path.string_of_path env.root);
          ignore(Unix.sleep 1);
          true
      | ServerMsg.SERVER_DYING
      | ServerMsg.NO_ERRORS
      | ServerMsg.ERRORS _
      | ServerMsg.DIRECTORY_MISMATCH _ ->
        let r = (ServerMsg.response_to_string response) in
        failwith ("Unexpected response from the server: "^r)
    with
      | Server_busy ->
          Printf.fprintf
            stderr
            "Replacing busy server for %s\n%!"
            (Path.string_of_path env.root);
          HackClientStop.kill_server env.root;
          true
      | Server_initializing ->
          Printf.fprintf
            stderr
            "Found initializing server for %s\n%!"
            (Path.string_of_path env.root);
          false
      | Server_cant_connect ->
          Printf.fprintf
            stderr
            "Replacing unresponsive server for %s\n%!"
            (Path.string_of_path env.root);
          HackClientStop.kill_server env.root;
          true
  end else true

let main env =
  if should_start env
  then start_server env
  else begin
    Printf.fprintf
      stderr
      "Error: Server already exists for %s\n\
      Use hh_client restart if you want to kill it and start a new one\n%!"
      (Path.string_of_path env.root);
    exit 77
  end
