(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let num_build_retries = 60

type env = {
  root: Path.path;
  server_options_cmd: string option;
}

let rec connect env retries =
  try
    let result = ClientUtils.connect env.root in
    Printf.printf "\n%!";
    result
  with
  | ClientExceptions.Server_cant_connect ->
    if retries > 0
    then begin
      Printf.printf "Can't connect to server yet, retrying.\n%!";
      Unix.sleep 1;
      connect env (retries - 1)
    end
    else begin
      Printf.fprintf stderr
        "Error: could not connect to hh_server, giving up!\n%!";
      exit 2
    end
  | ClientExceptions.Server_initializing ->
    Printf.printf "Server still initializing. %s\r%!" (Tty.spinner());
    if retries > 0
    then (
      Unix.sleep(1);
      connect env (retries - 1)
    )
    else exit 2

let main env =
  if not (ClientUtils.server_exists env.root)
  then ClientStart.start_server { ClientStart.
    root = env.root;
    wait = false;
    server_options_cmd = env.server_options_cmd;
  };
  let ic, oc = connect env num_build_retries in
  ServerMsg.cmd_to_channel oc ServerMsg.PROLOG;
  try
    while true do
      let str = input_line ic in
      print_endline str;
      (* coupling: hh_server.ml *)
      if Str.string_match (Str.regexp "PROLOG_READY:\\(.*\\)") str 0
      then begin
        let path = Str.matched_group 1 str in
        close_out oc;
        Unix.execv path [||]
      end
    done
  with End_of_file ->
    ()
