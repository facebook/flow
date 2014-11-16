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
}

let rec connect env retries =
  try
    let result = ClientUtils.connect env.root in
    Printf.printf "\n%!";
    result
  with
  | ClientExceptions.Server_cant_connect ->
    Printf.printf
      "Can't connect to the server\n\
      Try 'hh start your_directory'\n";
    exit 2
  | ClientExceptions.Server_initializing ->
    Printf.printf "Server still initializing. %s\r%!" (Tty.spinner());
    if retries > 0
    then (
      Unix.sleep(1);
      connect env (retries - 1)
    )
    else exit 2

let main env =
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
