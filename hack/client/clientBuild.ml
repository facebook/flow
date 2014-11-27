(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* 800s was chosen because it was above most of the historical p95 of
 * hack server startup times as observed here:
 * https://fburl.com/48825801, see also https://fburl.com/29184831 *)
let num_build_retries = 800

type env = {
  root : Path.path;
  build_opts : ServerMsg.build_opts;
  server_options_cmd : string option;
}

let rec connect env retries =
  try
    let result = ClientUtils.connect env.root in
    if Tty.spinner_used() then Tty.print_clear_line stdout;
    result
  with
  | ClientExceptions.Server_cant_connect ->
    Printf.printf "Can't connect to server yet, retrying.\n%!";
    if retries > 0
    then begin
      Unix.sleep 1;
      connect env (retries - 1)
    end
    else exit 2
  | ClientExceptions.Server_initializing ->
    Printf.printf
      (* This extra space before the \r is here to erase the spinner
         when the length of this line decreases (but by at most 1!) as
         it ticks down. We don't want to rely on Tty.print_clear_line
         --- it would emit newlines when stdout is not a tty, and
         obviate the effect of the \r. *)
      "Hack server still initializing. (will wait %d more seconds) %s \r%!"
      retries (Tty.spinner());
    if retries > 0
    then begin
      Unix.sleep 1;
      connect env (retries - 1)
    end
    else begin
      if Tty.spinner_used() then Tty.print_clear_line stdout;
      Printf.printf "Waited >%ds for hack server initialization.\n%s\n%s\n%s\n%!"
        num_build_retries
        "Your hack server is still initializing. This is an IO-bound"
        "operation and may take a while if your disk cache is cold."
        "Trying the build again may work; the server may be caught up now.";
      exit 2
    end

let rec main_ env retries =
  (* Check if a server is up *)
  if not (ClientUtils.server_exists env.root)
  then ClientStart.start_server { ClientStart.
    root = env.root;
    wait = false;
    server_options_cmd = env.server_options_cmd;
  };
  let ic, oc = connect env retries in
  ServerMsg.cmd_to_channel oc (ServerMsg.BUILD env.build_opts);
  let response = ServerMsg.response_from_channel ic in
  match response with
  | ServerMsg.SERVER_OUT_OF_DATE ->
    Printf.printf
      "Hack server is an old version, trying again.\n%!";
    Unix.sleep 2;
    main_ env (retries - 1)
  | ServerMsg.PONG -> (* successful case *)
    begin
      let exit_code = ref 0 in
      EventLogger.client_begin_work (ClientLogCommand.LCBuild
        (env.root, env.build_opts.ServerMsg.incremental));
      try
        while true do
          let line:ServerMsg.build_progress = Marshal.from_channel ic in
          match line with
          | ServerMsg.BUILD_PROGRESS s -> print_endline s
          | ServerMsg.BUILD_ERROR s -> exit_code := 2; print_endline s
        done
      with End_of_file ->
        if !exit_code = 0
        then ()
        else exit (!exit_code)
    end
  | resp -> Printf.printf "Unexpected server response %s.\n%!"
    (ServerMsg.response_to_string resp)

let main env =
  main_ env num_build_retries
