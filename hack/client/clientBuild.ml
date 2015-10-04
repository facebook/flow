(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Sys_utils

(* 800s was chosen because it was above most of the historical p95 of
 * hack server startup times as observed here:
 * https://fburl.com/48825801, see also https://fburl.com/29184831 *)
let num_build_retries = 800

type env = {
  root : Path.path;
  build_opts : ServerMsg.build_opts;
}

let build_kind_of build_opts =
  let module LC = ClientLogCommand in
  let {ServerMsg.steps; no_steps; is_push; incremental; _} = build_opts in
  if steps <> None || no_steps <> None then
    LC.Steps
  else if is_push then
    LC.Push
  else if incremental then
    LC.Incremental
  else
    LC.Full

let should_retry env tries = env.build_opts.ServerMsg.wait || tries > 0

let rec connect env retries =
  try
    let result = ClientUtils.connect env.root in
    if Tty.spinner_used() then Tty.print_clear_line stdout;
    result
  with
  | ClientExceptions.Server_cant_connect ->
    Printf.printf "Can't connect to server yet, retrying.\n%!";
    if should_retry env retries
    then begin
      Unix.sleep 1;
      connect env (retries - 1)
    end
    else exit 2
  | ClientExceptions.Server_initializing ->
    let wait_msg = if env.build_opts.ServerMsg.wait
                   then Printf.sprintf "will wait forever due to --wait option, have waited %d seconds" (num_build_retries - retries)
                   else Printf.sprintf "will wait %d more seconds" retries in
    Printf.printf
      (* This extra space before the \r is here to erase the spinner
         when the length of this line decreases (but by at most 1!) as
         it ticks down. We don't want to rely on Tty.print_clear_line
         --- it would emit newlines when stdout is not a tty, and
         obviate the effect of the \r. *)
      "Hack server still initializing. (%s) %s \r%!"
      wait_msg (Tty.spinner());
    if should_retry env retries
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

let rec wait_for_response ic =
  try
    with_timeout 1
      ~on_timeout:(fun _ -> raise ClientExceptions.Server_busy)
      ~do_:(fun () ->
        let response = ServerMsg.response_from_channel ic in
        if Tty.spinner_used() then Tty.print_clear_line stdout;
        response)
  with
  | End_of_file ->
     prerr_string "Server disconnected or crashed. Try `hh_client restart`\n";
     flush stderr;
     exit 1
  | ClientExceptions.Server_busy ->
     (* We timed out waiting for response from hh_server, update message *)
     Printf.printf
       "Awaiting response from hh_server, hh_server typechecking... %s \r%!"
       (Tty.spinner());
     wait_for_response ic

let wait_on_server_restart ic =
  try
    while true do
      let _ = input_char ic in
      ()
    done
  with
  | End_of_file
  | Sys_error _ ->
     (* Server has exited and hung up on us *)
     Printf.printf "Old server has exited\n%!";
     ()

let rec main_ env retries =
  (* Check if a server is up *)
  if not (ClientUtils.server_exists env.root)
  then ClientStart.start_server { ClientStart.
    root = env.root;
    wait = false;
    no_load = false;
  };
  let ic, oc = connect env retries in
  ServerMsg.cmd_to_channel oc (ServerMsg.BUILD env.build_opts);
  try
    let response = wait_for_response ic in
    handle_response response env retries ic
  with
  | ClientExceptions.Server_out_of_date ->
     (* The server is out of date and is going to exit. Subsequent calls to
      * connect on the Unix Domain Socket might succeed, connecting to the
      * server that is about to die, and eventually we will be hung up on
      * while trying to read from our end.
      *
      * To avoid that fate, when we know the server is about to exit, we wait
      * for the connection to be closed, signaling that the server has exited
      * and the OS has cleaned up after it, then we try again.
      *)
     Printf.printf "%s%s%!"
                   "Hack server is an old version, waiting for it to exit,"
                   " then will start the new server\n";
     wait_on_server_restart ic;
     close_in_noerr ic;
     main_ env (retries - 1)
  | Sys_error _ ->
     (* Connection reset, handle gracefully and try again *)
     close_in_noerr ic;
     if should_retry env retries then
       (Printf.printf "Connection hung up, trying again \n%!";
        main_ env (retries - 1))
     else
       (Printf.printf "Error: Client failed to connect to server!!\n%!";
        exit 2)

and handle_response response env retries ic =
  match response with
  | ServerMsg.SERVER_OUT_OF_DATE ->
    raise ClientExceptions.Server_out_of_date;
  | ServerMsg.PONG -> (* successful case *)
    begin
      let finished = ref false in
      let exit_code = ref 0 in
      EventLogger.client_begin_work (ClientLogCommand.LCBuild
        (env.root, build_kind_of env.build_opts));
      try
        while true do
          let line:ServerMsg.build_progress = Marshal.from_channel ic in
          match line with
          | ServerMsg.BUILD_PROGRESS s -> print_endline s
          | ServerMsg.BUILD_ERROR s -> exit_code := 2; print_endline s
          | ServerMsg.BUILD_FINISHED -> finished := true
        done
      with
      | End_of_file ->
        if not !finished then begin
          Printf.fprintf stderr ("Build unexpectedly terminated! "^^
            "You may need to do `hh_client restart`.\n");
          exit 1
        end;
        if !exit_code = 0
        then ()
        else exit (!exit_code)
      | Failure _ as e ->
        (* We are seeing Failure "input value: bad object" which can
         * realistically only happen from Marshal.from_channel ic.
         * This admittedly won't help us root cause this, but at least
         * this will help us identify where it is occurring
         *)
        let backtrace = Printexc.get_backtrace () in
        let e_str = Printexc.to_string e in
        Printf.fprintf stderr "Unexpected error: %s\n%s%!" e_str backtrace;
        raise e
    end
  | resp -> Printf.printf "Unexpected server response %s.\n%!"
    (ServerMsg.response_to_string resp)

let main env =
  main_ env num_build_retries
