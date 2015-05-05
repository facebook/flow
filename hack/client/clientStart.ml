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
open Sys_utils

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

let start_server env =
  let hh_server = Printf.sprintf "%s -d %s %s --waiting-client %d"
    (Filename.quote (get_hhserver ()))
    (Filename.quote (Path.string_of_path env.root))
    (if env.no_load then "--no-load" else "")
    (Unix.getpid ())
  in
  Printf.fprintf stderr "Server launched with the following command:\n\t%s\n%!"
    hh_server;

  (* Start up the hh_server, and wait on SIGUSR1, which is sent to us at various
   * stages of the start up process. See if we're in the state we want to be in;
   * if not, go to sleep again. *)
  let old_mask = Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigusr1] in
  (* Have to actually handle and discard the signal -- if it's ignored, it won't
   * break the sigsuspend. *)
  let old_handle = Sys.signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> ())) in

  let rec wait_loop () =
    Unix.sigsuspend old_mask;
    (* NB: SIGUSR1 is now blocked again. *)
    if env.wait && not (Lock.check env.root "init") then
      wait_loop ()
    else
      ()
  in

  (match Unix.system hh_server with
    | Unix.WEXITED 0 -> ()
    | _ -> Printf.fprintf stderr "Could not start hh_server!\n"; exit 77);
  wait_loop ();

  let _ = Sys.signal Sys.sigusr1 old_handle in
  let _ = Unix.sigprocmask Unix.SIG_SETMASK old_mask in
  ()

let should_start env =
  try
    with_timeout 6
      ~on_timeout:(fun _ -> raise Server_busy)
      ~do_:(fun () ->
        ignore (ClientUtils.connect env.root);
        false)
  with
  | Server_missing -> true
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
  | Server_out_of_date ->
      Printf.eprintf
        "Replacing out of date server for %s\n%!"
        (Path.string_of_path env.root);
      ignore(Unix.sleep 1);
      true

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
