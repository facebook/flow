(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module CCS = ClientConnectSimple

let get_hhserver () =
  let server_next_to_client = (Filename.dirname Sys.argv.(0)) ^ "/hh_server" in
  if Sys.file_exists server_next_to_client
  then server_next_to_client
  else "hh_server"

type env = {
  root: Path.t;
  wait: bool;
  no_load : bool;
}

let start_server env =
  let hh_server = Printf.sprintf "%s -d %s %s --waiting-client %d"
    (Filename.quote (get_hhserver ()))
    (Filename.quote (Path.to_string env.root))
    (if env.no_load then "--no-load" else "")
    (Unix.getpid ())
  in
  Printf.eprintf "Server launched with the following command:\n\t%s\n%!"
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
  let root_s = Path.to_string env.root in
  match CCS.connect_once env.root with
  | Result.Ok _conn -> false
  | Result.Error CCS.Server_missing
  | Result.Error CCS.Build_id_mismatch -> true
  | Result.Error CCS.Server_initializing ->
      Printf.eprintf "Found initializing server for %s\n%!" root_s;
      false
  | Result.Error CCS.Server_busy ->
      Printf.eprintf "Replacing unresponsive server for %s\n%!" root_s;
      HackClientStop.kill_server env.root;
      true

let main env =
  if should_start env
  then begin
    start_server env;
    Exit_status.Ok
  end else begin
    Printf.eprintf
      "Error: Server already exists for %s\n\
      Use hh_client restart if you want to kill it and start a new one\n%!"
      (Path.to_string env.root);
    Exit_status.Server_already_exists
  end
