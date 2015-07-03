(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module CCS = ClientConnectSimple

type env = {
  root : Path.t;
  autostart : bool;
  retries : int option;
  retry_if_init : bool;
  expiry : float option;
  no_load : bool;
}

let rec connect env retries =
  match retries with
  | Some n when n < 0 ->
      Printf.eprintf "\nError: Ran out of retries, giving up!\n";
      raise Exit_status.(Exit_with Out_of_retries)
  | Some _
  | None -> ();
  let has_timed_out = match env.expiry with
    | None -> false
    | Some t -> Unix.time() > t
  in
  if has_timed_out then begin
    Printf.eprintf "\nError: hh_client hit timeout, giving up!\n%!";
    raise Exit_status.(Exit_with Out_of_time)
  end;
  let conn = CCS.connect_once env.root in
  if Tty.spinner_used () then Tty.print_clear_line stderr;
  match conn with
  | Result.Ok (ic, oc) -> (ic, oc)
  | Result.Error CCS.Server_missing ->
      if env.autostart then begin
        ClientStart.start_server { ClientStart.
          root = env.root;
          wait = false;
          no_load = env.no_load;
        };
        connect env retries
      end else begin
        Printf.eprintf begin
          "Error: no hh_server running. Either start hh_server"^^
          " yourself or run hh_client without --autostart-server false\n%!"
        end;
        raise Exit_status.(Exit_with No_server_running)
      end
  | Result.Error CCS.Server_busy ->
      Printf.eprintf "hh_server is busy, retrying... %s%!" (Tty.spinner());
      Unix.sleep 1;
      connect env (Option.map retries (fun x -> x - 1))
  | Result.Error CCS.Build_id_mismatch ->
      Printf.eprintf begin
        "hh_server's version doesn't match the client's, "^^
        "so it has exited.\n%!"
      end;
      if env.autostart
      then begin
        Printf.eprintf "Going to launch a new one.\n%!";
        (* Don't decrement retries -- the server is definitely not running, so
         * the next time round will hit Server_missing above, *but* before that
         * will actually start the server -- we need to make sure that happens.
         *)
        connect env retries
      end else raise Exit_status.(Exit_with No_server_running)
  | Result.Error CCS.Server_initializing ->
      Printf.eprintf begin
        "hh_server still initializing. This can take some time.%!"
      end;
      if env.retry_if_init
      then begin
        Printf.eprintf " Retrying... %s%!" (Tty.spinner());
        Unix.sleep 1;
        connect env retries
      end else begin
        Printf.eprintf " Not retrying since --retry-if-init is false.\n%!";
        raise Exit_status.(Exit_with Server_initializing)
      end

let connect env = connect env env.retries
