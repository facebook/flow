(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module SMUtils = ServerMonitorUtils

exception Server_hung_up

type env = {
  root : Path.t;
  autostart : bool;
  retries : int option;
  retry_if_init : bool;
  expiry : float option;
  no_load : bool;
}

let running_load_script_re = Str.regexp_string "Running load script"

let load_state_found_re = Str.regexp_string "Load state found"

let load_state_not_found_re = Str.regexp_string "Load state not found"

let parsing_re = Str.regexp_string "Parsing"

let naming_re = Str.regexp_string "Naming"

let determining_changes_re = Str.regexp_string "Determining changes"

let type_decl_re = Str.regexp_string "Type-decl"

let type_check_re = Str.regexp_string "Type-check"

let server_ready_re = Str.regexp_string "Server is READY"

let matches_re re s =
  let pos = try Str.search_forward re s 0 with Not_found -> -1 in
  pos > -1

let re_list =
  [running_load_script_re;
   load_state_found_re;
   load_state_not_found_re;
   parsing_re;
   naming_re;
   determining_changes_re;
   type_decl_re;
   type_check_re;
   server_ready_re;
  ]

let is_valid_line s =
  List.exists (fun re -> matches_re re s) re_list


let rec is_load_state_not_found l =
  match l with
  | [] -> false
  | s::ss ->
     if matches_re load_state_found_re s then
       false
     else if matches_re load_state_not_found_re s then
       true
     else if matches_re server_ready_re s then
       false
     else
       is_load_state_not_found ss

let msg_of_tail tail_env =
  let line = Tail.last_line tail_env in
  if matches_re running_load_script_re line then
    "[running load script]"
  else if matches_re load_state_found_re line then
    "[load state found]"
  else if matches_re load_state_not_found_re line then
    "[load state not found]"
  else if matches_re parsing_re line then
    "[parsing]"
  else if matches_re naming_re line then
    "[naming]"
  else if matches_re determining_changes_re line then
    "[determining changes]"
  else if matches_re type_decl_re line then
    "[type decl]"
  else if matches_re type_check_re line then
    "[type check]"
  else if matches_re server_ready_re line then
    "[server is ready]"
  else
    "[processing]"

let delta_t : float = 3.0

let open_and_get_tail_msg start_time tail_env =
  let curr_time = Unix.time () in
  if not (Tail.is_open_env tail_env) &&
       curr_time -. start_time > delta_t then begin
      Tail.open_env tail_env;
      Tail.update_env is_valid_line tail_env;
    end else if Tail.is_open_env tail_env then
    Tail.update_env is_valid_line tail_env;
  let load_state_not_found =
    is_load_state_not_found (Tail.get_lines tail_env) in
  Tail.set_lines tail_env [];
  let tail_msg = msg_of_tail tail_env in
  load_state_not_found, tail_msg

let print_wait_msg ?(first_call=false) start_time tail_env =
  if (not first_call) && Tty.spinner_used () then
    Tty.print_clear_line stderr;
  let load_state_not_found, tail_msg =
    open_and_get_tail_msg start_time tail_env in
  if load_state_not_found then
    Printf.eprintf "%s\n%!" ClientMessages.load_state_not_found_msg;
  Tty.eprintf "hh_server is busy: %s %s%!" tail_msg (Tty.spinner());
  ()

(** Sleeps until the server says hello. While waiting, prints out spinner and
 * useful messages by tailing the server logs. *)
let rec wait_for_server_hello ic env retries start_time tail_env first_call =
  match retries with
  | Some n when n < 0 ->
      Printf.eprintf "\nError: Ran out of retries, giving up!\n";
      raise Exit_status.(Exit_with Out_of_retries)
  | Some _
  | None -> ();
  let readable, _, _  = Unix.select
    [Timeout.descr_of_in_channel ic] [] [Timeout.descr_of_in_channel ic] 1.0 in
  if readable = [] then (
    print_wait_msg ~first_call start_time tail_env;
    wait_for_server_hello ic env (Option.map retries (fun x -> x - 1))
      start_time tail_env false
  ) else
    try
      (match Timeout.input_line ic with
      | "Hello" ->
        ()
      | _ ->
        print_wait_msg ~first_call start_time tail_env;
        wait_for_server_hello ic env (Option.map retries (fun x -> x - 1))
          start_time tail_env false
      )
    with
    | End_of_file
    | Sys_error _ ->
      raise Server_hung_up

let rec connect ?(first_attempt=false) env retries start_time tail_env =
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
  let connect_once_start_t = Unix.time () in
  let conn = ServerUtils.connect_to_monitor env.root
    HhServerMonitorConfig.Program.hh_server in
  HackEventLogger.client_connect_once connect_once_start_t;
  let _, tail_msg = open_and_get_tail_msg start_time tail_env in
  match conn with
  | Result.Ok (ic, oc) ->
      (try begin
        wait_for_server_hello ic env retries start_time tail_env true;
        if Tty.spinner_used () then
          Tty.print_clear_line stderr
      end
      with
      | Server_hung_up ->
        (Printf.eprintf "hh_server died unexpectedly. Maybe you recently \
        launched a different version of hh_server. Now exiting hh_client.";
        Exit_status.exit Exit_status.No_server_running)
      );
      (ic, oc)
  | Result.Error e ->
    if first_attempt then
      Printf.eprintf
        "For more detailed logs, try `tail -f $(hh_client --monitor-logname) \
        $(hh_client --logname)`\n";
    match e with
    | SMUtils.Server_died
    | SMUtils.Monitor_connection_failure ->
      connect env (Option.map retries (fun x -> x - 1)) start_time tail_env
    | SMUtils.Server_missing ->
      if env.autostart then begin
        ClientStart.start_server { ClientStart.
          root = env.root;
          wait = false;
          no_load = env.no_load;
          silent = false;
        };
        connect env retries start_time tail_env
      end else begin
        Printf.eprintf begin
          "Error: no hh_server running. Either start hh_server"^^
          " yourself or run hh_client without --autostart-server false\n%!"
        end;
        raise Exit_status.(Exit_with No_server_running)
      end
    | SMUtils.Server_busy ->
      (** This should only happen during the transition from old-world to
      * new-world.
      *
      * In the monitor-typechecker split role world, this should never happen
      * because if a monitor already exists, it readily accepts connections even
      * when the typechecker hasn't finished initializing and if one doesn't
      * exist then the client starts one and waits until the typechecker is
      * finished before attempting a connection. *)
      if Tty.spinner_used () then Tty.print_clear_line stderr;
      Tty.eprintf "hh_server is busy: %s %s\n%!" tail_msg
        (Tty.spinner());
      HackEventLogger.client_connect_once_busy start_time;
      connect env (Option.map retries (fun x -> x - 1)) start_time tail_env
    | SMUtils.Build_id_mismatched ->
      Printf.eprintf begin
        "hh_server's version doesn't match the client's, "^^
        "so it will exit.\n%!"
      end;
      if env.autostart
      then
        let start_time = Unix.time () in
        begin
          Printf.eprintf "Going to launch a new one.\n%!";
          (* Don't decrement retries -- the server is definitely not running,
           * so the next time round will hit Server_missing above, *but*
           * before that will actually start the server -- we need to make
           * sure that happens.
           *)
          Tail.close_env tail_env;
          connect env retries start_time tail_env
        end else raise Exit_status.(Exit_with Build_id_mismatch)

let connect env =
  let link_file = ServerFiles.log_link env.root in
  let log_file = Sys_utils.readlink_no_fail link_file in
  let start_time = Unix.time () in
  let tail_env = Tail.create_env log_file in
  try
    let res = connect ~first_attempt:true env env.retries start_time tail_env in
    Tail.close_env tail_env;
    HackEventLogger.client_established_connection start_time;
    res
  with
  | e ->
    HackEventLogger.client_establish_connection_exception e;
    raise e
