(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module CCS = CommandConnectSimple

type env = {
  root : Path.t;
  autostart : bool;
  retries : int;
  retry_if_init : bool;
  expiry : float option;
  tmp_dir : string;
  shm_dir : string;
  log_file : string;
}

(* During initialization, we can provide the client with some clue as to what
 * the server is doing, even though we can't talk to the server yet through the
 * socket. The general idea is to tail the log file, match the last line
 * against a bunch of regexes, and show a message depending on the match *)
let parsing_re = Str.regexp_string "Parsing"

let infer_re = Str.regexp_string "Running local inference"

let merging_re = Str.regexp_string "Merging"

let server_ready_re = Str.regexp_string "Server is READY"

let matches_re re s =
  let pos = try Str.search_forward re s 0 with Not_found -> -1 in
  pos > -1

let re_list =
  [
    parsing_re;
    infer_re;
    merging_re;
    server_ready_re;
  ]

let is_valid_line s =
  List.exists (fun re -> matches_re re s) re_list


let msg_of_tail tail_env =
  let line = Tail.last_line tail_env in
  if matches_re parsing_re line then
    "[parsing]"
  else if matches_re infer_re line then
    "[local inference]"
  else if matches_re merging_re line then
    "[merging inference]"
  else if matches_re server_ready_re line then
    "[server is ready]"
  else
    "[processing]"

let delta_t : float = 3.0

(* Starts up a flow server by literally calling flow start *)
let start_flow_server ~tmp_dir ~shm_dir root =
  Utils.prerr_endlinef
    "Launching Flow server for %s"
    (Path.to_string root);
  let from_arg = match FlowEventLogger.((get_context ()).from) with
    | Some from -> from
    | None -> "" in
  let exe = Sys.argv.(0) in
  try
    let server_pid =
      Unix.(create_process exe
              [| exe; "start";
                 "--temp-dir"; tmp_dir;
                 "--shm-dir"; shm_dir;
                 "--from"; from_arg;
                 Path.to_string root |]
              stdin stdout stderr) in
    match Unix.waitpid [] server_pid with
      | _, Unix.WEXITED 0 -> true
      | _, Unix.WEXITED code when
          code = FlowExitStatus.(error_code Lock_stolen) -> false
      | _, status ->
        let msg = "Could not start Flow server!" in
        FlowExitStatus.(exit ~msg (Server_start_failed status))
  with _ ->
    let msg = "Could not start Flow server!" in
    FlowExitStatus.(exit ~msg Unknown_error)

type retry_info = {
  retries_remaining: int;
  original_retries: int;
  last_connect_time: float;
}

let reset_retries_if_necessary retries = function
  | Result.Error CCS.Server_missing
  | Result.Error CCS.Server_busy -> retries
  | Result.Ok _
  | Result.Error CCS.Build_id_mismatch
  | Result.Error CCS.Server_initializing ->
      { retries with
        retries_remaining = retries.original_retries;
      }

let consume_retry retries =
  let retries_remaining = retries.retries_remaining - 1 in
  (* Make sure there is at least 1 second between retries *)
  let sleep_time = int_of_float
    (ceil (1.0 -. (Unix.gettimeofday() -. retries.last_connect_time))) in
  if retries_remaining >= 0 && sleep_time > 0
  then Unix.sleep sleep_time;
  { retries with retries_remaining; }

(* A featureful wrapper around CommandConnectSimple.connect_once. This
 * function handles retries, timeouts, displaying messages during
 * initialization, etc *)
let rec connect env retries start_time tail_env =
  if retries.retries_remaining < 0
  then
    FlowExitStatus.(exit ~msg:"\nOut of retries, exiting!" Out_of_retries);
  let has_timed_out = match env.expiry with
    | None -> false
    | Some t -> Unix.time() > t
  in
  if has_timed_out
  then FlowExitStatus.(exit ~msg:"\nTimeout exceeded, exiting" Out_of_time);
  let retries = { retries with last_connect_time = Unix.gettimeofday () } in
  let conn = CCS.connect_once ~tmp_dir:env.tmp_dir env.root in
  let curr_time = Unix.time () in
  if not (Tail.is_open_env tail_env) &&
       curr_time -. start_time > delta_t then begin
      Tail.open_env tail_env;
      Tail.update_env is_valid_line tail_env;
    end else if Tail.is_open_env tail_env then
    Tail.update_env is_valid_line tail_env;
  Tail.set_lines tail_env [];
  let tail_msg = msg_of_tail tail_env in
  if Tty.spinner_used () then Tty.print_clear_line stderr;
  let retries = reset_retries_if_necessary retries conn in
  match conn with
  | Result.Ok (ic, oc) -> (ic, oc)
  | Result.Error CCS.Server_missing ->
      if env.autostart then begin
        let retries =
          if start_flow_server
               ~tmp_dir:env.tmp_dir ~shm_dir:env.shm_dir env.root
          then begin
            Printf.eprintf
              "Started a new flow server: %s%!"
              (Tty.spinner());
            retries
          end else begin
            Printf.eprintf
              "Failed to start a new flow server (%d %s remaining): %s %s%!"
              retries.retries_remaining
              (if retries.retries_remaining = 1 then "retry" else "retries")
              tail_msg
              (Tty.spinner());
            consume_retry retries
          end in
        connect env retries start_time tail_env
      end else begin
        let msg = Utils.spf
          "\nError: There is no Flow server running in '%s'."
          (Path.to_string env.root) in
        FlowExitStatus.(exit ~msg No_server_running)
      end
  | Result.Error CCS.Server_busy ->
      Printf.eprintf
        "The flow server is not responding (%d %s remaining): %s %s%!"
        retries.retries_remaining
        (if retries.retries_remaining = 1 then "retry" else "retries")
        tail_msg
        (Tty.spinner());
      connect env (consume_retry retries) start_time tail_env
  | Result.Error CCS.Build_id_mismatch ->
      let msg = "The flow server's version didn't match the client's, so it \
      exited." in
      if env.autostart
      then
        let start_time = Unix.time () in
        begin
          Utils.prerr_endlinef "%s\nGoing to launch a new one.\n%!" msg;
          (* Don't decrement retries -- the server is definitely not running,
           * so the next time round will hit Server_missing above, *but*
           * before that will actually start the server -- we need to make
           * sure that happens.
           *)
          Tail.close_env tail_env;
          connect env retries start_time tail_env
        end
      else
        let msg = "\n"^msg in
        FlowExitStatus.(exit ~msg Build_id_mismatch)
  | Result.Error CCS.Server_initializing ->
      let msg = "flow is still initializing; this can take some time." in
      if env.retry_if_init then begin
        Printf.eprintf "%s %s %s%!" msg tail_msg (Tty.spinner());
        connect env retries start_time tail_env
      end else begin
        let msg = "\n"^msg^" Not retrying since --retry-if-init is false." in
        FlowExitStatus.(exit ~msg Server_initializing)
      end

let connect env =
  let link_file = env.log_file in
  let log_file =
    if Sys.win32 && Sys.file_exists link_file then
      Sys_utils.cat link_file
    else
      link_file in
  let start_time = Unix.time () in
  let tail_env = Tail.create_env log_file in
  let retries = {
    retries_remaining = env.retries;
    original_retries = env.retries;
    last_connect_time = Unix.gettimeofday ();
  } in

  let res = connect env retries start_time tail_env in
  Tail.close_env tail_env;
  res
