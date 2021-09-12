(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module CCS = CommandConnectSimple

type env = {
  root: Path.t;
  autostart: bool;
  retries: int;
  expiry: float option;
  lazy_mode: string option;
  autostop: bool;
  tmp_dir: string;
  shm_hash_table_pow: int option;
  shm_log_level: int option;
  log_file: string;
  ignore_version: bool;
  emoji: bool;
  quiet: bool;
  flowconfig_name: string;
  rerun_on_mismatch: bool;
}

let arg name value arr =
  match value with
  | None -> arr
  | Some value -> name :: value :: arr

let arg_map name ~f value arr =
  let value = Base.Option.map ~f value in
  arg name value arr

let flag name value arr =
  if value then
    name :: arr
  else
    arr

(* Starts up a flow server by literally calling flow start *)
let start_flow_server env =
  let {
    tmp_dir;
    lazy_mode;
    autostop;
    shm_hash_table_pow;
    shm_log_level;
    ignore_version;
    root;
    quiet;
    flowconfig_name;
    _;
  } =
    env
  in
  let exe = Sys.executable_name in
  let args =
    [Path.to_string root]
    |> arg_map "--sharedmemory-hash-table-pow" ~f:string_of_int shm_hash_table_pow
    |> arg_map "--sharedmemory-log-level" ~f:string_of_int shm_log_level
    |> arg "--lazy-mode" lazy_mode
    |> arg "--temp-dir" (Some tmp_dir)
    |> arg "--from" (FlowEventLogger.get_from_I_AM_A_CLOWN ())
    |> flag "--ignore-version" ignore_version
    |> flag "--quiet" quiet
    |> flag "--autostop" autostop
    |> arg "--flowconfig-name" (Some flowconfig_name)
  in
  try
    let server_pid =
      Unix.(create_process exe (Array.of_list (exe :: "start" :: args)) stdin stdout stderr)
    in
    match Sys_utils.waitpid_non_intr [] server_pid with
    | (_, Unix.WEXITED 0) -> Ok ()
    | (_, Unix.WEXITED code) when code = Exit.(error_code Lock_stolen) ->
      Error ("Lock stolen", Exit.Lock_stolen)
    | (_, status) ->
      let msg = "Could not start Flow server!" in
      Error (msg, Exit.Server_start_failed status)
  with
  | exn ->
    let exn = Exception.wrap exn in
    let msg =
      Printf.sprintf
        "Could not start Flow server! Unexpected exception: %s"
        (Exception.to_string exn)
    in
    Error (msg, Exit.Unknown_error)

type retry_info = {
  retries_remaining: int;
  original_retries: int;
  last_connect_time: float;
}

let reset_retries_if_necessary retries = function
  | Error (CCS.Server_busy (CCS.Fail_on_init _)) -> { retries with retries_remaining = 0 }
  | Error CCS.Server_missing
  | Error (CCS.Server_busy _) ->
    retries
  | Ok _
  | Error CCS.Server_socket_missing
  | Error (CCS.Build_id_mismatch _) ->
    { retries with retries_remaining = retries.original_retries }

let rate_limit retries =
  (* Make sure there is at least 1 second between retries *)
  let sleep_time =
    int_of_float (ceil (1.0 -. (Unix.gettimeofday () -. retries.last_connect_time)))
  in
  if sleep_time > 0 then Unix.sleep sleep_time

let consume_retry retries =
  let retries_remaining = retries.retries_remaining - 1 in
  if retries_remaining >= 0 then rate_limit retries;
  { retries with retries_remaining }

(* A featureful wrapper around CommandConnectSimple.connect_once. This
 * function handles retries, timeouts, displaying messages during
 * initialization, etc *)
let rec connect ~flowconfig_name ~client_handshake env retries start_time =
  let connect = connect ~flowconfig_name in
  (if retries.retries_remaining < 0 then
    Exit.(exit ~msg:"\nOut of retries, exiting!" Out_of_retries));
  let has_timed_out =
    match env.expiry with
    | None -> false
    | Some t -> Unix.gettimeofday () > t
  in
  (if has_timed_out then Exit.(exit ~msg:"\nTimeout exceeded, exiting" Out_of_time));
  let retries = { retries with last_connect_time = Unix.gettimeofday () } in
  let conn = CCS.connect_once ~flowconfig_name ~client_handshake ~tmp_dir:env.tmp_dir env.root in
  if Tty.spinner_used () then Tty.print_clear_line stderr;
  let retries = reset_retries_if_necessary retries conn in
  match conn with
  | Ok (ic, oc) -> (ic, oc)
  | Error CCS.Server_missing ->
    handle_missing_server ~flowconfig_name ~client_handshake env retries start_time
  | Error (CCS.Server_busy busy_reason) ->
    let busy_reason =
      match busy_reason with
      | CCS.Too_many_clients -> "has too many clients and rejected our connection"
      | CCS.Not_responding -> "is not responding"
      | CCS.Fail_on_init _ -> "is still initializing and the client used --retry-if-init false"
    in
    if not env.quiet then
      Printf.eprintf
        "The flow server %s (%d %s remaining): %s%!"
        busy_reason
        retries.retries_remaining
        (if retries.retries_remaining = 1 then
          "retry"
        else
          "retries")
        (Tty.spinner ());
    connect ~client_handshake env (consume_retry retries) start_time
  | Error CCS.(Build_id_mismatch Server_exited) ->
    let msg = "The flow server's version didn't match the client's, so it exited." in
    if env.autostart then (
      let start_time = Unix.gettimeofday () in
      if not env.quiet then Utils_js.prerr_endlinef "%s\nGoing to launch a new one.\n%!" msg;

      (* Don't decrement retries -- the server is definitely not running,
       * so the next time round will hit Server_missing above, *but*
       * before that will actually start the server -- we need to make
       * sure that happens.
       *)
      connect ~client_handshake env retries start_time
    ) else
      let msg = "\n" ^ msg in
      Exit.(exit ~msg Build_id_mismatch)
  | Error CCS.(Build_id_mismatch (Client_should_error { server_bin; server_version })) ->
    if env.rerun_on_mismatch then (
      if not env.quiet then (
        Printf.eprintf
          "Version mismatch! Server binary is Flow v%s but we are using v%s\n%!"
          server_version
          Flow_version.version;
        Printf.eprintf "Restarting command using the same binary as the server\n%!"
      );
      let argv = Array.copy Sys.argv in
      argv.(0) <- server_bin;
      Unix.execv server_bin argv
    ) else
      let msg =
        Printf.sprintf
          "\nThe Flow server's version (v%s) didn't match the client's (v%s). Exiting"
          server_version
          Flow_version.version
      in
      Exit.(exit ~msg Build_id_mismatch)
  | Error CCS.Server_socket_missing ->
    begin
      try
        if not env.quiet then
          Utils_js.prerr_endlinef "Attempting to kill server for `%s`" (Path.to_string env.root);
        CommandMeanKill.mean_kill ~flowconfig_name ~tmp_dir:env.tmp_dir env.root;
        if not env.quiet then
          Utils_js.prerr_endlinef "Successfully killed server for `%s`" (Path.to_string env.root);
        let start_time = Unix.gettimeofday () in
        handle_missing_server ~flowconfig_name ~client_handshake env retries start_time
      with
      | CommandMeanKill.FailedToKill err ->
        begin
          if not env.quiet then
            match err with
            | Some err -> prerr_endline err
            | None -> ()
        end;
        let msg = Utils_js.spf "Failed to kill server for `%s`" (Path.to_string env.root) in
        Exit.(exit ~msg Kill_error)
    end

and handle_missing_server ~flowconfig_name ~client_handshake env retries start_time =
  if env.autostart then (
    if not env.quiet then
      Utils_js.prerr_endlinef "Launching Flow server for %s" (Path.to_string env.root);
    let retries =
      match start_flow_server env with
      | Ok () ->
        if not env.quiet then Printf.eprintf "Started a new flow server: %s%!" (Tty.spinner ());
        retries
      | Error (_, Exit.Lock_stolen) ->
        if not env.quiet then
          Printf.eprintf
            "Failed to start a new flow server (%d %s remaining): %s%!"
            retries.retries_remaining
            (if retries.retries_remaining = 1 then
              "retry"
            else
              "retries")
            (Tty.spinner ());
        consume_retry retries
      | Error (msg, code) -> Exit.exit ~msg code
    in
    connect ~flowconfig_name ~client_handshake env retries start_time
  ) else
    let msg =
      Utils_js.spf "\nError: There is no Flow server running in '%s'." (Path.to_string env.root)
    in
    Exit.(exit ~msg No_server_running)

let connect ~flowconfig_name ~client_handshake env =
  let start_time = Unix.gettimeofday () in
  let retries =
    {
      retries_remaining = env.retries;
      original_retries = env.retries;
      last_connect_time = Unix.gettimeofday ();
    }
  in
  let res = connect ~flowconfig_name ~client_handshake env retries start_time in
  res
