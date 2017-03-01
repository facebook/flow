(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 **)

open Core
open Utils

(*
 * Module for us to interface with Watchman, a file watching service.
 * https://facebook.github.io/watchman/
 *
 * TODO:
 *   * Connect directly to the Watchman server socket instead of spawning
 *     a client process each time
 *   * Use the BSER protocol for enhanced performance
 *)

exception Watchman_error of string

(** Throw this exception when we know there is something to read from
 * the watchman channel, but reading took too long. *)
exception Read_payload_too_long

let debug = false

(** This number is totally arbitrary. Just need some cap. *)
let max_reinit_attempts = 8

let subscription_name = "hh_type_check_watcher"

(**
 * Triggers watchman to flush buffered updates on this subscription.
 * See watchman/docs/cmd/flush-subscriptions.html
 *)
let flush_subscriptions_cmd = "cmd-flush-subscriptions"

let crash_marker_path root =
  let root_name = Path.slash_escaped_string_of_path root in
  Filename.concat GlobalConfig.tmp_dir (spf ".%s.watchman_failed" root_name)

include Watchman_sig.Types

type dead_env = {
  (** Will reuse original settings to reinitializing watchman subscription. *)
  prior_settings : init_settings;
  reinit_attempts: int;
  dead_since: float;
  prior_clockspec : string;
}

type env = {
  settings : init_settings;
  socket: Buffered_line_reader.t * out_channel;
  watch_root: string;
  relative_path: string;
  (* See https://facebook.github.io/watchman/docs/clockspec.html
   *
   * This is also used to reliably detect a crashed watchman. Watchman has a
   * facility to detect watchman process crashes between two "since" queries
   *
   * See also assert_no_fresh_instance *)
  mutable clockspec: string;
}

let dead_env_from_alive env =
  {
    prior_settings = env.settings;
    dead_since = Unix.time ();
    reinit_attempts = 0;
    (** When we start a new watchman connection, we continue to use the prior
     * clockspec. If the same watchman server is still alive, then all is good.
     * If not, the clockspec allows us to detect whether a new watchman
     * server had to be started. See also "is_fresh_instance" on watchman's
     * "since" response. *)
    prior_clockspec = env.clockspec;
  }

type watchman_instance =
  (** Indicates a dead watchman instance (most likely due to chef upgrading,
   * reconfiguration, or a user terminating watchman, or a timeout reading
   * from the connection) detected by, for example, a pipe error or a timeout.
   *
   * TODO: Currently fallback to a Watchman_dead is only handled in calls
   * wrapped by the with_crash_record. Pipe errors elsewhere (for example
   * during exec) will still result in Hack exiting. Need to cover those
   * cases too. *)
  | Watchman_dead of dead_env
  | Watchman_alive of env

(* Some JSON processing helpers *)
module J = struct
  let try_get_val key json =
    let obj = Hh_json.get_object_exn json in
    List.Assoc.find obj key

  let get_string_val key ?default json =
    let v = try_get_val key json in
    match v, default with
    | Some v, _ -> Hh_json.get_string_exn v
    | None, Some def -> def
    | None, None -> raise Not_found

  let get_array_val key ?default json =
    let v = try_get_val key json in
    match v, default with
    | Some v, _ -> Hh_json.get_array_exn v
    | None, Some def -> def
    | None, None -> raise Not_found

  let strlist args =
    Hh_json.JSON_Array begin
      List.map args (fun arg -> Hh_json.JSON_String arg)
    end

  (* Prepend a string to a JSON array of strings. pred stands for predicate,
   * because that's how they are typically represented in watchman. See e.g.
   * https://facebook.github.io/watchman/docs/expr/allof.html *)
  let pred name args =
    let open Hh_json in
    JSON_Array (JSON_String name :: args)
end

(*****************************************************************************)
(* JSON methods. *)
(*****************************************************************************)

let clock root = J.strlist ["clock"; root]

type watch_command = Subscribe | Query

let request_json
    ?(extra_kv=[]) ?(extra_expressions=[]) watchman_command env =
  let open Hh_json in
  let command = begin match watchman_command with
    | Subscribe -> "subscribe"
    | Query -> "query" end in
  let header =
    [JSON_String command ; JSON_String env.watch_root] @
      begin
        match watchman_command with
        | Subscribe -> [JSON_String subscription_name]
        | _ -> []
      end in
  let directives = [
    JSON_Object (extra_kv @ [
      "fields", J.strlist ["name"];
      "relative_root", JSON_String env.relative_path;
      "expression", J.pred "allof" @@ (extra_expressions @ [
        J.strlist ["type"; "f"];
        J.pred "anyof" @@ [
          J.strlist ["name"; ".hhconfig"];
          J.pred "anyof" @@ [
            J.strlist ["suffix"; "php"];
            J.strlist ["suffix"; "phpt"];
            J.strlist ["suffix"; "hh"];
            J.strlist ["suffix"; "hhi"];
            J.strlist ["suffix"; "xhp"];
            (* FIXME: This is clearly wrong, but we do it to match the
             * behavior on the server-side. We need to investigate if
             * tracking js files is truly necessary.
             *)
            J.strlist ["suffix"; "js"];
          ];
        ];
        J.pred "not" @@ [
          J.pred "anyof" @@ [
            (** We don't exclude the .hg directory, because we touch unique
             * files there to support synchronous queries. *)
            J.strlist ["dirname"; ".git"];
            J.strlist ["dirname"; ".svn"];
          ]
        ]
      ])
    ])
  ] in
  let request = JSON_Array (header @ directives) in
  request

let all_query env =
  request_json
    ~extra_expressions:([Hh_json.JSON_String "exists"])
    Query env

let since_query env =
  request_json
    ~extra_kv: ["since", Hh_json.JSON_String env.clockspec;
                "empty_on_fresh_instance", Hh_json.JSON_Bool true]
    Query env

let subscribe mode env =
  let mode = match mode with
  | Defer_changes -> "defer"
  | Drop_changes -> "drop" in
  request_json
    ~extra_kv:["since", Hh_json.JSON_String env.clockspec ;
               mode, J.strlist ["hg.update"] ;
               "empty_on_fresh_instance",
               Hh_json.JSON_Bool true]
    Subscribe env

let watch_project root = J.strlist ["watch-project"; root]

(* See https://facebook.github.io/watchman/docs/cmd/version.html *)
let capability_check ?(optional=[]) required =
  let open Hh_json in
  JSON_Array begin
    [JSON_String "version"] @ [
      JSON_Object [
        "optional", J.strlist optional;
        "required", J.strlist required;
      ]
    ]
  end

(*****************************************************************************)
(* Handling requests and responses. *)
(*****************************************************************************)

let has_input timeout reader =
  if Buffered_line_reader.has_buffered_content reader
  then true
  else
    match Unix.select [Buffered_line_reader.get_fd reader] [] [] timeout with
    | [], _, _ -> false
    | _ -> true

let read_with_timeout timeout reader =
  let start_t = Unix.time () in
  if not (has_input timeout reader)
  then
    raise Timeout
  else
    let remaining = start_t +. timeout -. Unix.time () in
    let timeout = int_of_float remaining in
    let timeout = max timeout 10 in
    try Timeout.with_timeout
      ~do_: (fun _ -> Buffered_line_reader.get_next_line reader)
      ~timeout
      ~on_timeout:(fun _ -> ())
    with
    | Timeout.Timeout ->
      let () = EventLogger.watchman_timeout () in
      raise Read_payload_too_long

(** We filter all responses from get_changes through this. This is to detect
 * Watchman server crashes.
 *
 * See also Watchman docs on "since" query parameter. *)
let assert_no_fresh_instance obj =
  let open Hh_json.Access in
  let _ = (return obj)
    >>= get_bool "is_fresh_instance"
    >>= (fun (is_fresh, trace) ->
     if is_fresh then begin
       Hh_logger.log "Watchman server is fresh instance. Exiting.";
       Exit_status.(exit Watchman_fresh_instance)
     end
     else
       Result.Ok ((), trace)
    ) in
  ()

let assert_no_error obj =
  (try
     let warning = J.get_string_val "warning" obj in
     EventLogger.watchman_warning warning;
     Hh_logger.log "Watchman warning: %s\n" warning
   with Not_found -> ());
  (try
     let error = J.get_string_val "error" obj in
     EventLogger.watchman_error error;
     raise @@ Watchman_error error
   with Not_found -> ())

let sanitize_watchman_response output =
  if debug then Printf.eprintf "Watchman response: %s\n%!" output;
  let response =
    try Hh_json.json_of_string output
    with e ->
      Printf.eprintf "Failed to parse string as JSON: %s\n%!" output;
      raise e
  in
  assert_no_error response;
  response

let send_request oc json =
  let json_str = Hh_json.(json_to_string json) in
  if debug then Printf.eprintf "Watchman request: %s\n%!" json_str ;
  output_string oc json_str;
  output_string oc "\n";
  flush oc

let exec ?(timeout=120.0) (reader, oc) json =
  send_request oc json;
  sanitize_watchman_response (read_with_timeout timeout reader)

(*****************************************************************************)
(* Initialization, reinitialization, and crash-tracking. *)
(*****************************************************************************)

exception Watchman_sync_directory_error

let get_sockname timeout =
  let ic =
    Timeout.open_process_in "watchman"
    [| "watchman"; "get-sockname"; "--no-pretty" |] in
  let reader = Buffered_line_reader.create @@ Timeout.descr_of_in_channel ic in
  let output = read_with_timeout (float_of_int timeout) reader in
  assert (Timeout.close_process_in ic = Unix.WEXITED 0);
  let json = Hh_json.json_of_string output in
  J.get_string_val "sockname" json

let assert_sync_dir_exists path =
  let stats = try Unix.stat path with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
      Hh_logger.log "Watchman sync directory doesn't exist: %s" path;
      raise Watchman_sync_directory_error
  in
  let () = if stats.Unix.st_kind <> Unix.S_DIR then begin
    Hh_logger.log "Watchman sync directory is not a directory: %s" path;
    raise Watchman_sync_directory_error
  end
  else () in
  try Unix.access path [Unix.R_OK; Unix.W_OK] with
  | Unix.Unix_error (Unix.EACCES, _, _) ->
    Hh_logger.log "Dont have read-write access to watchman sync directory: %s" path;
    raise Watchman_sync_directory_error

let with_crash_record_exn root source f =
  try f ()
  with e ->
    close_out @@ open_out @@ crash_marker_path root;
    Hh_logger.exc ~prefix:("Watchman " ^ source ^ ": ") e;
    raise e

let with_crash_record_opt root source f =
  Option.try_with (fun () -> with_crash_record_exn root source f)

let has_capability name capabilities =
  (** Projects down from the boolean error monad into booleans.
   * Error states go to false, values are projected directly. *)
  let project_bool m = match m with
    | Result.Ok (v, _) ->
      v
    | Result.Error _ ->
      false
  in
  let open Hh_json.Access in
  (return capabilities)
    >>= get_obj "capabilities"
    >>= get_bool name
    |> project_bool

let re_init ?prior_clockspec
  { init_timeout; subscribe_mode; sync_directory; root } =
  with_crash_record_opt root "init" @@ fun () ->
  let root_s = Path.to_string root in
  let sockname = get_sockname init_timeout in
  assert_sync_dir_exists (Filename.concat root_s sync_directory);
  let (tic, oc) = Timeout.open_connection (Unix.ADDR_UNIX sockname) in
  let reader = Buffered_line_reader.create
    @@ Timeout.descr_of_in_channel @@ tic in
  let capabilities = exec (reader, oc)
    (capability_check ~optional:[ flush_subscriptions_cmd ]
    ["relative_root"]) in
  assert_no_error capabilities;
  let supports_flush = has_capability flush_subscriptions_cmd capabilities in
  (** Disable subscribe if Watchman flush feature isn't supported. *)
  let subscribe_mode = if supports_flush then subscribe_mode else None in
  let response = exec (reader, oc) (watch_project root_s) in
  let watch_root = J.get_string_val "watch" response in
  let relative_path = J.get_string_val "relative_path" ~default:"" response in

  let clockspec = match prior_clockspec with
    | Some s -> s
    | None -> exec (reader, oc) (clock watch_root) |> J.get_string_val "clock"
  in
  let env = {
    settings = {
      init_timeout;
      subscribe_mode;
      sync_directory;
      root;
    };
    socket = (reader, oc);
    watch_root;
    relative_path;
    clockspec;
  } in
  match subscribe_mode with
  | None -> env
  | Some subscribe_mode ->
    (ignore @@ exec env.socket (subscribe subscribe_mode env));
    env

let init settings = re_init settings

let no_updates_response clockspec =
  let timeout_str = "{\"files\":[]," ^ "\"clock\":\"" ^ clockspec ^ "\"}" in
  Hh_json.json_of_string timeout_str

let poll_for_updates ?timeout env =
  let timeout = Option.value timeout ~default:0.0 in
  let ready = has_input timeout @@ fst env.socket in
  if not ready then
    if timeout = 0.0 then no_updates_response env.clockspec
    else raise Timeout
  else
    (* Use the timeout mechanism to limit maximum time to read payload (cap
     * data size) so we don't freeze if watchman sends an inordinate amount of
     * data, or if it is malformed (i.e. doesn't end in a newline). *)
    let timeout = 40 in
    let output = try Timeout.with_timeout
      ~do_: (fun _ -> Buffered_line_reader.get_next_line @@ fst env.socket)
      ~timeout
      ~on_timeout:begin fun _ -> () end
    with
      | Timeout.Timeout ->
        let () = Hh_logger.log "Watchman.poll_for_updates timed out" in
        raise Read_payload_too_long
    in
    sanitize_watchman_response output

let extract_file_names env json =
  let files = try J.get_array_val "files" json with
    (** When an hg.update happens, it shows up in the watchman subscription
     * as a notification with no files key present. *)
    | Not_found -> []
  in
  let files = List.map files begin fun json ->
    let s = Hh_json.get_string_exn json in
    let abs =
      Filename.concat env.watch_root @@
      Filename.concat env.relative_path s in
    abs
  end in
  files

let within_backoff_time attempts time =
  let offset = 4.0 *. (2.0 ** float (if attempts > 3 then 3 else attempts)) in
  (Unix.time ()) >= time +. offset

let maybe_restart_instance instance = match instance with
  | Watchman_alive _ -> instance
  | Watchman_dead dead_env ->
    if dead_env.reinit_attempts >= max_reinit_attempts then
      let () = Hh_logger.log "Ran out of watchman reinit attempts. Exiting." in
      Exit_status.(exit Watchman_failed)
    else if within_backoff_time dead_env.reinit_attempts dead_env.dead_since then
      let () =
        Hh_logger.log "Attemping to reestablish watchman subscription" in
      match re_init ~prior_clockspec:dead_env.prior_clockspec
        dead_env.prior_settings with
      | None ->
        Hh_logger.log "Reestablishing watchman subscription failed.";
        EventLogger.watchman_connection_reestablishment_failed ();
        Watchman_dead { dead_env with
          reinit_attempts = dead_env.reinit_attempts + 1 }
      | Some env ->
        Hh_logger.log "Watchman connection reestablished.";
        EventLogger.watchman_connection_reestablished ();
        Watchman_alive env
    else
      instance

let close_channel_on_instance env =
  let reader, _ = env.socket in
  Unix.close @@ Buffered_line_reader.get_fd reader;
  EventLogger.watchman_died_caught ();
  Watchman_dead (dead_env_from_alive env), Watchman_unavailable

(** Calls f on the instance, maybe restarting it if its dead and maybe
 * reverting it to a dead state if things go south. For example, if watchman
 * shuts the connection on us, or shuts down, or crashes, we revert to a dead
 * instance, upon which a restart will be attempted down the road.
 * Alternatively, we also proactively revert to a dead instance if it appears
 * to be unresponsive (Timeout), and if reading the payload from it is
 * taking too long. *)
let call_on_instance instance source f =
  let instance = maybe_restart_instance instance in
  match instance with
  | Watchman_dead _ ->
    instance, Watchman_unavailable
  | Watchman_alive env -> begin
    try
      let env, result = with_crash_record_exn
        env.settings.root source (fun () -> f env) in
      Watchman_alive env, result
    with
      | Sys_error("Broken pipe") ->
        Hh_logger.log "Watchman Pipe broken.";
        close_channel_on_instance env
      | Sys_error("Connection reset by peer") ->
        Hh_logger.log "Watchman connection reset by peer.";
        close_channel_on_instance env
      | Sys_error("Bad file descriptor") ->
        (** This happens when watchman is tearing itself down after we
         * retrieved a sock address and connected to the sock address. That's
         * because Unix.open_connection (via Timeout.open_connection) doesn't
         * error even when the sock adddress is no longer valid and actually -
         * it returns a channel that will error at some later time when you
         * actually try to do anything with it (write to it, or even get the
         * file descriptor of it). I'm pretty sure we don't need to close the
         * channel when that happens since we never had a useable channel
         * to start with. *)
        Hh_logger.log "Watchman bad file descriptor.";
        EventLogger.watchman_died_caught ();
        Watchman_dead (dead_env_from_alive env), Watchman_unavailable
      | End_of_file ->
        Hh_logger.log "Watchman connection End_of_file. Closing channel";
        close_channel_on_instance env
      | Read_payload_too_long ->
        Hh_logger.log "Watchman reading payload too long. Closing channel";
        close_channel_on_instance env
      | Timeout ->
        Hh_logger.log "Watchman reading Timeout. Closing channel";
        close_channel_on_instance env
      | Watchman_error msg ->
        Hh_logger.log "Watchman error: %s. Closing channel" msg;
        close_channel_on_instance env
      | e ->
        let msg = Printexc.to_string e in
        EventLogger.watchman_uncaught_failure msg;
        Exit_status.(exit Watchman_failed)
  end

(** This is a large >50MB payload, which could longer than 2 minutes for
 * Watchman to generate and push down the channel. *)
let get_all_files env =
  try with_crash_record_exn env.settings.root "get_all_files"  @@ fun () ->
    let response = exec env.socket (all_query env) in
    env.clockspec <- J.get_string_val "clock" response;
    extract_file_names env response with
    | _ ->
      Exit_status.(exit Watchman_failed)

let transform_synchronous_get_changes_response env data =
  env.clockspec <- J.get_string_val "clock" data;
  assert_no_fresh_instance data;
  env, set_of_list @@ extract_file_names env data

let make_state_change_response state name data =
  let metadata = J.try_get_val "metadata" data in
  match state with
  | `Enter ->
    State_enter (name, metadata)
  | `Leave ->
    State_leave (name, metadata)

let transform_asynchronous_get_changes_response env data =
  env.clockspec <- J.get_string_val "clock" data;
  assert_no_fresh_instance data;
  try env, make_state_change_response `Enter
    (J.get_string_val "state-enter" data) data with
  | Not_found ->
  try env, make_state_change_response `Leave
    (J.get_string_val "state-leave" data) data with
  | Not_found ->
    env, Files_changed (set_of_list @@ extract_file_names env data)

let get_changes ?deadline instance =
  let timeout = Option.map deadline ~f:(fun deadline ->
    let timeout = deadline -. (Unix.time ()) in
    max timeout 0.0
  ) in
  call_on_instance instance "get_changes" @@ fun env ->
    if env.settings.subscribe_mode <> None
    then
      let env, result = transform_asynchronous_get_changes_response
        env (poll_for_updates ?timeout env) in
      env, Watchman_pushed result
    else
      let env, result = transform_synchronous_get_changes_response
        env (exec ?timeout env.socket (since_query env)) in
      env, Watchman_synchronous result

let flush_request ~(timeout:int) watch_root =
  let open Hh_json in
  let directive = JSON_Object [
    (** Watchman expects timeout milliseconds. *)
    ("sync_timeout", (JSON_Number (string_of_int @@ timeout * 1000))) ] in
  JSON_Array [
    JSON_String "flush-subscriptions";
    JSON_String watch_root;
    directive;
  ]

let rec poll_until_sync ~deadline env acc =
  let is_finished_flush_response json =
    let open Hh_json.Access in
    let synced = (return json) >>= get_array "synced" |> begin function
      | Result.Error _ -> false
      | Result.Ok (vs, _) ->
        List.fold_left vs ~init:false ~f:(fun acc v ->
          acc || ((Hh_json.get_string_exn v) = subscription_name)) end
    in
    let not_needed = (return json) >>= get_array "no_sync_needed"
      |> begin function
        | Result.Error _ -> false
        | Result.Ok (vs, _) ->
          List.fold_left vs ~init:false ~f:(fun acc v ->
            acc || ((Hh_json.get_string_exn v) = subscription_name)) end
    in
    synced || not_needed
  in
  let timeout = deadline -. Unix.time () in
  if timeout < 0.0 then raise Timeout else ();
  let json = poll_for_updates ~timeout env in
  if is_finished_flush_response json then (env, acc) else
    let env, result = transform_synchronous_get_changes_response env json in
    poll_until_sync ~deadline env (SSet.union acc result)

let poll_until_sync ~deadline env =
  poll_until_sync ~deadline env SSet.empty

let get_changes_synchronously ~(timeout:int) instance =
  let instance, result = call_on_instance instance "get_changes_synchronously"
    @@ (fun env ->
      if env.settings.subscribe_mode = None
      then
        let env, files = transform_synchronous_get_changes_response
          env (exec ~timeout:(float_of_int timeout)
            env.socket (since_query env))
        in
        env, Watchman_synchronous files
      else
        let request = flush_request ~timeout env.watch_root in
        let _, oc = env.socket in
        let () = send_request oc request in
        let deadline = Unix.time () +. (float_of_int timeout) in
        let env, files = poll_until_sync ~deadline env in
        env, Watchman_synchronous files
    )
  in
  match result with
  | Watchman_unavailable ->
    raise (Watchman_error "Watchman unavailable for synchronous response")
  | Watchman_pushed _ ->
    raise (Watchman_error "Wtf? pushed response from synchronous request")
  | Watchman_synchronous files ->
    instance, files

module type Testing_sig = sig
  val test_env : env
  val transform_asynchronous_get_changes_response :
    env -> Hh_json.json -> env * pushed_changes
end

module Testing = struct
  let test_settings = {
    subscribe_mode = Some Defer_changes;
    init_timeout = 0;
    sync_directory = "";
    root = Path.dummy_path;
  }

  let test_env = {
    settings = test_settings;
    socket = (Buffered_line_reader.get_null_reader (), open_out "/dev/null");
    watch_root = "";
    relative_path = "";
    clockspec = "";
  }

  let transform_asynchronous_get_changes_response env json =
    transform_asynchronous_get_changes_response env json
end
