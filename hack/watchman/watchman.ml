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
exception Timeout

(** Throw this exception when we know there is something to read from
 * the watchman channel, but reading took too long. *)
exception Read_payload_too_long

let debug = false

(** This number is totally arbitrary. Just need some cap. *)
let max_reinit_attempts = 8

let sync_file_extension = "tmp_sync"

let crash_marker_path root =
  let root_name = Path.slash_escaped_string_of_path root in
  Filename.concat GlobalConfig.tmp_dir (spf ".%s.watchman_failed" root_name)

type init_settings = {
  subscribe_to_changes: bool;
  (** Seconds used for init timeout - will be reused for reinitialization. *)
  init_timeout: int;
  sync_directory: string;
  root: Path.t;
}

type dead_env = {
  (** Will reuse original settings to reinitializing watchman subscription. *)
  prior_settings : init_settings;
  reinit_attempts: int;
  dead_since: float;
  prior_clockspec : string;
}

type env = {
  settings : init_settings;
  socket: Timeout.in_channel * out_channel;
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

type pushed_changes =
  (** State name and metadata. *)
  | State_enter of string * Hh_json.json option
  | State_leave of string * Hh_json.json option
  | Files_changed of SSet.t

type changes =
  | Watchman_unavailable
  | Watchman_pushed of pushed_changes
  | Watchman_synchronous of SSet.t

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

let get_root_path instance = match instance with
  | Watchman_dead dead_env -> dead_env.prior_settings.root
  | Watchman_alive env -> env.settings.root

let get_sync_dir instance = match instance with
  | Watchman_dead dead_env -> dead_env.prior_settings.sync_directory
  | Watchman_alive env -> env.settings.sync_directory

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
        | Subscribe -> [JSON_String "hh_type_check_watcher"]
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
            J.strlist ["suffix"; sync_file_extension];
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

let subscribe env = request_json
                      ~extra_kv:["since", Hh_json.JSON_String env.clockspec ;
                                 "defer", J.strlist ["hg.update"] ;
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

let read_with_timeout timeout ic =
  let fd = Timeout.descr_of_in_channel ic in
  let deadline = Unix.gettimeofday () +. timeout in
  match Unix.select [fd] [] [] timeout with
    | [ready_fd], _, _ when ready_fd = fd -> begin
     Timeout.with_timeout
       ~timeout:(max 1 (int_of_float (deadline -. Unix.gettimeofday ())))
       ~do_:(fun t -> Timeout.input_line ~timeout:t ic)
       ~on_timeout:begin fun _ ->
                     EventLogger.watchman_timeout ();
                     raise Read_payload_too_long
                   end
    end
    | _, _, _ ->
      EventLogger.watchman_timeout ();
      raise Timeout

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

let exec ?(timeout=120.0) (ic, oc) json =
  let json_str = Hh_json.(json_to_string json) in
  if debug then Printf.eprintf "Watchman request: %s\n%!" json_str ;
  output_string oc json_str;
  output_string oc "\n";
  flush oc ;
  sanitize_watchman_response (read_with_timeout timeout ic)

(*****************************************************************************)
(* Initialization, reinitialization, and crash-tracking. *)
(*****************************************************************************)

exception Watchman_sync_directory_error

let get_sockname timeout =
  let ic =
    Timeout.open_process_in "watchman"
    [| "watchman"; "get-sockname"; "--no-pretty" |] in
  let output = read_with_timeout (float_of_int timeout) ic in
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

let re_init ?prior_clockspec
  { init_timeout; subscribe_to_changes; sync_directory; root } =
  with_crash_record_opt root "init" @@ fun () ->
  let root_s = Path.to_string root in
  let sockname = get_sockname init_timeout in
  assert_sync_dir_exists (Filename.concat root_s sync_directory);
  let socket = Timeout.open_connection (Unix.ADDR_UNIX sockname) in
  ignore @@ exec socket (capability_check ["relative_root"]);
  let response = exec socket (watch_project root_s) in
  let watch_root = J.get_string_val "watch" response in
  let relative_path = J.get_string_val "relative_path" ~default:"" response in

  let clockspec = match prior_clockspec with
    | Some s -> s
    | None -> exec socket (clock watch_root) |> J.get_string_val "clock"
  in
  let env = {
    settings = {
      init_timeout;
      subscribe_to_changes;
      sync_directory;
      root;
    };
    socket;
    watch_root;
    relative_path;
    clockspec;
  } in
  if subscribe_to_changes then (ignore @@ exec env.socket (subscribe env)) ;
  env

let init settings = re_init settings

let has_input timeout (in_channel, _) =
  let fd = Timeout.descr_of_in_channel in_channel in
  match Unix.select [fd] [] [] timeout with
    | [_], _, _ -> true
    | _ -> false

let no_updates_response clockspec =
  let timeout_str = "{\"files\":[]," ^ "\"clock\":\"" ^ clockspec ^ "\"}" in
  Hh_json.json_of_string timeout_str

let poll_for_updates ?timeout env =
  let timeout = Option.value timeout ~default:0.0 in
  let ready = has_input timeout env.socket in
  if not ready then
    if timeout = 0.0 then no_updates_response env.clockspec
    else raise Timeout
  else
  let timeout = 20 in
  try
    let output = begin
      let in_channel, _  = env.socket in
      (* Use the timeout mechanism to limit maximum time to read payload (cap
       * data size). *)
      Timeout.with_timeout
        ~do_: (fun t -> Timeout.input_line ~timeout:t in_channel)
        ~timeout
        ~on_timeout:begin fun _ -> () end
    end in
    sanitize_watchman_response output
  with
  | Timeout.Timeout ->
    let () = Hh_logger.log "Watchman.poll_for_updates timed out" in
    raise Read_payload_too_long
  | _ as e ->
    raise e

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
  let ic, _ = env.socket in
  Timeout.close_in ic;
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

let random_filepath root sync_dir =
  let root_name = Path.to_string root in
  let dir = Filename.concat root_name sync_dir in
  let name = Random_id.(short_string_with_alphabet alphanumeric_alphabet) in
  Filename.concat dir (spf ".%s.%s" name sync_file_extension)

let get_changes ?deadline instance =
  let timeout = Option.map deadline ~f:(fun deadline ->
    let timeout = deadline -. (Unix.time ()) in
    max timeout 0.0
  ) in
  call_on_instance instance "get_changes" @@ fun env ->
    if env.settings.subscribe_to_changes
    then
      let env, result = transform_asynchronous_get_changes_response
        env (poll_for_updates ?timeout env) in
      env, Watchman_pushed result
    else
      let env, result = transform_synchronous_get_changes_response
        env (exec ?timeout env.socket (since_query env)) in
      env, Watchman_synchronous result

let rec get_changes_until_file_sync deadline syncfile instance acc_changes =
  if Unix.time () >= deadline then raise Timeout else ();
  let instance, changes = get_changes ~deadline instance in
  match changes with
  | Watchman_unavailable ->
    (** We don't need to use Retry_with_backoff_exception because there is
     * exponential backoff built into get_changes to restart the watchman
     * instance. *)
    get_changes_until_file_sync
      deadline syncfile instance acc_changes (** Not in 4.01 yet [@tailcall] *)
  | Watchman_synchronous changes
  | Watchman_pushed (Files_changed changes) ->
    let acc_changes = SSet.union acc_changes changes in
    if SSet.mem syncfile changes then
      instance, acc_changes
    else
      get_changes_until_file_sync deadline syncfile instance acc_changes
  | Watchman_pushed (State_enter _)
  | Watchman_pushed (State_leave _) ->
    (** TODO: Add these enter and exit events to the synchronous response. *)
    get_changes_until_file_sync deadline syncfile instance acc_changes

(** Raise this exception together with a with_retries_until_deadline call to
 * make use of its exponential backoff machinery. *)
exception Retry_with_backoff_exception

(** Call "f instance temp_file_name" with a random temporary file created
 * before f and deleted after f. *)
let with_random_temp_file instance f =
  let root = get_root_path instance in
  let temp_file = random_filepath root (get_sync_dir instance) in
  let fd = try Some (
    Unix.openfile temp_file [Unix.O_CREAT; Unix.O_EXCL] 555)
    with
    | e ->
      let () = Hh_logger.log "Creating watchman sync file failed: %s"
        (Printexc.to_string e) in
      None
  in
  match fd with
  | None ->
    (** Failed to create temp file. Retry with exponential backoff. *)
    raise Retry_with_backoff_exception
  | Some fd ->
    let () = Unix.close fd in
    let result = f instance temp_file in
    let () = Sys.remove temp_file in
    result

(** Call f with retries if it throws Retry_with_backoff_exception,
 * using exponential backoff between attempts.
 *
 * Raise Timeout if deadline arrives. *)
let rec with_retries_until_deadline ~attempt instance deadline f =
  if Unix.time () > deadline then raise Timeout else ();
  let max_wait_time = 10.0 in
  try f instance with
    | Retry_with_backoff_exception ->
      let () = if Unix.time () > deadline then raise Timeout else () in
      let wait_time = min max_wait_time (2.0 ** (float_of_int attempt)) in
      let () = ignore @@ Unix.select [] [] [] wait_time in
      with_retries_until_deadline ~attempt:(attempt + 1) instance deadline f

let get_changes_synchronously ~(timeout:int) instance =
  (** Reading uses Timeout.with_timeout, which is not re-entrant. So
   * we can't use that out here. *)
  let deadline = Unix.time () +. (float_of_int timeout) in
  with_retries_until_deadline ~attempt:0 instance deadline begin
    (** Lambda here must take an instance to avoid capturing the one in the
     * outer scope, which is the wrong one since it doesn't change between
     * restart attempts. *)
    fun instance ->
      with_random_temp_file instance begin fun instance sync_file ->
        let result =
          get_changes_until_file_sync deadline sync_file instance SSet.empty in
        result
      end
  end

module type Testing_sig = sig
  val test_env : env
  val transform_asynchronous_get_changes_response :
    env -> Hh_json.json -> env * pushed_changes
end

module Testing = struct
  let test_settings = {
    subscribe_to_changes = true;
    init_timeout = 0;
    sync_directory = "";
    root = Path.dummy_path;
  }

  let test_env = {
    settings = test_settings;
    socket = (Timeout.open_in "/dev/null", open_out "/dev/null");
    watch_root = "";
    relative_path = "";
    clockspec = "";
  }

  let transform_asynchronous_get_changes_response env json =
    transform_asynchronous_get_changes_response env json
end
