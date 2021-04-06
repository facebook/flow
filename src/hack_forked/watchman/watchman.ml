(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open Utils
module Let_syntax = Lwt.Infix.Let_syntax

(*
 * Module for us to interface with Watchman, a file watching service.
 * https://facebook.github.io/watchman/
 *
 * TODO:
 *   * Connect directly to the Watchman server socket instead of spawning
 *     a client process each time
 *   * Use the BSER protocol for enhanced performance
 *)

exception Timeout

exception Watchman_error of string

(** Throw this exception when we know there is something to read from
    the watchman channel, but reading took too long. *)
exception Read_payload_too_long

exception Subscription_canceled_by_watchman

exception Watchman_restarted

type subscribe_mode =
  | All_changes
  | Defer_changes
  | Drop_changes
      (** See also Watchman docs on drop. This means the subscriber will not
          get a list of files changed during a repo update. Practically, this
          is not useful for the typechecker process which needs to actually
          know which files were changed. This is useful for the monitor to
          aggressively kill the server. *)

type timeout = float option

type init_settings = {
  debug_logging: bool;
  defer_states: string list;
  expression_terms: Hh_json.json list;  (** See watchman expression terms. *)
  mergebase_with: string;
  roots: Path.t list;  (** symbolic commit to find changes against *)
  subscribe_mode: subscribe_mode;
  subscription_prefix: string;
  sync_timeout: int option;
}

(** The message's clock. *)
type clock = string

type pushed_changes =
  (*
   * State name and metadata.
   *
   * For example:
   *   State name: "hg.update"
   * Metadata:
   *   {
   *    "partial":false,
   *    "rev":"780dab9ff0a01691c9b18a5ee1194810e555c78b",
   *    "distance":2,
   *    "status":"ok"
   *   }
   *
   * Note: The distance is HG Revision distance, not SVN revision distance.
   *)
  | State_enter of string * Hh_json.json option
  | State_leave of string * Hh_json.json option
  | Changed_merge_base of string * SSet.t * clock
  | Files_changed of SSet.t

type changes =
  | Watchman_unavailable
  | Watchman_pushed of pushed_changes

module Jget = Hh_json_helpers.Jget
module J = Hh_json_helpers.AdhocJsonHelpers

(* Looks for common errors in watchman responses *)
let assert_no_error obj =
  let obj = Some obj in
  (match Jget.string_opt obj "warning" with
  | None -> ()
  | Some warning ->
    EventLogger.watchman_warning warning;
    Hh_logger.log "Watchman warning: %s\n" warning);
  (match Jget.string_opt obj "error" with
  | None -> ()
  | Some error ->
    EventLogger.watchman_error error;
    raise @@ Watchman_error error);
  (match Jget.bool_opt obj "canceled" with
  | None
  | Some false ->
    ()
  | Some true ->
    EventLogger.watchman_error "Subscription canceled by watchman";
    raise @@ Subscription_canceled_by_watchman);
  ()

(* Verifies that a watchman response is valid JSON and free from common errors *)
let sanitize_watchman_response ~debug_logging output =
  if debug_logging then Hh_logger.info "Watchman response: %s" output;
  let response =
    try Hh_json.json_of_string output
    with e ->
      let exn = Exception.wrap e in
      Hh_logger.error
        "Failed to parse string as JSON: %s\nEXCEPTION:%s\nSTACK:%s\n"
        output
        (Exception.get_ctor_string exn)
        (Exception.get_backtrace_string exn);
      Exception.reraise exn
  in
  assert_no_error response;
  response

type conn = Buffered_line_reader_lwt.t * Lwt_io.output_channel

let catch ~f ~catch =
  Lwt.catch f (fun exn ->
      let e = Exception.wrap exn in
      match exn with
      | Lwt.Canceled -> Exception.reraise e
      | _ -> catch e)

(** This number is totally arbitrary. Just need some cap. *)
let max_reinit_attempts = 8

type dead_env = {
  (* Will reuse original settings to reinitializing watchman subscription. *)
  prior_settings: init_settings;
  reinit_attempts: int;
  dead_since: float;
  prior_clockspec: string;
}

type env = {
  (* See https://facebook.github.io/watchman/docs/clockspec.html
   *
   * This is also used to reliably detect a crashed watchman. Watchman has a
   * facility to detect watchman process crashes between two "since" queries. *)
  mutable clockspec: string;
  conn: conn;
  settings: init_settings;
  subscription: string;
  watch_root: string;
  watched_path_expression_terms: Hh_json.json option;
}

let dead_env_from_alive env =
  {
    prior_settings = env.settings;
    dead_since = Unix.time ();
    reinit_attempts = 0;
    (* When we start a new watchman connection, we continue to use the prior
     * clockspec. If the same watchman server is still alive, then all is
     * good. If not, the clockspec allows us to detect whether a new watchman
     * server had to be started. See also "is_fresh_instance" on watchman's
     * "since" response. *)
    prior_clockspec = env.clockspec;
  }

type watchman_instance =
  (* Indicates a dead watchman instance (most likely due to chef upgrading,
   * reconfiguration, or a user terminating watchman, or a timeout reading
   * from the connection) detected by, for example, a pipe error or a timeout.
   *
   * TODO: Currently fallback to a Watchman_dead is only handled in calls
   * wrapped by the with_crash_record. Pipe errors elsewhere (for example
   * during request) will still result in Hack exiting. Need to cover those
   * cases too. *)
  | Watchman_dead of dead_env
  | Watchman_alive of env

(****************************************************************************)
(* JSON methods. *)
(****************************************************************************)

(* Projects down from the boolean error monad into booleans.
  * Error states go to false, values are projected directly. *)
let project_bool m =
  match m with
  | Ok (v, _) -> v
  | Error _ -> false

let clock root = J.strlist ["clock"; root]

type watch_command =
  | Subscribe
  | Query

(** Conjunction of extra_expressions and expression_terms. *)
let request_json ?(extra_kv = []) ?(extra_expressions = []) watchman_command env =
  Hh_json.(
    let command =
      match watchman_command with
      | Subscribe -> "subscribe"
      | Query -> "query"
    in
    let header =
      [JSON_String command; JSON_String env.watch_root]
      @
      match watchman_command with
      | Subscribe -> [JSON_String env.subscription]
      | _ -> []
    in
    let expressions = extra_expressions @ env.settings.expression_terms in
    let expressions =
      match env.watched_path_expression_terms with
      | Some terms -> terms :: expressions
      | None -> expressions
    in
    assert (not (List.is_empty expressions));
    let extra_kv =
      match env.settings.sync_timeout with
      | Some sync_timeout -> ("sync_timeout", JSON_Number (Int.to_string sync_timeout)) :: extra_kv
      | None -> extra_kv
    in
    let directives =
      [
        JSON_Object
          ( extra_kv
          @ [
              ("fields", J.strlist ["name"]);
              (* Watchman doesn't allow an empty allof expression. But expressions is never empty *)
              ("expression", J.pred "allof" expressions);
            ] );
      ]
    in
    let request = JSON_Array (header @ directives) in
    request)

let get_changes_since_mergebase_query env =
  let mergebase_with = env.settings.mergebase_with in
  let extra_kv =
    [
      ( "since",
        Hh_json.JSON_Object
          [("scm", Hh_json.JSON_Object [("mergebase-with", Hh_json.JSON_String mergebase_with)])] );
    ]
  in
  request_json ~extra_kv Query env

let subscribe ~mode ~states env =
  let states = "hg.update" :: states in
  let (since, mode) =
    match mode with
    | All_changes -> (Hh_json.JSON_String env.clockspec, [])
    | Defer_changes -> (Hh_json.JSON_String env.clockspec, [("defer", J.strlist states)])
    | Drop_changes -> (Hh_json.JSON_String env.clockspec, [("drop", J.strlist states)])
  in
  request_json
    ~extra_kv:(([("since", since)] @ mode) @ [("empty_on_fresh_instance", Hh_json.JSON_Bool true)])
    Subscribe
    env

let watch_project root = J.strlist ["watch-project"; root]

(** We filter all responses from get_changes through this. This is to detect
   * Watchman server crashes.
   *
   * See also Watchman docs on "since" query parameter. *)
let is_fresh_instance obj =
  Hh_json.Access.(return obj >>= get_bool "is_fresh_instance" |> project_bool)

(****************************************************************************)
(* I/O stuff *)
(****************************************************************************)

let with_timeout timeout f =
  match timeout with
  | None -> f ()
  | Some timeout ->
    (try%lwt Lwt_unix.with_timeout timeout f with Lwt_unix.Timeout -> raise Timeout)

(* Send a request to the watchman process *)
let send_request ~debug_logging oc json =
  let json_str = Hh_json.(json_to_string json) in
  if debug_logging then Hh_logger.info "Watchman request: %s" json_str;

  (* Print the json with a newline and then flush *)
  let%lwt () = Lwt_io.fprintl oc json_str in
  Lwt_io.flush oc

let get_sockname () =
  let cmd = "watchman" in
  let args = ["--no-pretty"; "get-sockname"] in
  let%lwt { LwtSysUtils.status; stdout; stderr } = LwtSysUtils.exec cmd args in
  match status with
  | Unix.WEXITED 0 ->
    let json = Hh_json.json_of_string stdout in
    Lwt.return @@ J.get_string_val "sockname" json
  | Unix.WEXITED 127 ->
    let msg =
      spf
        "watchman not found on PATH: %s"
        (Option.value (Sys_utils.getenv_path ()) ~default:"(not set)")
    in
    let () = EventLogger.watchman_error msg in
    let () = Hh_logger.error "%s" msg in
    raise (Watchman_error "watchman not found on PATH")
  | Unix.WEXITED code ->
    let () = EventLogger.watchman_error (spf "watchman exited code %d, stderr = %S" code stderr) in
    raise (Watchman_error (spf "watchman exited code %d" code))
  | Unix.WSIGNALED signal ->
    let msg = spf "watchman signaled with %s signal" (PrintSignal.string_of_signal signal) in
    let () = EventLogger.watchman_error msg in
    raise (Watchman_error msg)
  | Unix.WSTOPPED signal ->
    let msg = spf "watchman stopped with %s signal" (PrintSignal.string_of_signal signal) in
    let () = EventLogger.watchman_error msg in
    raise (Watchman_error msg)

(** Opens a connection to the watchman process through the socket *)
let open_connection () =
  let%lwt sockname = get_sockname () in
  let (ic, oc) =
    try
      if Sys.unix then
        (* Yes, I know that Unix.open_connection uses the same fd for input and output. But I don't
         * want to hardcode that assumption here. So let's pretend like ic and oc might be back by
         * different fds *)
        Unix.open_connection (Unix.ADDR_UNIX sockname)
      else
        (* On Windows, however, named pipes behave like regular files from the client's perspective.
         * We just open the file and create in/out channels for it. The file permissions attribute
         * is not needed because the file should exist already but we have to pass something. *)
        let fd = Unix.openfile sockname [Unix.O_RDWR] 0o640 in
        (Unix.in_channel_of_descr fd, Unix.out_channel_of_descr fd)
    with Unix.Unix_error (error, _, _) ->
      let msg = spf "%s (socket: %s)" (Unix.error_message error) sockname in
      EventLogger.watchman_error msg;
      raise (Watchman_error msg)
  in
  let reader =
    Unix.descr_of_in_channel ic
    |> Lwt_unix.of_unix_file_descr ~blocking:true
    |> Buffered_line_reader_lwt.create
  in
  let oc =
    Unix.descr_of_out_channel oc
    |> Lwt_unix.of_unix_file_descr ~blocking:true
    |> Lwt_io.of_fd ~mode:Lwt_io.output
  in
  Lwt.return (reader, oc)

let close_connection (reader, oc) =
  let%lwt () = Lwt_unix.close @@ Buffered_line_reader_lwt.get_fd reader in
  (* As mention above, if we open the connection with Unix.open_connection, we use a single fd for
   * both input and output. That means we might be trying to close it twice here. If so, this
   * second close with throw. So let's catch that exception and ignore it. *)
  try%lwt Lwt_io.close oc with Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit

let with_watchman_conn ~timeout f =
  let%lwt conn = with_timeout timeout open_connection in
  let%lwt result =
    try%lwt f conn
    with e ->
      let e = Exception.wrap e in
      let%lwt () = close_connection conn in
      Exception.reraise e
  in
  let%lwt () = close_connection conn in
  Lwt.return result

(* Sends a request to watchman and returns the response. If we don't have a connection,
 * a new connection will be created before the request and destroyed after the response *)
let rec request ~debug_logging ?conn ~timeout json =
  match conn with
  | None -> with_watchman_conn ~timeout (fun conn -> request ~debug_logging ~conn ~timeout json)
  | Some (reader, oc) ->
    let%lwt () = send_request ~debug_logging oc json in
    let%lwt line =
      with_timeout timeout @@ fun () -> Buffered_line_reader_lwt.get_next_line reader
    in
    Lwt.return @@ sanitize_watchman_response ~debug_logging line

let has_input ~timeout reader =
  let fd = Buffered_line_reader_lwt.get_fd reader in
  match timeout with
  | None -> Lwt.return @@ Lwt_unix.readable fd
  | Some timeout ->
    (try%lwt
       Lwt_unix.with_timeout timeout @@ fun () ->
       let%lwt () = Lwt_unix.wait_read fd in
       Lwt.return true
     with Lwt_unix.Timeout -> Lwt.return false)

let blocking_read ~debug_logging ~timeout ~conn:(reader, _) =
  let%lwt ready = has_input ~timeout reader in
  if not ready then
    match timeout with
    | None -> Lwt.return None
    | _ -> raise Timeout
  else
    let%lwt output =
      try%lwt Lwt_unix.with_timeout 40.0 @@ fun () -> Buffered_line_reader_lwt.get_next_line reader
      with Lwt_unix.Timeout ->
        let () = Hh_logger.log "blocking_read timed out" in
        raise Read_payload_too_long
    in
    Lwt.return @@ Some (sanitize_watchman_response ~debug_logging output)

(****************************************************************************)
(* Initialization, reinitialization *)
(****************************************************************************)

(* When we re-init our connection to Watchman, we use the old clockspec to get all the changes
 * since our last response. However, if Watchman has restarted and the old clockspec pre-dates
 * the new Watchman, then we may miss updates.
 *
 * Unfortunately, the response to "subscribe" doesn't have the "is_fresh_instance" field. So
 * we'll instead send a small "query" request. It should always return 0 files, but it should
 * tell us whether the Watchman service has restarted since clockspec.
 *)
let has_watchman_restarted_since ~debug_logging ~conn ~timeout ~watch_root ~clockspec =
  let hard_to_match_name = "irrelevant.potato" in
  let query =
    Hh_json.(
      JSON_Array
        [
          JSON_String "query";
          JSON_String watch_root;
          JSON_Object
            [
              ("since", JSON_String clockspec);
              ("empty_on_fresh_instance", JSON_Bool true);
              ("expression", JSON_Array [JSON_String "name"; JSON_String hard_to_match_name]);
            ];
        ])
  in
  let%bind response = request ~debug_logging ~conn ~timeout query in
  let result =
    match Hh_json_helpers.Jget.bool_opt (Some response) "is_fresh_instance" with
    | Some has_restarted -> Ok has_restarted
    | None ->
      (* The response to this query **always** should include the `is_fresh_instance` boolean
       * property. If it is missing then something has gone wrong with Watchman. Since we can't
       * prove that Watchman has not restarted, we must treat this as an error. *)
      Error
        (spf
           "Invalid Watchman response to `empty_on_fresh_instance` query:\n%s"
           (Hh_json.json_to_string ~pretty:true response))
  in
  Lwt.return result

let prepend_relative_path_term ~relative_path ~terms =
  match terms with
  | None -> None
  | Some _ when String.is_empty relative_path ->
    (* If we're watching the watch root directory, then there's no point in specifying a list of
     * files and directories to watch. We're already subscribed to any change in this watch root
     * anyway *)
    None
  | Some terms ->
    (* So lets say we're being told to watch foo/bar. Is foo/bar a directory? Is it a file? If it
     * is a file now, might it become a directory later? I'm not aware of aterm which will watch for either a file or a directory, so let's add two terms *)
    Some (J.strlist ["dirname"; relative_path] :: J.strlist ["name"; relative_path] :: terms)

(** Gets the current clockspec. If we have a prior clockspec, we make sure it's still valid by
    checking whether watchman has restarted and may have missed updates; if we don't have a
    prior clockspec, grab the current clock. *)
let get_clockspec ~debug_logging ~conn ~watch_root prior_clockspec =
  match prior_clockspec with
  | Some clockspec ->
    let%bind has_restarted =
      has_watchman_restarted_since
        ~debug_logging
        ~conn
        ~timeout:None (* the whole init process should be wrapped in a timeout *)
        ~watch_root
        ~clockspec
    in
    (match has_restarted with
    | Ok true ->
      Hh_logger.error "Watchman server restarted so we may have missed some updates";
      raise Watchman_restarted
    | Ok false -> Lwt.return clockspec
    | Error err ->
      Hh_logger.error "%s" err;
      raise Exit.(Exit_with Watchman_failed))
  | None ->
    let%map response =
      request
        ~debug_logging
        ~conn
        ~timeout:None (* the whole init process should be wrapped in a timeout *)
        (clock watch_root)
    in
    J.get_string_val "clock" response

let re_init
    ?prior_clockspec
    {
      debug_logging;
      defer_states;
      expression_terms;
      mergebase_with;
      roots;
      subscribe_mode;
      subscription_prefix;
      sync_timeout;
    } =
  let%bind conn = open_connection () in
  let%bind (watched_path_expression_terms, watch_roots, failed_paths) =
    Lwt_list.fold_left_s
      (fun (terms, watch_roots, failed_paths) path ->
        (* Watch this root. If the path doesn't exist, watch_project will throw. In that case catch
         * the error and continue for now. *)
        let%map response =
          catch
            ~f:(fun () ->
              let%map response =
                request
                  ~debug_logging
                  ~conn
                  ~timeout:None (* the whole init process should be wrapped in a timeout *)
                  (watch_project (Path.to_string path))
              in
              Some response)
            ~catch:(fun _ -> Lwt.return None)
        in
        match response with
        | None -> (terms, watch_roots, SSet.add (Path.to_string path) failed_paths)
        | Some response ->
          let watch_root = J.get_string_val "watch" response in
          let relative_path = J.get_string_val "relative_path" ~default:"" response in
          let terms = prepend_relative_path_term ~relative_path ~terms in
          let watch_roots = SSet.add watch_root watch_roots in
          (terms, watch_roots, failed_paths))
      (Some [], SSet.empty, SSet.empty)
      roots
  in
  (* The failed_paths are likely includes which don't exist on the filesystem, so watch_project
   * returned an error. Let's do a best effort attempt to infer the watch root and relative
   * path for each bad include *)
  let watched_path_expression_terms =
    SSet.fold
      (fun path terms ->
        String_utils.(
          match SSet.find_first_opt (fun root -> string_starts_with path root) watch_roots with
          | None -> failwith (spf "Cannot deduce watch root for path %s" path)
          | Some root ->
            let relative_path = lstrip (lstrip path root) Caml.Filename.dir_sep in
            prepend_relative_path_term ~relative_path ~terms))
      failed_paths
      watched_path_expression_terms
  in
  (* All of our watched paths should have the same watch root. Let's assert that *)
  let watch_root =
    match SSet.elements watch_roots with
    | [] -> failwith "Cannot run watchman with fewer than 1 root"
    | [watch_root] -> watch_root
    | _ ->
      failwith
        (spf
           "Can't watch paths across multiple Watchman watch_roots. Found %d watch_roots"
           (SSet.cardinal watch_roots))
  in
  let%bind clockspec = get_clockspec ~debug_logging ~conn ~watch_root prior_clockspec in
  let watched_path_expression_terms =
    Option.map watched_path_expression_terms ~f:(J.pred "anyof")
  in
  let env =
    {
      settings =
        {
          debug_logging;
          defer_states;
          expression_terms;
          mergebase_with;
          roots;
          subscribe_mode;
          subscription_prefix;
          sync_timeout;
        };
      conn;
      watch_root;
      watched_path_expression_terms;
      clockspec;
      subscription = Printf.sprintf "%s.%d" subscription_prefix (Unix.getpid ());
    }
  in
  let%map response =
    request
      ~debug_logging
      ~conn
      ~timeout:None (* the whole init process should be wrapped in a timeout *)
      (subscribe ~mode:subscribe_mode ~states:defer_states env)
  in
  ignore response;
  env

let init settings =
  catch
    ~f:(fun () ->
      let%map v = re_init settings in
      Some v)
    ~catch:(fun exn ->
      Hh_logger.exception_ ~prefix:"Watchman init: " exn;
      match Exception.unwrap exn with
      (* Avoid swallowing these *)
      | Exit.Exit_with _
      | Watchman_restarted ->
        Exception.reraise exn
      | _ -> Lwt.return None)

let extract_file_names env json =
  let open Hh_json.Access in
  return json
  >>= get_array "files"
  |> counit_with (fun _ ->
         (* When an hg.update happens, it shows up in the watchman subscription
            as a notification with no files key present. *)
         [])
  |> List.map ~f:(fun json ->
         let s = Hh_json.get_string_exn json in
         let abs = Caml.Filename.concat env.watch_root s in
         abs)
  |> SSet.of_list

let within_backoff_time attempts time =
  let attempts = min attempts 3 in
  let offset = 4 * (2 ** attempts) in
  Float.(Unix.time () >= time +. of_int offset)

let maybe_restart_instance instance =
  match instance with
  | Watchman_alive _ -> Lwt.return instance
  | Watchman_dead dead_env ->
    if dead_env.reinit_attempts >= max_reinit_attempts then
      let () = Hh_logger.log "Ran out of watchman reinit attempts. Exiting." in
      raise Exit.(Exit_with Watchman_failed)
    else if within_backoff_time dead_env.reinit_attempts dead_env.dead_since then (
      let () = Hh_logger.log "Attemping to reestablish watchman subscription" in
      (* TODO: don't hardcode this timeout *)
      let timeout = 120. in
      match%lwt
        with_timeout (Some timeout) @@ fun () ->
        re_init ~prior_clockspec:dead_env.prior_clockspec dead_env.prior_settings
      with
      | env ->
        Hh_logger.log "Watchman connection reestablished.";
        EventLogger.watchman_connection_reestablished ();
        Lwt.return (Watchman_alive env)
      | exception Timeout ->
        Hh_logger.log "Reestablishing watchman subscription timed out.";
        EventLogger.watchman_connection_reestablishment_failed
          (spf "Timed out after %f seconds" timeout);
        Lwt.return (Watchman_dead { dead_env with reinit_attempts = dead_env.reinit_attempts + 1 })
      | exception ((Lwt.Canceled | Exit.Exit_with _ | Watchman_restarted) as exn) ->
        (* Avoid swallowing these *)
        Exception.reraise (Exception.wrap exn)
      | exception e ->
        let exn = Exception.wrap e in
        Hh_logger.exception_ ~prefix:"Reestablishing watchman subscription failed: " exn;
        let exn_str =
          Printf.sprintf
            "%s\nBacktrace: %s"
            (Exception.get_ctor_string exn)
            (Exception.get_full_backtrace_string 500 exn)
        in
        EventLogger.watchman_connection_reestablishment_failed exn_str;
        Lwt.return (Watchman_dead { dead_env with reinit_attempts = dead_env.reinit_attempts + 1 })
    ) else
      Lwt.return instance

let close env = close_connection env.conn

let close_channel_on_instance env =
  let%map () = close env in
  dead_env_from_alive env

let with_instance instance ~try_to_restart ~on_alive ~on_dead =
  let%bind instance =
    if try_to_restart then
      maybe_restart_instance instance
    else
      Lwt.return instance
  in
  match instance with
  | Watchman_dead dead_env -> on_dead dead_env
  | Watchman_alive env -> on_alive env

(** Calls f on the instance, maybe restarting it if its dead and maybe
   * reverting it to a dead state if things go south. For example, if watchman
   * shuts the connection on us, or shuts down, or crashes, we revert to a dead
   * instance, upon which a restart will be attempted down the road.
   * Alternatively, we also proactively revert to a dead instance if it appears
   * to be unresponsive (Timeout), and if reading the payload from it is
   * taking too long. *)
let call_on_instance :
    watchman_instance ->
    string ->
    on_dead:(dead_env -> 'a) ->
    on_alive:(env -> (env * 'a) Lwt.t) ->
    (watchman_instance * 'a) Lwt.t =
  let on_dead' f dead_env = Lwt.return (Watchman_dead dead_env, f dead_env) in
  let on_alive' ~on_dead source f env =
    catch
      ~f:(fun () ->
        let%map (env, result) = f env in
        (Watchman_alive env, result))
      ~catch:(fun exn ->
        Hh_logger.exception_ ~prefix:("Watchman " ^ source ^ ": ") exn;
        let close_channel_on_instance' env =
          let%bind env = close_channel_on_instance env in
          on_dead' on_dead env
        in
        let log_died msg =
          Hh_logger.log "%s" msg;
          EventLogger.watchman_died_caught msg
        in
        match Exception.unwrap exn with
        | Sys_error msg when String.equal msg "Broken pipe" ->
          log_died ("Watchman Pipe broken.\n" ^ Exception.get_full_backtrace_string 500 exn);
          close_channel_on_instance' env
        | Sys_error msg when String.equal msg "Connection reset by peer" ->
          log_died
            ("Watchman connection reset by peer.\n" ^ Exception.get_full_backtrace_string 500 exn);
          close_channel_on_instance' env
        | Sys_error msg when String.equal msg "Bad file descriptor" ->
          (* This happens when watchman is tearing itself down after we
           * retrieved a sock address and connected to the sock address. That's
           * because Unix.open_connection (via Timeout.open_connection) doesn't
           * error even when the sock adddress is no longer valid and actually -
           * it returns a channel that will error at some later time when you
           * actually try to do anything with it (write to it, or even get the
           * file descriptor of it). I'm pretty sure we don't need to close the
           * channel when that happens since we never had a useable channel
           * to start with. *)
          log_died ("Watchman bad file descriptor.\n" ^ Exception.get_full_backtrace_string 500 exn);
          on_dead' on_dead (dead_env_from_alive env)
        | End_of_file ->
          log_died
            ("Watchman connection End_of_file.\n" ^ Exception.get_full_backtrace_string 500 exn);
          close_channel_on_instance' env
        | Read_payload_too_long ->
          log_died "Watchman reading payload too long. Closing channel";
          close_channel_on_instance' env
        | Timeout ->
          log_died "Watchman reading Timeout. Closing channel";
          close_channel_on_instance' env
        | Watchman_error msg ->
          log_died (Printf.sprintf "Watchman error: %s. Closing channel" msg);
          close_channel_on_instance' env
        | Subscription_canceled_by_watchman ->
          log_died "Watchman cancelled our subscription. Closing channel";
          close_channel_on_instance' env
        | _ ->
          let msg = Exception.to_string exn in
          EventLogger.watchman_uncaught_failure msg;
          raise Exit.(Exit_with Watchman_failed))
  in
  fun instance source ~on_dead ~on_alive ->
    with_instance
      instance
      ~try_to_restart:true
      ~on_dead:(on_dead' on_dead)
      ~on_alive:(on_alive' ~on_dead source on_alive)

let make_state_change_response state name data =
  let metadata = J.try_get_val "metadata" data in
  match state with
  | `Enter -> State_enter (name, metadata)
  | `Leave -> State_leave (name, metadata)

let extract_mergebase data =
  Hh_json.Access.(
    let accessor = return data in
    let ret =
      accessor >>= get_obj "clock" >>= get_string "clock" >>= fun (clock, _) ->
      accessor >>= get_obj "clock" >>= get_obj "scm" >>= get_string "mergebase"
      >>= fun (mergebase, _) -> return (clock, mergebase)
    in
    to_option ret)

let make_mergebase_changed_response env data =
  match extract_mergebase data with
  | None -> Error "Failed to extract mergebase"
  | Some (clock, mergebase) ->
    let files = extract_file_names env data in
    env.clockspec <- clock;
    let response = Changed_merge_base (mergebase, files, clock) in
    Ok (env, response)

let transform_asynchronous_get_changes_response env data =
  match data with
  | None -> (env, Files_changed SSet.empty)
  | Some data ->
    begin
      match make_mergebase_changed_response env data with
      | Ok (env, response) -> (env, response)
      | Error _ ->
        env.clockspec <- Jget.string_exn (Some data) "clock";
        if is_fresh_instance data then (
          Hh_logger.log "Watchman server is fresh instance. Exiting.";
          raise Exit.(Exit_with Watchman_fresh_instance)
        ) else (
          match Jget.string_opt (Some data) "state-enter" with
          | Some state -> (env, make_state_change_response `Enter state data)
          | None ->
            (match Jget.string_opt (Some data) "state-leave" with
            | Some state -> (env, make_state_change_response `Leave state data)
            | None -> (env, Files_changed (extract_file_names env data)))
        )
    end

let get_changes ?deadline instance =
  call_on_instance
    instance
    "get_changes"
    ~on_dead:(fun _ -> Watchman_unavailable)
    ~on_alive:(fun env ->
      let timeout =
        Option.map deadline (fun deadline ->
            let timeout = deadline -. Unix.time () in
            Float.max timeout 0.0)
      in
      let debug_logging = env.settings.debug_logging in
      let%map response = blocking_read ~debug_logging ~timeout ~conn:env.conn in
      let (env, result) = transform_asynchronous_get_changes_response env response in
      (env, Watchman_pushed result))

let get_mergebase_and_changes ~timeout instance =
  call_on_instance
    instance
    "get_mergebase_and_changes"
    ~on_dead:(fun _dead_env -> Error "Failed to connect to Watchman to get mergebase")
    ~on_alive:(fun env ->
      let%map response =
        request
          ~timeout
          ~debug_logging:env.settings.debug_logging
          (get_changes_since_mergebase_query env)
      in
      match extract_mergebase response with
      | Some (_clock, mergebase) ->
        let changes = extract_file_names env response in
        (env, Ok (mergebase, changes))
      | None -> (env, Error "Failed to extract mergebase from response"))

let conn_of_instance = function
  | Watchman_dead _ -> None
  | Watchman_alive { conn; _ } -> Some conn

module Testing = struct
  let test_settings =
    {
      debug_logging = false;
      defer_states = [];
      expression_terms = [];
      mergebase_with = "hash";
      roots = [Path.dummy_path];
      subscribe_mode = Defer_changes;
      subscription_prefix = "dummy_prefix";
      sync_timeout = None;
    }

  let get_test_conn () =
    let%lwt reader = Buffered_line_reader_lwt.get_null_reader ()
    and oc = Lwt_io.open_file ~mode:Lwt_io.output "/dev/null" in
    Lwt.return (reader, oc)

  let get_test_env () =
    let%map conn = get_test_conn () in
    {
      settings = test_settings;
      conn;
      watch_root = "/path/to/root";
      clockspec = "";
      watched_path_expression_terms =
        Some (J.pred "anyof" [J.strlist ["dirname"; "foo"]; J.strlist ["name"; "foo"]]);
      subscription = "dummy_prefix.123456789";
    }

  let transform_asynchronous_get_changes_response env json =
    transform_asynchronous_get_changes_response env json
end
