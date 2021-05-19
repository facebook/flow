(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
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

exception Watchman_error

exception Subscription_canceled_by_watchman

exception Watchman_restarted

exception Config_problem

type error_severity =
  | Not_installed of { path: string }
  | Socket_unavailable of { msg: string }
  | Response_error of {
      request: string option;
      response: string;
      msg: string;
    }
  | Fresh_instance
  | Subscription_canceled
  | Restarted
  | Unsupported_watch_roots of {
      roots: string list;  (** roots returned by watch-project. either 0 or >1 items. *)
      failed_paths: string list;  (** paths we tried to watch but couldn't figure out the root for *)
    }

let log_error ?request ?response msg =
  let response = Option.map ~f:(String_utils.truncate 100000) response in
  let () = EventLogger.watchman_error ?request ?response msg in
  let () = Hh_logger.error "%s" msg in
  ()

let raise_error = function
  | Not_installed { path } ->
    let msg = spf "watchman not found on PATH: %s" path in
    let () = log_error msg in
    raise Config_problem
  | Socket_unavailable { msg } ->
    let () = log_error msg in
    raise Watchman_error
  | Response_error { request; response; msg } ->
    let () = log_error ?request ~response msg in
    raise Watchman_error
  | Fresh_instance ->
    Hh_logger.log "Watchman server is fresh instance. Exiting.";
    raise Exit.(Exit_with Watchman_fresh_instance)
  | Subscription_canceled ->
    EventLogger.watchman_error "Subscription canceled by watchman";
    raise Subscription_canceled_by_watchman
  | Restarted ->
    Hh_logger.error "Watchman server restarted so we may have missed some updates";
    raise Watchman_restarted
  | Unsupported_watch_roots { roots; failed_paths } ->
    let msg =
      match roots with
      | [] -> spf "Cannot deduce watch root for paths:\n%s" (String.concat ~sep:"\n" failed_paths)
      | _ ->
        spf
          "Can't watch paths across multiple Watchman watch_roots. Found %d watch roots:\n%s"
          (List.length roots)
          (String.concat ~sep:"\n" roots)
    in
    let () = log_error msg in
    raise Config_problem

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

let json_of_string str =
  try Ok (Hh_json.json_of_string str)
  with Hh_json.Syntax_error err ->
    let msg = spf "Failed to parse string as JSON: %s" err in
    Error msg

(** Parses a watchman response into JSON and checks for errors *)
let parse_response =
  let log_warnings obj =
    match Jget.string_opt (Some obj) "warning" with
    | None -> ()
    | Some warning ->
      EventLogger.watchman_warning warning;
      Hh_logger.log "Watchman warning: %s\n" warning
  in
  (* Looks for common errors in watchman responses *)
  let handle_errors obj =
    match Jget.string_opt (Some obj) "error" with
    | None -> Ok obj
    | Some error -> Error error
  in
  fun ~debug_logging output ->
    if debug_logging then Hh_logger.info "Watchman response: %s" output;
    let open Result.Let_syntax in
    let%bind json = json_of_string output in
    log_warnings json;
    handle_errors json

type conn = Buffered_line_reader_lwt.t * Lwt_io.output_channel

let catch ~f ~catch =
  Lwt.catch f (fun exn ->
      let e = Exception.wrap exn in
      match exn with
      | Lwt.Canceled -> Exception.reraise e
      | _ -> catch e)

(** This number is totally arbitrary. Just need some cap. *)
let max_reinit_attempts = 8

(** Watchman watches a single root folder, and we may only care about some paths
    within it. All queries have to be against the root, and include terms to
    limit it to just the sub-paths we want. *)
type watch = {
  watch_root: string;
  watched_path_expression_terms: Hh_json.json option;
}

type dead_env = {
  (* Will reuse original settings to reinitializing watchman subscription. *)
  prior_settings: init_settings;
  reinit_attempts: int;
  dead_since: float;
  prior_clockspec: string;
  prior_watch_root: string;
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
  watch: watch;
}

let dead_env_from_alive env =
  {
    prior_settings = env.settings;
    dead_since = Unix.gettimeofday ();
    reinit_attempts = 0;
    (* When we start a new watchman connection, we continue to use the prior
     * clockspec. If the same watchman server is still alive, then all is
     * good. If not, the clockspec allows us to detect whether a new watchman
     * server had to be started. See also "is_fresh_instance" on watchman's
     * "since" response. *)
    prior_clockspec = env.clockspec;
    prior_watch_root = env.watch.watch_root;
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

let response_error_of_json ?query ~response msg =
  let request = Option.map ~f:Hh_json.json_to_string query in
  let response = Hh_json.json_to_string response in
  Response_error { request; response; msg }

let get_string_prop ?query prop json =
  let open Hh_json.Access in
  match return json >>= get_string prop with
  | Ok (result, _keytrace) -> Ok result
  | Error err ->
    let msg = access_failure_to_string err in
    Error (response_error_of_json ?query ~response:json msg)

let clock { watch_root; _ } = J.strlist ["clock"; watch_root]

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
      [JSON_String command; JSON_String env.watch.watch_root]
      @
      match watchman_command with
      | Subscribe -> [JSON_String env.subscription]
      | _ -> []
    in
    let expressions = extra_expressions @ env.settings.expression_terms in
    let expressions =
      match env.watch.watched_path_expression_terms with
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

let subscribe env =
  let states = "hg.update" :: env.settings.defer_states in
  let (since, mode) =
    match env.settings.subscribe_mode with
    | All_changes -> (Hh_json.JSON_String env.clockspec, [])
    | Defer_changes -> (Hh_json.JSON_String env.clockspec, [("defer", J.strlist states)])
    | Drop_changes -> (Hh_json.JSON_String env.clockspec, [("drop", J.strlist states)])
  in
  request_json
    ~extra_kv:(([("since", since)] @ mode) @ [("empty_on_fresh_instance", Hh_json.JSON_Bool true)])
    Subscribe
    env

(** We filter all responses from get_changes through this. This is to detect
   * Watchman server crashes.
   *
   * See also Watchman docs on "since" query parameter. *)
let is_fresh_instance obj =
  Hh_json.Access.(return obj >>= get_bool "is_fresh_instance" |> project_bool)

(****************************************************************************)
(* I/O stuff *)
(****************************************************************************)

(* Send a request to the watchman process *)
let send_request ~debug_logging oc json =
  let json_str = Hh_json.(json_to_string json) in
  if debug_logging then Hh_logger.info "Watchman request: %s" json_str;

  try%lwt
    (* Print the json with a newline and then flush *)
    let%lwt () = Lwt_io.fprintl oc json_str in
    let%lwt () = Lwt_io.flush oc in
    Lwt.return (Ok ())
  with Unix.Unix_error (Unix.EPIPE, _, _) ->
    let msg = "Connection closed (EPIPE)" in
    Lwt.return (Error (Socket_unavailable { msg }))

let get_sockname () =
  let cmd = "watchman" in
  let args = ["--no-pretty"; "get-sockname"] in
  let%lwt { LwtSysUtils.status; stdout; stderr } = LwtSysUtils.exec cmd args in
  match status with
  | Unix.WEXITED 0 ->
    let json = Hh_json.json_of_string stdout in
    Lwt.return (Ok (J.get_string_val "sockname" json))
  | Unix.WEXITED 127 ->
    let path = Option.value (Sys_utils.getenv_path ()) ~default:"(not set)" in
    Lwt.return (Error (Not_installed { path }))
  | Unix.WEXITED code ->
    let msg = spf "get-sockname exited code %d, stderr = %S" code stderr in
    Lwt.return (Error (Socket_unavailable { msg }))
  | Unix.WSIGNALED signal ->
    let signal = PrintSignal.string_of_signal signal in
    let msg = spf "get-sockname signaled with %s signal" signal in
    Lwt.return (Error (Socket_unavailable { msg }))
  | Unix.WSTOPPED signal ->
    let signal = PrintSignal.string_of_signal signal in
    let msg = spf "get-sockname stopped with %s signal" signal in
    Lwt.return (Error (Socket_unavailable { msg }))

(** Opens a connection to the watchman process through the socket *)
let open_connection =
  (* Opens a Unix socket. Could raise a [Unix.Unix_error]. *)
  let open_socket_exn sockname =
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
  in
  let open_socket sockname =
    try Ok (open_socket_exn sockname)
    with Unix.Unix_error (error, _, _) ->
      let msg = spf "%s (socket: %s)" (Unix.error_message error) sockname in
      Error (Socket_unavailable { msg })
  in
  let connection_of_sockname sockname =
    let open Result.Let_syntax in
    let%bind (ic, oc) = open_socket sockname in
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
    Ok (reader, oc)
  in
  fun () ->
    match%lwt get_sockname () with
    | Ok sockname -> Lwt.return (connection_of_sockname sockname)
    | Error _ as err -> Lwt.return err

let close_connection (reader, oc) =
  let ic = Buffered_line_reader_lwt.get_fd reader in
  let%lwt () =
    (* We call [close_connection] in [with_watchman_conn] even on error. Sometimes
       those errors are because the connection was closed unexpectedly, so we should
      ignore an already-closed connection (BADF) here. *)
    try%lwt Lwt_unix.close ic with Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit
  in
  let%lwt () =
    (* As mention in [open_connection], if we open the connection with [Unix.open_connection],
       we use a single fd for both input and output. That means we might be trying to close
       it twice here. If so, this second close with throw. So let's catch that exception and
       ignore it. *)
    try%lwt Lwt_io.close oc with Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit
  in
  Lwt.return_unit

let with_watchman_conn f =
  match%lwt open_connection () with
  | Ok conn ->
    let%lwt result =
      try%lwt f conn
      with e ->
        let e = Exception.wrap e in
        let%lwt () = close_connection conn in
        Exception.reraise e
    in
    let%lwt () = close_connection conn in
    Lwt.return result
  | Error _ as err -> Lwt.return err

let read_line reader =
  try%lwt
    let%lwt line = Buffered_line_reader_lwt.get_next_line reader in
    Lwt.return (Ok line)
  with
  | End_of_file ->
    let msg = "Connection closed (end of file)" in
    Lwt.return (Error (Socket_unavailable { msg }))
  | Unix.Unix_error (Unix.EBADF, _, _) ->
    let msg = "Connection closed (bad file descriptor)" in
    Lwt.return (Error (Socket_unavailable { msg }))
  | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
    let msg = "Connection closed (connection reset)" in
    Lwt.return (Error (Socket_unavailable { msg }))

let read_line_with_timeout ~timeout reader =
  try%lwt Lwt_unix.with_timeout timeout @@ fun () -> read_line reader
  with Lwt_unix.Timeout ->
    let msg = spf "Timed out reading payload after %f seconds" timeout in
    Lwt.return (Error (Socket_unavailable { msg }))

(* Sends a request to watchman and returns the response. If we don't have a connection,
 * a new connection will be created before the request and destroyed after the response *)
let rec request ~debug_logging ?conn json =
  match conn with
  | None -> with_watchman_conn (fun conn -> request ~debug_logging ~conn json)
  | Some (reader, oc) ->
    (match%lwt send_request ~debug_logging oc json with
    | Ok () ->
      (match%lwt read_line reader with
      | Ok response ->
        (match parse_response ~debug_logging response with
        | Ok _ as result -> Lwt.return result
        | Error msg ->
          let request = Some (Hh_json.json_to_string json) in
          Lwt.return (Error (Response_error { request; response; msg })))
      | Error _ as err -> Lwt.return err)
    | Error _ as err -> Lwt.return err)

let wait_read reader =
  match%lwt Lwt_unix.wait_read (Buffered_line_reader_lwt.get_fd reader) with
  | () -> Lwt.return (Ok ())
  | exception Unix.Unix_error (Unix.EBADF, _, _) ->
    (* this is a curious error. it means that the file descriptor was already
       closed via `Lwt_unix.close` before we called `wait_read`, and the only
       place we do that is in `close_connection`. So that suggests that we're
       calling `Watchman.close` and then still calling `blocking_read` on the
       same instance again, but it's not clear where; we are cancelling those
       promises. *)
    let msg = "Connection closed (bad file descriptor)" in
    Lwt.return (Error (Socket_unavailable { msg }))

let blocking_read ~debug_logging ~conn:(reader, _) =
  match%lwt wait_read reader with
  | Error _ as err -> Lwt.return err
  | Ok () ->
    (match%lwt read_line_with_timeout 40.0 reader with
    | Error _ as err -> Lwt.return err
    | Ok response ->
      (match parse_response ~debug_logging response with
      | Ok _ as response -> Lwt.return response
      | Error msg -> Lwt.return (Error (Response_error { request = None; response; msg }))))

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
let has_watchman_restarted_since ~debug_logging ~conn ~watch_root ~clockspec =
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
              (* by default, watchman will wait up to 60s to synchronize with the filesystem.
                 we don't care about the actual state of anything in this query, only whether
                 it's a new watchman instance, so set it to 0 *)
              ("sync_timeout", JSON_Number "0");
            ];
        ])
  in
  match%lwt request ~debug_logging ~conn query with
  | Ok response ->
    (match Hh_json_helpers.Jget.bool_opt (Some response) "is_fresh_instance" with
    | Some has_restarted -> Lwt.return (Ok has_restarted)
    | None ->
      (* The response to this query **always** should include the `is_fresh_instance` boolean
       * property. If it is missing then something has gone wrong with Watchman. Since we can't
       * prove that Watchman has not restarted, we must treat this as an error. *)
      let msg = "Expected an is_fresh_instance property" in
      Lwt.return (Error (response_error_of_json ~query ~response msg)))
  | Error (Response_error _) ->
    (* if this simple query failed, it is because the root isn't being watched, so it may have
       missed updates. *)
    Lwt.return (Ok true)
  | Error _ as err -> Lwt.return err

let has_watchman_restarted dead_env =
  with_watchman_conn (fun conn ->
      let debug_logging = dead_env.prior_settings.debug_logging in
      let clockspec = dead_env.prior_clockspec in
      let watch_root = dead_env.prior_watch_root in
      has_watchman_restarted_since ~debug_logging ~conn ~watch_root ~clockspec)

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

(** Gets the clockspec. Uses the clockspec from the prior connection if one exists. *)
let get_clockspec ~debug_logging ~conn ~watch prior_clockspec =
  match prior_clockspec with
  | Some clockspec -> Lwt.return (Ok clockspec)
  | None ->
    let query = clock watch in
    let%lwt response = request ~debug_logging ~conn query in
    Lwt.return (Result.bind ~f:(get_string_prop ~query "clock") response)

(** Watch this root

    If the path doesn't exist or the request errors for any other reason, this will
    return [None]. Otherwise, returns the watch root and relative path to that root. *)
let watch_project ~debug_logging ~conn root =
  let query = J.strlist ["watch-project"; Path.to_string root] in
  let%lwt response = request ~debug_logging ~conn query in
  let result =
    match response with
    | Error (Response_error _) -> Ok None
    | Error _ as err -> err
    | Ok response ->
      let watch_root = J.get_string_val "watch" response in
      let relative_path = J.get_string_val "relative_path" ~default:"" response in
      Ok (Some (watch_root, relative_path))
  in
  Lwt.return result

(** Calls [watchman watch-project] on a list of paths to watch (e.g. all of the
    include directories). The paths may be children of the actual watchman root,
    in which case watch-project returns the watch root and relative path.
    This function computes the set of terms that should be included in queries
    against the root to filter it to only the relative paths we care about. *)
let watch_paths ~debug_logging ~conn paths =
  LwtUtils.fold_result_s
    ~f:(fun (terms, watch_roots, failed_paths) path ->
      match%lwt watch_project ~debug_logging ~conn path with
      | Error _ as err -> Lwt.return err
      | Ok None -> Lwt.return (Ok (terms, watch_roots, SSet.add (Path.to_string path) failed_paths))
      | Ok (Some (watch_root, relative_path)) ->
        let terms = prepend_relative_path_term ~relative_path ~terms in
        let watch_roots = SSet.add watch_root watch_roots in
        Lwt.return (Ok (terms, watch_roots, failed_paths)))
    ~init:(Some [], SSet.empty, SSet.empty)
    paths

let watch =
  (* The failed_paths are likely includes which don't exist on the filesystem, so watch_project
    returned an error. Let's do a best effort attempt to infer the relative path for each bad
    include. *)
  let guess_missing_relative_paths terms watch_root failed_paths =
    let failed_paths = SSet.elements failed_paths in
    let (terms, failed_paths) =
      List.fold
        ~f:(fun (terms, failed_paths) path ->
          let open String_utils in
          if string_starts_with path watch_root then
            let relative_path = lstrip (lstrip path watch_root) Caml.Filename.dir_sep in
            (prepend_relative_path_term ~relative_path ~terms, failed_paths)
          else
            (terms, path :: failed_paths))
        ~init:(terms, [])
        failed_paths
    in
    match failed_paths with
    | [] -> Ok terms
    | _ -> Error (Unsupported_watch_roots { roots = []; failed_paths })
  in
  (* All of our watched paths should have the same watch root. Let's assert that *)
  let consolidate_watch_roots watch_roots failed_paths =
    match SSet.elements watch_roots with
    | [watch_root] -> Ok watch_root
    | roots ->
      let failed_paths = SSet.elements failed_paths in
      Error (Unsupported_watch_roots { roots; failed_paths })
  in
  fun ~debug_logging ~conn roots ->
    match%lwt watch_paths ~debug_logging ~conn roots with
    | Ok (terms, watch_roots, failed_paths) ->
      (match consolidate_watch_roots watch_roots failed_paths with
      | Ok watch_root ->
        (match guess_missing_relative_paths terms watch_root failed_paths with
        | Ok terms ->
          let watched_path_expression_terms = Option.map terms ~f:(J.pred "anyof") in
          Lwt.return (Ok { watch_root; watched_path_expression_terms })
        | Error _ as err -> Lwt.return err)
      | Error _ as err -> Lwt.return err)
    | Error _ as err -> Lwt.return err

let re_init ?prior_clockspec settings =
  let open Lwt_result.Infix in
  let debug_logging = settings.debug_logging in
  open_connection () >>= fun conn ->
  watch ~debug_logging ~conn settings.roots >>= fun watch ->
  get_clockspec ~debug_logging ~conn ~watch prior_clockspec >>= fun clockspec ->
  let subscription = Printf.sprintf "%s.%d" settings.subscription_prefix (Unix.getpid ()) in
  let env = { settings; conn; watch; clockspec; subscription } in
  request ~debug_logging ~conn (subscribe env) >>= fun response ->
  ignore response;
  Lwt.return (Ok env)

let re_init_exn ?prior_clockspec settings =
  match%lwt re_init ?prior_clockspec settings with
  | Ok env -> Lwt.return env
  | Error err -> raise_error err

let init settings =
  catch
    ~f:(fun () ->
      let%lwt v = re_init_exn settings in
      Lwt.return (Some (Watchman_alive v)))
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
         let abs = Caml.Filename.concat env.watch.watch_root s in
         abs)
  |> SSet.of_list

let within_backoff_time attempts time =
  let attempts = min attempts 3 in
  let offset = 4 * (2 ** attempts) in
  Float.(Unix.gettimeofday () >= time +. of_int offset)

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
        Lwt_unix.with_timeout timeout @@ fun () ->
        match%lwt has_watchman_restarted dead_env with
        | Ok false -> re_init_exn ~prior_clockspec:dead_env.prior_clockspec dead_env.prior_settings
        | Ok true -> raise_error Restarted
        | Error err -> raise_error err
      with
      | env ->
        Hh_logger.log "Watchman connection reestablished.";
        let downtime = Unix.gettimeofday () -. dead_env.dead_since in
        EventLogger.watchman_connection_reestablished downtime;
        Lwt.return (Watchman_alive env)
      | exception Lwt_unix.Timeout ->
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

let close_channel_on_instance env =
  let%lwt () = close_connection env.conn in
  Lwt.return (dead_env_from_alive env)

let close = function
  | Watchman_dead _ -> Lwt.return ()
  | Watchman_alive env -> close_connection env.conn

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
    on_alive:(env -> (env * 'a, error_severity) Result.t Lwt.t) ->
    (watchman_instance * 'a) Lwt.t =
  let on_alive' ~on_dead source f env =
    catch
      ~f:(fun () ->
        match%lwt f env with
        | Ok (env, result) -> Lwt.return (Watchman_alive env, result)
        | Error err -> raise_error err)
      ~catch:(fun exn ->
        Hh_logger.exception_ ~prefix:("Watchman " ^ source ^ ": ") exn;
        let close_channel_on_instance' env =
          let%lwt env = close_channel_on_instance env in
          on_dead env
        in
        match Exception.unwrap exn with
        | Watchman_error -> close_channel_on_instance' env
        | Subscription_canceled_by_watchman -> close_channel_on_instance' env
        | Config_problem ->
          (* this only happens during (re)init, so we don't need to handle it here.
             it's unexpected if it happens, so reraising it like other unexpected
             exceptions, for explictness. *)
          Exception.reraise exn
        | _ -> Exception.reraise exn)
  in
  fun instance source ~on_dead ~on_alive ->
    let on_dead dead_env = Lwt.return (Watchman_dead dead_env, on_dead dead_env) in
    match%lwt maybe_restart_instance instance with
    | Watchman_dead dead_env -> on_dead dead_env
    | Watchman_alive env -> on_alive' ~on_dead source on_alive env

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
  | None -> None
  | Some (clock, mergebase) ->
    let files = extract_file_names env data in
    env.clockspec <- clock;
    let response = Changed_merge_base (mergebase, files, clock) in
    Some (env, response)

let subscription_is_cancelled data =
  match Jget.bool_opt (Some data) "canceled" with
  | None
  | Some false ->
    false
  | Some true -> true

let transform_asynchronous_get_changes_response env data =
  match make_mergebase_changed_response env data with
  | Some (env, response) -> Ok (env, response)
  | None ->
    if is_fresh_instance data then
      Error Fresh_instance
    else if subscription_is_cancelled data then
      Error Subscription_canceled
    else (
      env.clockspec <- Jget.string_exn (Some data) "clock";
      match Jget.string_opt (Some data) "state-enter" with
      | Some state -> Ok (env, make_state_change_response `Enter state data)
      | None ->
        (match Jget.string_opt (Some data) "state-leave" with
        | Some state -> Ok (env, make_state_change_response `Leave state data)
        | None -> Ok (env, Files_changed (extract_file_names env data)))
    )

let get_changes instance =
  call_on_instance
    instance
    "get_changes"
    ~on_dead:(fun _ -> Watchman_unavailable)
    ~on_alive:(fun env ->
      let debug_logging = env.settings.debug_logging in
      match%lwt blocking_read ~debug_logging ~conn:env.conn with
      | Error _ as err -> Lwt.return err
      | Ok response ->
        (match transform_asynchronous_get_changes_response env response with
        | Error _ as err -> Lwt.return err
        | Ok (env, result) -> Lwt.return (Ok (env, Watchman_pushed result))))

let get_mergebase_and_changes instance =
  call_on_instance
    instance
    "get_mergebase_and_changes"
    ~on_dead:(fun _dead_env -> Error "Failed to connect to Watchman to get mergebase")
    ~on_alive:(fun env ->
      let debug_logging = env.settings.debug_logging in
      let query = get_changes_since_mergebase_query env in
      match%lwt request ~debug_logging query with
      | Error _ as err -> Lwt.return err
      | Ok response ->
        let result =
          match extract_mergebase response with
          | Some (_clock, mergebase) ->
            let changes = extract_file_names env response in
            Ok (env, Ok (mergebase, changes))
          | None -> Error (response_error_of_json ~query ~response "Failed to extract mergebase")
        in
        Lwt.return result)

module Testing = struct
  type nonrec env = env

  type nonrec error_severity = error_severity

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
    let%lwt conn = get_test_conn () in
    Lwt.return
      {
        settings = test_settings;
        conn;
        clockspec = "";
        watch =
          {
            watch_root = "/path/to/root";
            watched_path_expression_terms =
              Some (J.pred "anyof" [J.strlist ["dirname"; "foo"]; J.strlist ["name"; "foo"]]);
          };
        subscription = "dummy_prefix.123456789";
      }

  let transform_asynchronous_get_changes_response env json =
    transform_asynchronous_get_changes_response env json
end
