(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_kernel
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

module Watchman_actual = struct
  include Watchman_sig.Types
  module J = Hh_json_helpers.AdhocJsonHelpers

  (* Throw this exception when we know there is something to read from
   * the watchman channel, but reading took too long. *)
  exception Read_payload_too_long

  (* Looks for common errors in watchman responses *)
  let assert_no_error obj =
    (try
       let warning = J.get_string_val "warning" obj in
       EventLogger.watchman_warning warning;
       Hh_logger.log "Watchman warning: %s\n" warning
     with Caml.Not_found -> ());
    (try
       let error = J.get_string_val "error" obj in
       EventLogger.watchman_error error;
       raise @@ Watchman_error error
     with Caml.Not_found -> ());
    try
      let canceled = J.get_bool_val "canceled" obj in
      if canceled then (
        EventLogger.watchman_error "Subscription canceled by watchman";
        raise @@ Subscription_canceled_by_watchman
      ) else
        ()
    with Caml.Not_found -> ()

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

  let ( >>= ) = Lwt.( >>= )

  let ( >|= ) = Lwt.( >|= )

  type conn = Buffered_line_reader_lwt.t * Lwt_io.output_channel

  let catch ~f ~catch =
    Lwt.catch f (fun exn ->
        let e = Exception.wrap exn in
        match exn with
        | Lwt.Canceled -> Exception.reraise e
        | _ -> catch e)

  (** This number is totally arbitrary. Just need some cap. *)
  let max_reinit_attempts = 8

  (**
   * Triggers watchman to flush buffered updates on this subscription.
   * See watchman/docs/cmd/flush-subscriptions.html
   *)
  let flush_subscriptions_cmd = "cmd-flush-subscriptions"

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
     * facility to detect watchman process crashes between two "since" queries
     *
     * See also assert_no_fresh_instance *)
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
      assert (expressions <> []);
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
    let extra_kv =
      [
        ( "since",
          Hh_json.JSON_Object
            [("scm", Hh_json.JSON_Object [("mergebase-with", Hh_json.JSON_String "master")])] );
      ]
    in
    request_json ~extra_kv Query env

  let since_query env =
    request_json
      ~extra_kv:
        [
          ("since", Hh_json.JSON_String env.clockspec);
          ("empty_on_fresh_instance", Hh_json.JSON_Bool true);
        ]
      Query
      env

  let subscribe ~mode env =
    let (since, mode) =
      match mode with
      | All_changes -> (Hh_json.JSON_String env.clockspec, [])
      | Defer_changes -> (Hh_json.JSON_String env.clockspec, [("defer", J.strlist ["hg.update"])])
      | Drop_changes -> (Hh_json.JSON_String env.clockspec, [("drop", J.strlist ["hg.update"])])
      | Scm_aware ->
        Hh_logger.log "Making Scm_aware subscription";
        let scm = Hh_json.JSON_Object [("mergebase-with", Hh_json.JSON_String "master")] in
        let since = Hh_json.JSON_Object [("scm", scm); ("drop", J.strlist ["hg.update"])] in
        (since, [])
    in
    request_json
      ~extra_kv:(([("since", since)] @ mode) @ [("empty_on_fresh_instance", Hh_json.JSON_Bool true)])
      Subscribe
      env

  let watch_project root = J.strlist ["watch-project"; root]

  (* See https://facebook.github.io/watchman/docs/cmd/version.html *)
  let capability_check ?(optional = []) required =
    Hh_json.(
      JSON_Array
        ( [JSON_String "version"]
        @ [JSON_Object [("optional", J.strlist optional); ("required", J.strlist required)]] ))

  (** We filter all responses from get_changes through this. This is to detect
   * Watchman server crashes.
   *
   * See also Watchman docs on "since" query parameter. *)
  let assert_no_fresh_instance obj =
    Hh_json.Access.(
      let _ =
        return obj >>= get_bool "is_fresh_instance" >>= fun (is_fresh, trace) ->
        if is_fresh then (
          Hh_logger.log "Watchman server is fresh instance. Exiting.";
          raise Exit_status.(Exit_with Watchman_fresh_instance)
        ) else
          Ok ((), trace)
      in
      ())

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
          (Base.Option.value (Sys_utils.getenv_path ()) ~default:"(not set)")
      in
      let () = EventLogger.watchman_error msg in
      let () = Hh_logger.error "%s" msg in
      raise (Watchman_error "watchman not found on PATH")
    | Unix.WEXITED code ->
      let () =
        EventLogger.watchman_error (spf "watchman exited code %d, stderr = %S" code stderr)
      in
      raise (Watchman_error (spf "watchman exited code %d" code))
    | Unix.WSIGNALED signal ->
      let msg = spf "watchman signaled with %s signal" (PrintSignal.string_of_signal signal) in
      let () = EventLogger.watchman_error msg in
      raise (Watchman_error msg)
    | Unix.WSTOPPED signal ->
      let msg = spf "watchman stopped with %s signal" (PrintSignal.string_of_signal signal) in
      let () = EventLogger.watchman_error msg in
      raise (Watchman_error msg)

  (* Opens a connection to the watchman process through the socket *)
  let open_connection () =
    let%lwt sockname = get_sockname () in
    let (ic, oc) =
      if
        Sys.os_type = "Unix"
        (* Yes, I know that Unix.open_connection uses the same fd for input and output. But I don't
         * want to hardcode that assumption here. So let's pretend like ic and oc might be back by
         * different fds *)
      then
        Unix.open_connection (Unix.ADDR_UNIX sockname)
      (* On Windows, however, named pipes behave like regular files from the client's perspective.
       * We just open the file and create in/out channels for it. The file permissions attribute
       * is not needed because the file should exist already but we have to pass something. *)
      else
        let fd = Unix.openfile sockname [Unix.O_RDWR] 0o640 in
        (Unix.in_channel_of_descr fd, Unix.out_channel_of_descr fd)
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
        try%lwt
          Lwt_unix.with_timeout 40.0 @@ fun () -> Buffered_line_reader_lwt.get_next_line reader
        with Lwt_unix.Timeout ->
          let () = Hh_logger.log "blocking_read timed out" in
          raise Read_payload_too_long
      in
      Lwt.return @@ Some (sanitize_watchman_response ~debug_logging output)

  (****************************************************************************)
  (* Initialization, reinitialization, and crash-tracking. *)
  (****************************************************************************)

  let with_crash_record_exn source f =
    catch ~f ~catch:(fun exn ->
        Hh_logger.exception_ ~prefix:("Watchman " ^ source ^ ": ") exn;
        Exception.reraise exn)

  let with_crash_record_opt source f =
    catch
      ~f:(fun () -> with_crash_record_exn source f >|= fun v -> Some v)
      ~catch:(fun exn ->
        match Exception.unwrap exn with
        (* Avoid swallowing these *)
        | Exit_status.Exit_with _
        | Watchman_restarted ->
          Exception.reraise exn
        | _ -> Lwt.return None)

  let has_capability name capabilities =
    (* Projects down from the boolean error monad into booleans.
     * Error states go to false, values are projected directly. *)
    let project_bool m =
      match m with
      | Ok (v, _) -> v
      | Error _ -> false
    in
    Hh_json.Access.(
      return capabilities >>= get_obj "capabilities" >>= get_bool name |> project_bool)

  (* When we re-init our connection to Watchman, we use the old clockspec to get all the changes
   * since our last response. However, if Watchman has restarted and the old clockspec pre-dates
   * the new Watchman, then we may miss updates. It is important for Flow and Hack to restart
   * in that case.
   *
   * Unfortunately, the response to "subscribe" doesn't have the "is_fresh_instance" field. So
   * we'll instead send a small "query" request. It should always return 0 files, but it should
   * tell us whether the Watchman service has restarted since clockspec.
   *)
  let assert_watchman_has_not_restarted_since ~debug_logging ~conn ~timeout ~watch_root ~clockspec =
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
    request ~debug_logging ~conn ~timeout query >>= fun response ->
    match Hh_json_helpers.Jget.bool_opt (Some response) "is_fresh_instance" with
    | Some false -> Lwt.return ()
    | Some true ->
      Hh_logger.error "Watchman server restarted so we may have missed some updates";
      raise Watchman_restarted
    | None ->
      (* The response to this query **always** should include the `is_fresh_instance` boolean
       * property. If it is missing then something has gone wrong with Watchman. Since we can't
       * prove that Watchman has not restarted, we must treat this as an error. *)
      Hh_logger.error
        "Invalid Watchman response to `empty_on_fresh_instance` query:\n%s"
        (Hh_json.json_to_string ~pretty:true response);
      raise Exit_status.(Exit_with Watchman_failed)

  let prepend_relative_path_term ~relative_path ~terms =
    match terms with
    | None -> None
    | Some _ when relative_path = "" ->
      (* If we're watching the watch root directory, then there's no point in specifying a list of
       * files and directories to watch. We're already subscribed to any change in this watch root
       * anyway *)
      None
    | Some terms ->
      (* So lets say we're being told to watch foo/bar. Is foo/bar a directory? Is it a file? If it
       * is a file now, might it become a directory later? I'm not aware of aterm which will watch for either a file or a directory, so let's add two terms *)
      Some (J.strlist ["dirname"; relative_path] :: J.strlist ["name"; relative_path] :: terms)

  let re_init
      ?prior_clockspec
      { subscribe_mode; expression_terms; debug_logging; roots; subscription_prefix } =
    with_crash_record_opt "init" @@ fun () ->
    open_connection () >>= fun conn ->
    request
      ~debug_logging
      ~conn
      ~timeout:None (* the whole init process should be wrapped in a timeout *)
      (capability_check ~optional:[flush_subscriptions_cmd] ["relative_root"])
    >>= fun capabilities ->
    let supports_flush = has_capability flush_subscriptions_cmd capabilities in
    (* Disable subscribe if Watchman flush feature isn't supported. *)
    let subscribe_mode =
      if supports_flush then
        subscribe_mode
      else
        None
    in
    Lwt_list.fold_left_s
      (fun (terms, watch_roots, failed_paths) path ->
        (* Watch this root. If the path doesn't exist, watch_project will throw. In that case catch
         * the error and continue for now. *)
        catch
          ~f:(fun () ->
            request
              ~debug_logging
              ~conn
              ~timeout:None (* the whole init process should be wrapped in a timeout *)
              (watch_project (Path.to_string path))
            >|= fun response -> Some response)
          ~catch:(fun _ -> Lwt.return None)
        >|= fun response ->
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
    >>= fun (watched_path_expression_terms, watch_roots, failed_paths) ->
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
              let relative_path = lstrip (lstrip path root) Filename.dir_sep in
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
    (* If we don't have a prior clockspec, grab the current clock *)
    (match prior_clockspec with
    | Some clockspec ->
      assert_watchman_has_not_restarted_since
        ~debug_logging
        ~conn
        ~timeout:None (* the whole init process should be wrapped in a timeout *)
        ~watch_root
        ~clockspec
      >>= fun () -> Lwt.return clockspec
    | None ->
      request
        ~debug_logging
        ~conn
        ~timeout:None (* the whole init process should be wrapped in a timeout *)
        (clock watch_root)
      >|= J.get_string_val "clock")
    >>= fun clockspec ->
    let watched_path_expression_terms =
      Base.Option.map watched_path_expression_terms ~f:(J.pred "anyof")
    in
    let env =
      {
        settings = { debug_logging; subscribe_mode; expression_terms; roots; subscription_prefix };
        conn;
        watch_root;
        watched_path_expression_terms;
        clockspec;
        subscription = Printf.sprintf "%s.%d" subscription_prefix (Unix.getpid ());
      }
    in
    (match subscribe_mode with
    | None -> Lwt.return ()
    | Some mode ->
      request
        ~debug_logging
        ~conn
        ~timeout:None (* the whole init process should be wrapped in a timeout *)
        (subscribe ~mode env)
      >|= ignore)
    >|= fun () -> env

  let init ?since_clockspec settings () =
    let prior_clockspec = since_clockspec in
    re_init ?prior_clockspec settings

  let extract_file_names env json =
    let files =
      try J.get_array_val "files" json
      with
      (* When an hg.update happens, it shows up in the watchman subscription
       * as a notification with no files key present. *)
      | Caml.Not_found ->
        []
    in
    let files =
      List.map files (fun json ->
          let s = Hh_json.get_string_exn json in
          let abs = Filename.concat env.watch_root s in
          abs)
    in
    files

  let within_backoff_time attempts time =
    let offset =
      4.0
      *. 2.0
         ** float
              ( if attempts > 3 then
                3
              else
                attempts )
    in
    Unix.time () >= time +. offset

  let maybe_restart_instance instance =
    match instance with
    | Watchman_alive _ -> Lwt.return instance
    | Watchman_dead dead_env ->
      if dead_env.reinit_attempts >= max_reinit_attempts then
        let () = Hh_logger.log "Ran out of watchman reinit attempts. Exiting." in
        raise Exit_status.(Exit_with Watchman_failed)
      else if within_backoff_time dead_env.reinit_attempts dead_env.dead_since then (
        let () = Hh_logger.log "Attemping to reestablish watchman subscription" in
        (* TODO: don't hardcode this timeout *)
        with_timeout (Some 120.) @@ fun () ->
        re_init ~prior_clockspec:dead_env.prior_clockspec dead_env.prior_settings >|= function
        | None ->
          Hh_logger.log "Reestablishing watchman subscription failed.";
          EventLogger.watchman_connection_reestablishment_failed ();
          Watchman_dead { dead_env with reinit_attempts = dead_env.reinit_attempts + 1 }
        | Some env ->
          Hh_logger.log "Watchman connection reestablished.";
          EventLogger.watchman_connection_reestablished ();
          Watchman_alive env
      ) else
        Lwt.return instance

  let close env = close_connection env.conn

  let close_channel_on_instance env =
    close env >|= fun () ->
    EventLogger.watchman_died_caught ();
    dead_env_from_alive env

  let with_instance instance ~try_to_restart ~on_alive ~on_dead =
    ( if try_to_restart then
      maybe_restart_instance instance
    else
      Lwt.return instance )
    >>= function
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
          with_crash_record_exn source (fun () -> f env) >|= fun (env, result) ->
          (Watchman_alive env, result))
        ~catch:(fun exn ->
          let close_channel_on_instance' env = close_channel_on_instance env >>= on_dead' on_dead in
          match Exception.unwrap exn with
          | Sys_error msg when msg = "Broken pipe" ->
            Hh_logger.log "Watchman Pipe broken.";
            close_channel_on_instance' env
          | Sys_error msg when msg = "Connection reset by peer" ->
            Hh_logger.log "Watchman connection reset by peer.";
            close_channel_on_instance' env
          | Sys_error msg when msg = "Bad file descriptor" ->
            (* This happens when watchman is tearing itself down after we
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
            on_dead' on_dead (dead_env_from_alive env)
          | End_of_file ->
            Hh_logger.log "Watchman connection End_of_file. Closing channel";
            close_channel_on_instance' env
          | Read_payload_too_long ->
            Hh_logger.log "Watchman reading payload too long. Closing channel";
            close_channel_on_instance' env
          | Timeout ->
            Hh_logger.log "Watchman reading Timeout. Closing channel";
            close_channel_on_instance' env
          | Watchman_error msg ->
            Hh_logger.log "Watchman error: %s. Closing channel" msg;
            close_channel_on_instance' env
          | _ ->
            let msg = Exception.to_string exn in
            EventLogger.watchman_uncaught_failure msg;
            raise Exit_status.(Exit_with Watchman_failed))
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
      let files = set_of_list @@ extract_file_names env data in
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
          env.clockspec <- J.get_string_val "clock" data;
          assert_no_fresh_instance data;
          (try (env, make_state_change_response `Enter (J.get_string_val "state-enter" data) data)
           with Caml.Not_found ->
             (try (env, make_state_change_response `Leave (J.get_string_val "state-leave" data) data)
              with Caml.Not_found ->
                (env, Files_changed (set_of_list @@ extract_file_names env data))))
      end

  let get_changes ?deadline instance =
    call_on_instance
      instance
      "get_changes"
      ~on_dead:(fun _ -> Watchman_unavailable)
      ~on_alive:(fun env ->
        let timeout =
          Base.Option.map deadline (fun deadline ->
              let timeout = deadline -. Unix.time () in
              max timeout 0.0)
        in
        let debug_logging = env.settings.debug_logging in
        if env.settings.subscribe_mode <> None then
          blocking_read ~debug_logging ~timeout ~conn:env.conn >|= fun response ->
          let (env, result) = transform_asynchronous_get_changes_response env response in
          (env, Watchman_pushed result)
        else
          let query = since_query env in
          request ~debug_logging ~conn:env.conn ~timeout query >|= fun response ->
          let (env, changes) = transform_asynchronous_get_changes_response env (Some response) in
          (env, Watchman_synchronous [changes]))

  let get_changes_since_mergebase ~timeout env =
    request
      ~timeout
      ~debug_logging:env.settings.debug_logging
      (get_changes_since_mergebase_query env)
    >|= extract_file_names env

  let get_mergebase ~timeout instance =
    call_on_instance
      instance
      "get_mergebase"
      ~on_dead:(fun _dead_env -> Error "Failed to connect to Watchman to get mergebase")
      ~on_alive:(fun env ->
        request
          ~timeout
          ~debug_logging:env.settings.debug_logging
          (get_changes_since_mergebase_query env)
        >|= fun response ->
        match extract_mergebase response with
        | Some (_clock, mergebase) -> (env, Ok mergebase)
        | None -> (env, Error "Failed to extract mergebase from response"))

  let conn_of_instance = function
    | Watchman_dead _ -> None
    | Watchman_alive { conn; _ } -> Some conn

  module Testing = struct
    open Watchman_sig.Types

    let test_settings =
      {
        subscribe_mode = Some Defer_changes;
        expression_terms = [];
        debug_logging = false;
        roots = [Path.dummy_path];
        subscription_prefix = "dummy_prefix";
      }

    let get_test_conn () =
      let%lwt reader = Buffered_line_reader_lwt.get_null_reader ()
      and oc = Lwt_io.open_file ~mode:Lwt_io.output "/dev/null" in
      Lwt.return (reader, oc)

    let get_test_env () =
      get_test_conn () >|= fun conn ->
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
end

include Watchman_actual
