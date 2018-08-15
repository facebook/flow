(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 **)

open Hh_core
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

(** Stuff shared between Actual and Mocked implementations. *)
module Testing_common = struct
  open Watchman_sig.Types
  let test_settings = {
    subscribe_mode = Some Defer_changes;
    init_timeout = 0;
    expression_terms = [];
    debug_logging = false;
    roots = [Path.dummy_path];
    subscription_prefix = "dummy_prefix"
  }
end

module Watchman_process_helpers = struct
  include Watchman_sig.Types
  module J = Hh_json_helpers.AdhocJsonHelpers

  let debug = false

  (** Throw this exception when we know there is something to read from
   * the watchman channel, but reading took too long. *)
  exception Read_payload_too_long

  (* Looks for common errors in watchman responses *)
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
    with Not_found -> ());
    (try
      let canceled = J.get_bool_val "canceled" obj in
      if canceled then begin
      EventLogger.watchman_error "Subscription canceled by watchman";
      raise @@ Subscription_canceled_by_watchman
      end else ()
    with Not_found -> ())

  (* Verifies that a watchman response is valid JSON and free from common errors *)
  let sanitize_watchman_response ~debug_logging output =
    if debug_logging then Hh_logger.info "Watchman response: %s" output;
    let response =
    try Hh_json.json_of_string output
    with e ->
      Hh_logger.error ~exn:e "Failed to parse string as JSON: %s" output;
      raise e
    in
    assert_no_error response;
    response
end

module Regular_watchman_process: sig
  include Watchman_sig.WATCHMAN_PROCESS with type 'a result = 'a

  val get_reader: conn -> Buffered_line_reader.t
end = struct
  include Watchman_process_helpers

  type 'a result = 'a
  type conn = Buffered_line_reader.t * out_channel

  let (>>=) a f = f a
  let (>|=) a f = f a
  let return x = x

  let catch ~f ~catch = try f () with exn -> catch exn

  let map_fold_values map ~init ~f = SMap.fold (fun _ v acc -> f v acc) map init
  let map_iter_values_s map ~f = SMap.iter (fun _ v -> f v) map

  (* Send a request to the watchman process *)
  let send_request ~debug_logging oc json =
    let json_str = Hh_json.(json_to_string json) in
    if debug_logging then Hh_logger.info "Watchman request: %s" json_str ;
    output_string oc json_str;
    output_string oc "\n";
    flush oc

  (***************************************************************************)
  (* Handling requests and responses. *)
  (***************************************************************************)

  let has_input timeout reader =
    if Buffered_line_reader.has_buffered_content reader
    then true
    else
      match Sys_utils.select_non_intr [Buffered_line_reader.get_fd reader] [] [] timeout with
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
      Timeout.with_timeout
        ~do_: (fun _ -> Buffered_line_reader.get_next_line reader)
        ~timeout
        ~on_timeout:(fun () ->
          let () = EventLogger.watchman_timeout () in
          raise Read_payload_too_long
        )

  (* Asks watchman for the path to the socket file *)
  let get_sockname timeout =
    let ic =
      Timeout.open_process_in "watchman"
      [| "watchman"; "get-sockname"; "--no-pretty" |] in
    let reader = Buffered_line_reader.create @@
      Timeout.descr_of_in_channel ic in
    let output = read_with_timeout timeout reader in
    assert (Timeout.close_process_in ic = Unix.WEXITED 0);
    let json = Hh_json.json_of_string output in
    J.get_string_val "sockname" json

  (* Opens a connection to the watchman process through the socket *)
  let open_connection ~timeout =
    let sockname = get_sockname timeout in
    let (tic, oc) = Timeout.open_connection (Unix.ADDR_UNIX sockname) in
    let reader = Buffered_line_reader.create
      @@ Timeout.descr_of_in_channel @@ tic in
    reader, oc

  let close_connection conn =
    let reader, _ = conn in
    Unix.close @@ Buffered_line_reader.get_fd reader

  (** Open a connection to the watchman socket, call the continuation, then
    * close. *)
  let with_watchman_conn ~timeout f =
    let conn = open_connection ~timeout in
    let result = try f conn with
      | e ->
        Unix.close @@ Buffered_line_reader.get_fd @@ fst conn;
        raise e
    in
    Unix.close @@ Buffered_line_reader.get_fd @@ fst conn;
    result

  (* Sends a request to watchman and returns the response. If we don't have a connection,
   * a new connection will be created before the request and destroyed after the response *)
  let rec request ~debug_logging ?conn ?(timeout=120.0) json =
    match conn with
    | None ->
      with_watchman_conn ~timeout (fun conn -> request ~debug_logging ~conn ~timeout json)
    | Some (reader, oc) -> begin
      send_request ~debug_logging oc json;
      sanitize_watchman_response ~debug_logging (read_with_timeout timeout reader)
    end

  let send_request_and_do_not_wait_for_response ~debug_logging ~conn:(_, oc) json =
    send_request ~debug_logging oc json

  let blocking_read ~debug_logging ?timeout ~conn =
    let timeout = Option.value timeout ~default:0.0 in
    let ready = has_input timeout @@ fst conn in
    if not ready then
      if timeout = 0.0 then None
      else raise Timeout
    else
      (* Use the timeout mechanism to limit maximum time to read payload (cap
       * data size) so we don't freeze if watchman sends an inordinate amount of
       * data, or if it is malformed (i.e. doesn't end in a newline). *)
      let timeout = 40 in
      let output = Timeout.with_timeout
        ~do_: (fun _ -> Buffered_line_reader.get_next_line @@ fst conn)
        ~timeout
        ~on_timeout:begin fun () ->
          let () = Hh_logger.log "Regular_watchman_process.blocking_read timed out" in
          raise Read_payload_too_long
        end
      in
      Some (sanitize_watchman_response ~debug_logging output)

  let get_reader (reader, _) = reader

  module Testing = struct
    let get_test_conn () = (Buffered_line_reader.get_null_reader (), open_out "/dev/null")
  end
end

module Functor (Watchman_process: Watchman_sig.WATCHMAN_PROCESS): Watchman_sig.S
  with type 'a result = 'a Watchman_process.result and type conn = Watchman_process.conn =
struct
  let (>>=) = Watchman_process.(>>=)
  let (>|=) = Watchman_process.(>|=)

  type 'a result = 'a Watchman_process.result
  type conn = Watchman_process.conn

  (**
   * Ocaml module signatures are static. This means since mocking
   * functionality is exposed in the mli, it must be implemented for
   * both actual builds as well as testing/mocked builds.
   *
   * For actual builds, we just raise exceptions for the
   * setters.
   *)
  module Mocking = struct
    exception Cannot_set_when_mocks_disabled

    let print_env _ =
      raise Cannot_set_when_mocks_disabled
    let get_changes_returns _ =
      raise Cannot_set_when_mocks_disabled
    let init_returns _ =
      raise Cannot_set_when_mocks_disabled
  end

  (** This number is totally arbitrary. Just need some cap. *)
  let max_reinit_attempts = 8

  (**
   * Triggers watchman to flush buffered updates on this subscription.
   * See watchman/docs/cmd/flush-subscriptions.html
   *)
  let flush_subscriptions_cmd = "cmd-flush-subscriptions"

  include Watchman_sig.Types

  type watched_path = {
    subscription_name: string;
    path: Path.t;
    relative_path: string;
    (* See https://facebook.github.io/watchman/docs/clockspec.html
     *
     * This is also used to reliably detect a crashed watchman. Watchman has a
     * facility to detect watchman process crashes between two "since" queries
     *
     * See also assert_no_fresh_instance *)
    mutable clockspec: string;
  }

  type resume_watches =
    | Prior_watched_paths of watched_path SMap.t
    | New_watch_from_clockspec of string

  type dead_env = {
    (** Will reuse original settings to reinitializing watchman subscription. *)
    prior_settings : init_settings;
    reinit_attempts: int;
    dead_since: float;
    prior_watched_paths: watched_path SMap.t;
  }

  type env = {
    settings : init_settings;
    conn: Watchman_process.conn;
    watch_root: string;
    watched_paths: watched_path SMap.t;
  }

  let dead_env_from_alive env =
    {
      prior_settings = env.settings;
      dead_since = Unix.time ();
      reinit_attempts = 0;
      (** When we start a new watchman connection, we continue to use the prior
       * clockspec for each watched root. If the same watchman server is still alive, then all is
       * good. If not, the clockspec allows us to detect whether a new watchman
       * server had to be started. See also "is_fresh_instance" on watchman's
       * "since" response. *)
      prior_watched_paths = env.watched_paths;
    }

  type watchman_instance =
    (** Indicates a dead watchman instance (most likely due to chef upgrading,
     * reconfiguration, or a user terminating watchman, or a timeout reading
     * from the connection) detected by, for example, a pipe error or a timeout.
     *
     * TODO: Currently fallback to a Watchman_dead is only handled in calls
     * wrapped by the with_crash_record. Pipe errors elsewhere (for example
     * during request) will still result in Hack exiting. Need to cover those
     * cases too. *)
    | Watchman_dead of dead_env
    | Watchman_alive of env

  module J = Hh_json_helpers.AdhocJsonHelpers

  (****************************************************************************)
  (* JSON methods. *)
  (****************************************************************************)

  let clock root = J.strlist ["clock"; root]

  type watch_command = Subscribe | Query

  (** Conjunction of extra_expressions and expression_terms. *)
  let request_json
      ?(extra_kv=[]) ?(extra_expressions=[]) ~watched_path watchman_command env =
    let open Hh_json in
    let command = begin match watchman_command with
      | Subscribe -> "subscribe"
      | Query -> "query" end in
    let header =
      [JSON_String command ; JSON_String env.watch_root] @
        begin
          match watchman_command with
          | Subscribe -> [JSON_String watched_path.subscription_name]
          | _ -> []
        end in
    let expressions = extra_expressions @ (env.settings.expression_terms) in
    let directives = [
      JSON_Object (extra_kv
      @ [
        "fields", J.strlist ["name"];
        "relative_root", JSON_String watched_path.relative_path;]
      (** Watchman doesn't allow an empty allof expression. Omit if empty. *)
      @ if expressions = [] then [] else [
        "expression", J.pred "allof" expressions
      ])
    ] in
    let request = JSON_Array (header @ directives) in
    request

  let all_query ~watched_path env =
    request_json
      ~extra_expressions:([Hh_json.JSON_String "exists"])
      ~watched_path
      Query env

  let since_query ~watched_path env =
    request_json
      ~extra_kv: ["since", Hh_json.JSON_String watched_path.clockspec;
                  "empty_on_fresh_instance", Hh_json.JSON_Bool true]
      ~watched_path
      Query env

  let subscribe ~mode ~watched_path env =
    let since, mode = match mode with
    | All_changes -> Hh_json.JSON_String watched_path.clockspec, []
    | Defer_changes ->
      Hh_json.JSON_String watched_path.clockspec, ["defer", J.strlist ["hg.update"]]
    | Drop_changes -> Hh_json.JSON_String watched_path.clockspec, ["drop", J.strlist ["hg.update"]]
    | Scm_aware ->
        Hh_logger.log "Making Scm_aware subscription";
        let scm = Hh_json.JSON_Object
          [("mergebase-with", Hh_json.JSON_String "master")] in
        let since = Hh_json.JSON_Object
          [("scm", scm); ("drop", J.strlist ["hg.update"]);] in
        since, []
    in
    request_json
      ~extra_kv:((["since", since] @ mode) @
                 ["empty_on_fresh_instance",
                 Hh_json.JSON_Bool true])
      ~watched_path
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
         raise Exit_status.(Exit_with Watchman_fresh_instance)
       end
       else
         Ok ((), trace)
      ) in
    ()

  (****************************************************************************)
  (* Initialization, reinitialization, and crash-tracking. *)
  (****************************************************************************)

  let with_crash_record_exn source f =
    Watchman_process.catch ~f ~catch:(fun e ->
      Hh_logger.exc ~prefix:("Watchman " ^ source ^ ": ") e;
      raise e
    )

  let with_crash_record_opt source f =
    Watchman_process.catch
      ~f:(fun () -> with_crash_record_exn source f >|= fun v -> Some v)
      ~catch:(fun _ -> Watchman_process.return None)

  let has_capability name capabilities =
    (** Projects down from the boolean error monad into booleans.
     * Error states go to false, values are projected directly. *)
    let project_bool m = match m with
      | Ok (v, _) ->
        v
      | Error _ ->
        false
    in
    let open Hh_json.Access in
    (return capabilities)
      >>= get_obj "capabilities"
      >>= get_bool name
      |> project_bool

  let re_init ?resume_watches
    { init_timeout; subscribe_mode; expression_terms; debug_logging; roots; subscription_prefix } =

    (* Dedupe roots *)
    let roots_with_clockspecs = match resume_watches with
    | None ->
      (* Dedupe roots & add in that we have no prior clockspec *)
      List.fold_left roots
        ~init:SMap.empty
        ~f:(fun map path -> SMap.add (Path.to_string path) (path, None) map)
    | Some (New_watch_from_clockspec clockspec) ->
      (* Dedupe roots & add clockspec for resuming watch. *)
      List.fold_left roots
        ~init:SMap.empty
        ~f:(fun map path -> SMap.add (Path.to_string path) (path, Some clockspec) map)
    | Some (Prior_watched_paths watched_paths) ->
      SMap.fold
        (fun _ { path; clockspec; _; } -> SMap.add (Path.to_string path) (path, Some clockspec))
        watched_paths
        SMap.empty
    in
    if SMap.cardinal roots_with_clockspecs = 0
    then failwith "Cannot run watchman with fewer than 1 root";

    with_crash_record_opt "init" @@ fun () ->
    Watchman_process.open_connection ~timeout:(float_of_int init_timeout) >>= fun conn ->
    Watchman_process.request ~debug_logging ~conn
      (capability_check ~optional:[ flush_subscriptions_cmd ]
      ["relative_root"]) >>= fun capabilities ->
    let supports_flush = has_capability flush_subscriptions_cmd capabilities in
    (** Disable subscribe if Watchman flush feature isn't supported. *)
    let subscribe_mode = if supports_flush then subscribe_mode else None in

    Watchman_process.map_fold_values roots_with_clockspecs
      ~init:(SMap.empty, SSet.empty)
      ~f: (fun (path, prior_clockspec) (watched_paths, watch_roots) ->
        (* Watch this root *)
        Watchman_process.request
          ~debug_logging ~conn (watch_project (Path.to_string path)) >>= fun response ->
        let watch_root = J.get_string_val "watch" response in
        let relative_path = J.get_string_val "relative_path" ~default:"" response in

        (* If we don't have a prior clockspec, grab the current clock *)
        (match prior_clockspec with
          | Some s -> Watchman_process.return s
          | None ->
            Watchman_process.request ~debug_logging ~conn (clock watch_root)
            >|= J.get_string_val "clock"
        ) >|= fun clockspec ->

        (* We may not end up subscribing, but if we do this will be the name *)
        let subscription_name = spf "%s:%s" subscription_prefix relative_path in
        let watched_path = { subscription_name; path; relative_path; clockspec } in
        let watched_paths = SMap.add subscription_name watched_path watched_paths in

        let watch_roots = SSet.add watch_root watch_roots in
        watched_paths, watch_roots
      )
    >>= fun (watched_paths, watch_roots) ->

    (* All of our watched paths should have the same watch root. Let's assert that *)
    let watch_root = match SSet.elements watch_roots with
    | [] -> failwith "Unreachable - we already asserted at least 1 root"
    | [watch_root] -> watch_root
    | _ ->
      failwith (
        spf "Can't watch paths across multiple Watchman watch_roots. Found %d watch_roots"
          (SSet.cardinal watch_roots)
      )
    in

    let env = {
      settings = {
        init_timeout;
        debug_logging;
        subscribe_mode;
        expression_terms;
        roots;
        subscription_prefix;
      };
      conn;
      watch_root;
      watched_paths;
    } in
    (match subscribe_mode with
    | None -> Watchman_process.return ()
    | Some mode ->
      Watchman_process.map_iter_values_s env.watched_paths ~f:(fun watched_path ->
        Watchman_process.request ~debug_logging ~conn (subscribe ~mode ~watched_path env) >|= ignore
      )
    ) >|= fun () ->
    env

  let init ?since_clockspec settings () =
    let resume_watches = Option.map since_clockspec ~f:(fun clockspec -> New_watch_from_clockspec clockspec) in
    re_init ?resume_watches settings

  let extract_file_names ~watched_path env json =
    let files = try J.get_array_val "files" json with
      (** When an hg.update happens, it shows up in the watchman subscription
       * as a notification with no files key present. *)
      | Not_found -> []
    in
    let files = List.map files begin fun json ->
      let s = Hh_json.get_string_exn json in
      let abs =
        Filename.concat env.watch_root @@
        Filename.concat watched_path.relative_path s in
      abs
    end in
    files

  let within_backoff_time attempts time =
    let offset = 4.0 *. (2.0 ** float (if attempts > 3 then 3 else attempts)) in
    (Unix.time ()) >= time +. offset

  let maybe_restart_instance instance = match instance with
    | Watchman_alive _ -> Watchman_process.return instance
    | Watchman_dead dead_env ->
      if dead_env.reinit_attempts >= max_reinit_attempts then
        let () = Hh_logger.log
          "Ran out of watchman reinit attempts. Exiting." in
        raise Exit_status.(Exit_with Watchman_failed)
      else if within_backoff_time dead_env.reinit_attempts dead_env.dead_since
      then
        let () =
          Hh_logger.log "Attemping to reestablish watchman subscription" in
        re_init ~resume_watches:(Prior_watched_paths dead_env.prior_watched_paths)
          dead_env.prior_settings
        >|= function
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
        Watchman_process.return instance

  let close_channel_on_instance env =
    Watchman_process.close_connection env.conn >|= fun () ->
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
    maybe_restart_instance instance >>= fun instance ->
    match instance with
    | Watchman_dead _ ->
      Watchman_process.return (instance, Watchman_unavailable)
    | Watchman_alive env -> begin
      Watchman_process.catch
        ~f:(fun () ->
          with_crash_record_exn source (fun () -> f env) >|= fun (env, result) ->
          Watchman_alive env, result
        )
        ~catch:(function
          | Sys_error msg when msg = "Broken pipe" ->
            Hh_logger.log "Watchman Pipe broken.";
            close_channel_on_instance env
          | Sys_error msg when msg = "Connection reset by peer" ->
            Hh_logger.log "Watchman connection reset by peer.";
            close_channel_on_instance env
          | Sys_error msg when msg = "Bad file descriptor" ->
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
            Watchman_process.return (Watchman_dead (dead_env_from_alive env), Watchman_unavailable)
          | End_of_file ->
            Hh_logger.log "Watchman connection End_of_file. Closing channel";
            close_channel_on_instance env
          | Watchman_process.Read_payload_too_long ->
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
            raise Exit_status.(Exit_with Watchman_failed)
        )
    end

  (** This is a large >50MB payload, which could longer than 2 minutes for
   * Watchman to generate and push down the channel. *)
  let get_all_files env =
    Watchman_process.catch
      ~f:(fun () ->
        with_crash_record_exn "get_all_files" @@ fun () ->
          Watchman_process.map_fold_values
            env.watched_paths
            ~init:SSet.empty
            ~f:(fun watched_path set ->
              Watchman_process.request
                ~debug_logging:env.settings.debug_logging
                (all_query ~watched_path env)
              >|= fun response ->
                watched_path.clockspec <- J.get_string_val "clock" response;
                extract_file_names ~watched_path env response
                |> List.fold ~init:set ~f:(fun set file -> SSet.add file set)
            )
          >|= SSet.elements
      )
      ~catch:(fun _ -> raise Exit_status.(Exit_with Watchman_failed))

  let transform_synchronous_get_changes_response ~watched_path env data =
    watched_path.clockspec <- J.get_string_val "clock" data;
    assert_no_fresh_instance data;
    env, set_of_list @@ extract_file_names ~watched_path env data

  let make_state_change_response state name data =
    let metadata = J.try_get_val "metadata" data in
    match state with
    | `Enter ->
      State_enter (name, metadata)
    | `Leave ->
      State_leave (name, metadata)

  let make_mergebase_changed_response ~watched_path env data =
    let open Hh_json.Access in
    let accessor = return data in
    accessor >>=
      get_obj "clock" >>=
      get_string "clock" >>= fun (clock, _) ->
    accessor >>= get_obj "clock" >>=
      get_obj "scm" >>=
      get_string "mergebase" >>= fun (mergebase, keytrace) ->
    let files = set_of_list @@ extract_file_names ~watched_path env data in
    watched_path.clockspec <- clock;
    let response = Changed_merge_base (mergebase, files, clock) in
    Ok ((env, response), keytrace)

  let get_watched_path_for_response env data =
    let subscription_name =
      try J.get_string_val "subscription" data
      with Not_found ->
        failwith (
          spf
            "Malformed response from watchman missing 'subscription' field:\n%s"
            (Hh_json.json_to_string ~pretty:true data)
        )
    in
    try SMap.find_unsafe subscription_name env.watched_paths
    with Not_found -> failwith (spf "Unknown watchman subscription '%s'" subscription_name)

  let transform_asynchronous_get_changes_response env data = match data with
    | None ->
      env, Files_changed (SSet.empty)
    | Some data -> begin
      let watched_path = get_watched_path_for_response env data in

      match make_mergebase_changed_response ~watched_path env data with
      | Ok ((env, response), _) -> env, response
      | Error _ ->
        watched_path.clockspec <- J.get_string_val "clock" data;
        assert_no_fresh_instance data;
        try env, make_state_change_response `Enter
          (J.get_string_val "state-enter" data) data with
        | Not_found ->
        try env, make_state_change_response `Leave
          (J.get_string_val "state-leave" data) data with
        | Not_found ->
          env, Files_changed (set_of_list @@ extract_file_names ~watched_path env data)
    end

  let get_changes ?deadline instance =
    let timeout = Option.map deadline ~f:(fun deadline ->
      let timeout = deadline -. (Unix.time ()) in
      max timeout 0.0
    ) in
    call_on_instance instance "get_changes" @@ fun env ->
      let debug_logging = env.settings.debug_logging in
      if env.settings.subscribe_mode <> None
      then
        Watchman_process.blocking_read ~debug_logging ?timeout ~conn:env.conn >|= fun response ->
        let env, result = transform_asynchronous_get_changes_response env response in
        env, Watchman_pushed result
      else
        Watchman_process.map_fold_values
          env.watched_paths
          ~init:(env, SSet.empty)
          ~f:(fun watched_path (env, files) ->
            let query = since_query ~watched_path env in
            Watchman_process.request
              ~debug_logging ~conn:env.conn ?timeout query
            >|= fun response ->
              let env, files' =
                transform_synchronous_get_changes_response ~watched_path env response
              in
              env, SSet.union files files'
          )
        >|= fun (env, result) -> env, Watchman_synchronous result

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
    let is_finished_flush_response json = match json with
      | None -> false
      | Some json -> begin
        let open Hh_json.Access in
        let synced = (return json) >>= get_array "synced" |> function
          | Error _ -> SSet.empty
          | Ok (vs, _) -> List.map vs ~f:Hh_json.get_string_exn |> SSet.of_list
        in
        let not_needed = (return json) >>= get_array "no_sync_needed" |> function
          | Error _ -> SSet.empty
          | Ok (vs, _) -> List.map vs ~f:Hh_json.get_string_exn |> SSet.of_list
        in
        (* All the subscriptions which we need to sync *)
        let to_sync = env.watched_paths |> SMap.bindings |> List.map ~f:fst |> SSet.of_list in
        (* Return true if all subs have been sync'd or don't need to be sync'd *)
        SSet.diff (SSet.diff to_sync synced) not_needed |> SSet.is_empty
      end
    in
    let timeout = deadline -. Unix.time () in
    if timeout < 0.0 then raise Timeout else ();

    let debug_logging = env.settings.debug_logging in
    Watchman_process.blocking_read ~debug_logging ~timeout ~conn:env.conn >>= fun json ->

    if is_finished_flush_response json
    then Watchman_process.return (env, acc)
    else
      let env, result = match json with
      | None -> env, SSet.empty
      | Some json ->
        let watched_path = get_watched_path_for_response env json in
        transform_synchronous_get_changes_response ~watched_path env json
      in
      poll_until_sync ~deadline env (SSet.union result acc)

  let poll_until_sync ~deadline env =
    poll_until_sync ~deadline env SSet.empty

  let get_changes_synchronously ~(timeout:int) instance =
    call_on_instance instance "get_changes_synchronously"
      @@ (fun env ->
        if env.settings.subscribe_mode = None
        then
          Watchman_process.map_fold_values
            env.watched_paths
            ~init:(env, SSet.empty)
            ~f:(fun watched_path (env, files) ->
              let timeout = float_of_int timeout in
              let query = since_query ~watched_path env in
              Watchman_process.request
                ~debug_logging:env.settings.debug_logging
                ~conn:env.conn ~timeout query
              >|= fun response ->
                let env, files' =
                  transform_synchronous_get_changes_response ~watched_path env response
                in
                env, SSet.union files files'
            )
          >|= fun (env, files) -> env, Watchman_synchronous files
        else
          let request = flush_request ~timeout env.watch_root in
          let conn = env.conn in
          Watchman_process.send_request_and_do_not_wait_for_response
            ~debug_logging:env.settings.debug_logging ~conn request >>= fun () ->
          let deadline = Unix.time () +. (float_of_int timeout) in
          poll_until_sync ~deadline env >|= fun (env, files) ->
          env, Watchman_synchronous files
      )
    >|= function
    | _, Watchman_unavailable ->
      raise (Watchman_error "Watchman unavailable for synchronous response")
    | _, Watchman_pushed _ ->
      raise (Watchman_error "Wtf? pushed response from synchronous request")
    | instance, Watchman_synchronous files ->
      instance, files

  let conn_of_instance = function
    | Watchman_dead _ -> None
    | Watchman_alive {conn; _} -> Some conn

  module Testing = struct
    include Testing_common

    let get_test_env () =
      Watchman_process.Testing.get_test_conn () >|= fun conn ->
      {
        settings = test_settings;
        conn;
        watch_root = "/path/to/root";
        watched_paths =
          SMap.add "mysubscriptionname" {
            subscription_name = "mysubscriptionname";
            path = Path.dummy_path;
            relative_path = "";
            clockspec = "";
          } SMap.empty
      }

    let transform_asynchronous_get_changes_response env json =
      transform_asynchronous_get_changes_response env json
  end

end;;

module Watchman_actual = struct
  include Functor (Regular_watchman_process)

  let get_reader instance =
    Option.map (conn_of_instance instance) ~f:Regular_watchman_process.get_reader
end

module Watchman_mock = struct

  exception Not_available_in_mocking

  type 'a result = 'a
  type conn

  include Watchman_sig.Types
  type env = string
  type dead_env = unit
  type watchman_instance =
    | Watchman_dead of dead_env
    | Watchman_alive of env

  module Mocking = struct
    let print_env env = env

    let init = ref None
    let init_returns v =
      init := v

    let changes = ref Watchman_unavailable
    let get_changes_returns v =
      changes := v

    let changes_synchronously = ref SSet.empty

    let all_files = ref []

  end

  module Testing = struct
    include Testing_common
    let get_test_env () = "test_env"
    let transform_asynchronous_get_changes_response _ _ =
      raise Not_available_in_mocking

  end

  let init ?since_clockspec:_ _ () = !Mocking.init
  let get_changes ?deadline instance =
    let _ = deadline in
    let result = !Mocking.changes in
    Mocking.changes := Watchman_unavailable;
    instance, result

  let get_changes_synchronously ~timeout instance =
    let _ = timeout in
    let result = !Mocking.changes_synchronously in
    Mocking.changes_synchronously := SSet.empty;
    instance, result

  let get_reader _ = None

  let conn_of_instance _ = None

  let get_all_files _ =
    let result = !Mocking.all_files in
    Mocking.all_files := [];
    result

end

module type S = sig
  include Watchman_sig.S with type 'a result = 'a

  val get_reader: watchman_instance -> Buffered_line_reader.t option
end

include (val (if Injector_config.use_test_stubbing
  then (module Watchman_mock : S)
  else (module Watchman_actual : S)
))
