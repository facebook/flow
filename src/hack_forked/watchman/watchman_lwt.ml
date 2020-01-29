(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Lwt_watchman_process : Watchman_sig.WATCHMAN_PROCESS with type 'a result = 'a Lwt.t = struct
  include Watchman_sig.Types
  include Watchman.Watchman_process_helpers

  type 'a result = 'a Lwt.t

  type conn = Buffered_line_reader_lwt.t * Lwt_io.output_channel

  let ( >>= ) = Lwt.( >>= )

  let ( >|= ) = Lwt.( >|= )

  let return = Lwt.return

  let spf = Printf.sprintf

  let catch ~f ~catch =
    Lwt.catch f (fun exn ->
        let e = Exception.wrap exn in
        match exn with
        | Lwt.Canceled -> Exception.reraise e
        | _ -> catch e)

  let with_timeout timeout f =
    match timeout_to_secs timeout with
    | None -> f ()
    | Some timeout ->
      (try%lwt Lwt_unix.with_timeout timeout f with Lwt_unix.Timeout -> raise Timeout)

  let list_fold_values l ~init ~f = Lwt_list.fold_left_s f init l

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
  let rec request ~debug_logging ?conn ?(timeout = Default_timeout) json =
    match conn with
    | None -> with_watchman_conn ~timeout (fun conn -> request ~debug_logging ~conn ~timeout json)
    | Some (reader, oc) ->
      let%lwt () = send_request ~debug_logging oc json in
      let%lwt line =
        with_timeout timeout @@ fun () -> Buffered_line_reader_lwt.get_next_line reader
      in
      Lwt.return @@ sanitize_watchman_response ~debug_logging line

  let send_request_and_do_not_wait_for_response ~debug_logging ~conn:(_, oc) json =
    send_request ~debug_logging oc json

  let has_input ~timeout reader =
    let fd = Buffered_line_reader_lwt.get_fd reader in
    match timeout_to_secs timeout with
    | None -> Lwt.return @@ Lwt_unix.readable fd
    | Some timeout ->
      (try%lwt
         Lwt_unix.with_timeout timeout @@ fun () ->
         let%lwt () = Lwt_unix.wait_read fd in
         Lwt.return true
       with Lwt_unix.Timeout -> Lwt.return false)

  let blocking_read ~debug_logging ?(timeout = No_timeout) ~conn:(reader, _) =
    let%lwt ready = has_input ~timeout reader in
    if not ready then
      match timeout with
      | No_timeout -> Lwt.return None
      | _ -> raise Timeout
    else
      let%lwt output =
        try%lwt
          Lwt_unix.with_timeout 40.0 @@ fun () -> Buffered_line_reader_lwt.get_next_line reader
        with Lwt_unix.Timeout ->
          let () = Hh_logger.log "Lwt_watchman_process.blocking_read timed out" in
          raise Read_payload_too_long
      in
      Lwt.return @@ Some (sanitize_watchman_response ~debug_logging output)

  module Testing = struct
    let get_test_conn () =
      let%lwt reader = Buffered_line_reader_lwt.get_null_reader ()
      and oc = Lwt_io.open_file ~mode:Lwt_io.output "/dev/null" in
      Lwt.return (reader, oc)
  end
end

module Watchman_actual = struct
  include Watchman.Functor (Lwt_watchman_process)
end

module Watchman_mock = struct
  exception Not_available_in_mocking

  type 'a result = 'a Lwt.t

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

    let init_returns v = init := v

    let changes = ref Watchman_unavailable

    let get_changes_returns v = changes := v
  end

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

    let get_test_env () = Lwt.return "test_env"

    let transform_asynchronous_get_changes_response _ _ = raise Not_available_in_mocking
  end

  let init ?since_clockspec:_ _ () = Lwt.return !Mocking.init

  let get_changes ?deadline instance =
    let _ = deadline in
    let result = !Mocking.changes in
    Mocking.changes := Watchman_unavailable;
    Lwt.return (instance, result)

  let conn_of_instance _ = None

  let get_changes_since_mergebase ?timeout:_ _ = Lwt.return []

  let get_mergebase ?timeout:_ instance = Lwt.return (instance, Ok "mergebase")

  let close _ = Lwt.return ()

  let with_instance instance ~try_to_restart:_ ~on_alive ~on_dead =
    match instance with
    | Watchman_dead dead_env -> on_dead dead_env
    | Watchman_alive env -> on_alive env
end

module type S = sig
  include Watchman_sig.S with type 'a result = 'a Lwt.t
end

include ( val if Injector_config.use_test_stubbing then
                (module Watchman_mock : S)
              else
                (module Watchman_actual : S) )
