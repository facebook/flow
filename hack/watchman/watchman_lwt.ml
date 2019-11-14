(*
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module Lwt_watchman_process :
  Watchman_sig.WATCHMAN_PROCESS with type 'a result = 'a Lwt.t = struct
  include Watchman_sig.Types
  include Watchman.Watchman_process_helpers

  type 'a result = 'a Lwt.t

  type conn = Buffered_line_reader_lwt.t * Lwt_io.output_channel

  let ( >>= ) = Lwt.( >>= )

  let ( >|= ) = Lwt.( >|= )

  let return = Lwt.return

  external reraise : exn -> 'a = "%reraise"

  let catch ~f ~catch =
    Lwt.catch f (fun e ->
        match e with
        | Lwt.Canceled -> reraise e
        | e -> catch ~stack:(Printexc.get_backtrace ()) e)

  let list_fold_values l ~init ~f = Lwt_list.fold_left_s f init l

  (* Send a request to the watchman process *)
  let send_request ~debug_logging oc json =
    let json_str = Hh_json.(json_to_string json) in
    if debug_logging then Hh_logger.info "Watchman request: %s" json_str;

    (* Print the json with a newline and then flush *)
    let%lwt () = Lwt_io.fprintl oc json_str in
    Lwt_io.flush oc

  let get_sockname timeout =
    let process =
      Lwt_process.open_process_in
        ("", [| "watchman"; "--no-pretty"; "get-sockname" |])
    in
    let%lwt output =
      match timeout_to_secs timeout with
      | None -> Lwt_io.read_line process#stdout
      | Some timeout ->
        (try%lwt
           Lwt_unix.with_timeout timeout @@ fun () ->
           Lwt_io.read_line process#stdout
         with Lwt_unix.Timeout -> raise Timeout)
    in
    let%lwt status = process#close in
    assert (status = Unix.WEXITED 0);
    let json = Hh_json.json_of_string output in
    Lwt.return @@ J.get_string_val "sockname" json

  (* Opens a connection to the watchman process through the socket *)
  let open_connection ~timeout =
    let%lwt sockname = get_sockname timeout in
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
    try%lwt Lwt_io.close oc
    with Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit

  let with_watchman_conn ~timeout f =
    let%lwt conn = open_connection ~timeout in
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
    | None ->
      with_watchman_conn ~timeout (fun conn ->
          request ~debug_logging ~conn ~timeout json)
    | Some (reader, oc) ->
      let%lwt () = send_request ~debug_logging oc json in
      let%lwt line =
        match timeout_to_secs timeout with
        | None -> Buffered_line_reader_lwt.get_next_line reader
        | Some timeout ->
          (try%lwt
             Lwt_unix.with_timeout timeout @@ fun () ->
             Buffered_line_reader_lwt.get_next_line reader
           with Lwt_unix.Timeout -> raise Timeout)
      in
      Lwt.return @@ sanitize_watchman_response ~debug_logging line

  let send_request_and_do_not_wait_for_response
      ~debug_logging ~conn:(_, oc) json =
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
          Lwt_unix.with_timeout 40.0 @@ fun () ->
          Buffered_line_reader_lwt.get_next_line reader
        with Lwt_unix.Timeout ->
          let () =
            Hh_logger.log "Lwt_watchman_process.blocking_read timed out"
          in
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

include Watchman.Functor (Lwt_watchman_process)
