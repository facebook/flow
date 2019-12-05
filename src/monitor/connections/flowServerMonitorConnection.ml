(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The Flow server monitor needs to deal with multiple connections. There's the connection to the
 * server (a pipe), there's 0 or more connections to ephemeral clients (over a socket), and there's
 * 0 or more connections to persistent clients (over a socket). We need to be able to read and write
 * to all of these clients in a non-blocking and race-condition free manner.
 *
 * This module wraps a connection. The basic idea is as follows:
 *
 * 1. There is a loop which reads from the connection. When a message is received, we pass it to the
 *    on_read callback
 * 2. There is a stream of things to write to the connection. Other threads can add things to this
 *    stream
 * 3. There is a loop which reads from the stream and writes to the connection
 *
 * We didn't **really** need to make this a functor. CONNECTION.t could in theory be parameterized.
 * However, glevi liked writing `ServerConnection.write conn` over `Connection.write conn`. So it's
 * a minor stylistic choice.
 *)

module Logger = FlowServerMonitorLogger

module type CONNECTION_PROCESSOR = sig
  type in_message

  type out_message
end

type 'out_message command =
  | Write of 'out_message
  | WriteAndClose of 'out_message

module type CONNECTION = sig
  type t

  type in_message

  type out_message

  val create :
    name:(* A name for this connection for debugging messages *)
         string ->
    in_fd:(* The fd from which we should read *)
          Lwt_unix.file_descr ->
    out_fd:(* The fd to which we should write *)
           Lwt_unix.file_descr ->
    close:((* A function that closes the in and out fds *)
           unit -> unit Lwt.t) ->
    on_read:
      (msg:(* A callback for when we read a message from the in_fd *)
           in_message ->
      connection:t ->
      unit Lwt.t) ->
    (* Returns the tuple (start, conn), where conn is the connection and `start ()` tells the
     * connection to reading from and writing to the fds *)
    ((unit -> unit) * t) Lwt.t

  val write : msg:out_message -> t -> unit

  val write_and_close : msg:out_message -> t -> unit

  val close_immediately : t -> unit Lwt.t

  val flush_and_close : t -> unit Lwt.t

  val is_closed : t -> bool

  val wait_for_closed : t -> unit Lwt.t
end

module Make (ConnectionProcessor : CONNECTION_PROCESSOR) :
  CONNECTION
    with type in_message := ConnectionProcessor.in_message
     and type out_message := ConnectionProcessor.out_message = struct
  type t = {
    name: string;
    in_fd: Lwt_unix.file_descr;
    out_fd: Lwt_unix.file_descr;
    command_stream: ConnectionProcessor.out_message command Lwt_stream.t;
    push_to_stream: ConnectionProcessor.out_message command option -> unit;
    close: unit -> unit Lwt.t;
    on_read: msg:ConnectionProcessor.in_message -> connection:t -> unit Lwt.t;
    read_thread: unit Lwt.t;
    command_thread: unit Lwt.t;
    wait_for_closed_thread: unit Lwt.t;
  }

  let send_command conn command = conn.push_to_stream (Some command)

  let close_stream conn = (try conn.push_to_stream None with Lwt_stream.Closed -> ())

  let write ~msg conn = send_command conn (Write msg)

  let write_and_close ~msg conn =
    send_command conn (WriteAndClose msg);
    close_stream conn

  (* Doesn't actually close the file descriptors, but does stop all the loops and streams *)
  let stop_everything conn =
    close_stream conn;
    Lwt.cancel conn.read_thread;
    Lwt.cancel conn.command_thread

  let close_immediately conn =
    stop_everything conn;
    conn.close ()

  let handle_command conn = function
    | Write msg ->
      let%lwt _size = Marshal_tools_lwt.to_fd_with_preamble conn.out_fd msg in
      Lwt.return_unit
    | WriteAndClose msg ->
      Lwt.cancel conn.command_thread;
      let%lwt _size = Marshal_tools_lwt.to_fd_with_preamble conn.out_fd msg in
      close_immediately conn

  (* Write everything available in the stream and then close the connection *)
  let flush_and_close conn =
    stop_everything conn;
    close_stream conn;
    let%lwt () =
      Lwt_list.iter_s (handle_command conn) (Lwt_stream.get_available conn.command_stream)
    in
    conn.close ()

  let is_closed conn = Lwt_stream.is_closed conn.command_stream

  let wait_for_closed conn = conn.wait_for_closed_thread

  module CommandLoop = LwtLoop.Make (struct
    type acc = t

    let main conn =
      let%lwt command = Lwt_stream.next conn.command_stream in
      let%lwt () = handle_command conn command in
      Lwt.return conn

    let catch conn exn =
      match Exception.unwrap exn with
      (* The command stream has been closed. This means the command loop should gracefully exit *)
      | Lwt_stream.Empty -> Lwt.return_unit
      | _ ->
        let exn = Exception.to_exn exn in
        Logger.error
          ~exn
          "Closing connection '%s' due to uncaught exception in command loop"
          conn.name;
        close_immediately conn
  end)

  module ReadLoop = LwtLoop.Make (struct
    type acc = t

    let main connection =
      let%lwt msg =
        ( Marshal_tools_lwt.from_fd_with_preamble connection.in_fd
          : ConnectionProcessor.in_message Lwt.t )
      in
      let%lwt () = connection.on_read ~msg ~connection in
      Lwt.return connection

    let catch connection exn =
      (match Exception.unwrap exn with
      | End_of_file -> Logger.error "Connection '%s' was closed from the other side" connection.name
      | _ ->
        let exn = Exception.to_exn exn in
        Logger.error
          ~exn
          "Closing connection '%s' due to uncaught exception in read loop"
          connection.name);
      close_immediately connection
  end)

  let create ~name ~in_fd ~out_fd ~close ~on_read =
    let (wait_for_closed_thread, close) =
      (* Lwt.wait creates a thread that can't be canceled *)
      let (wait_for_closed_thread, wakener) = Lwt.wait () in
      (* If we've already woken the thread, then do nothing *)
      let wakeup () = (try Lwt.wakeup wakener () with Invalid_argument _ -> ()) in
      (* On close, wake wait_for_closed_thread *)
      let close () =
        let%lwt () = close () in
        wakeup ();
        Lwt.return_unit
      in
      (wait_for_closed_thread, close)
    in
    let (command_stream, push_to_stream) = Lwt_stream.create () in
    (* Lwt.task creates a thread that can be canceled *)
    let (paused_thread, wakener) = Lwt.task () in
    let conn =
      {
        name;
        in_fd;
        out_fd;
        command_stream;
        push_to_stream;
        close;
        on_read;
        command_thread =
          (let%lwt conn = paused_thread in
           CommandLoop.run conn);
        read_thread =
          (let%lwt conn = paused_thread in
           ReadLoop.run conn);
        wait_for_closed_thread;
      }
    in
    let start () = Lwt.wakeup wakener conn in
    Lwt.return (start, conn)
end
