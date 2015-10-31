(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

exception Timeout

module Alarm_timeout = struct

  (** Timeout *)

  type t = unit
  let with_timeout ~timeout ?on_timeout ~do_ =
    let old_handler = ref Sys.Signal_default in
    let old_timeout = ref 0 in
    let on_timeout _sigalrm =
      match on_timeout with
      | None -> raise Timeout
      | Some f -> f (); raise Timeout in
    Utils.with_context
      ~enter:(fun () ->
          old_handler := Sys.signal Sys.sigalrm (Sys.Signal_handle on_timeout);
          old_timeout := Unix.alarm timeout)
      ~exit:(fun () ->
          ignore (Unix.alarm !old_timeout);
          Sys.set_signal Sys.sigalrm !old_handler)
      ~do_
  let check_timeout () = ()

  (** Channel *)

  type in_channel = Pervasives.in_channel * int option
  let ignore_timeout f ?timeout (ic, _pid) = f ic
  let input = ignore_timeout Pervasives.input
  let really_input = ignore_timeout Pervasives.really_input
  let input_char = ignore_timeout Pervasives.input_char
  let input_line = ignore_timeout Pervasives.input_line
  let input_value = ignore_timeout Pervasives.input_value
  let open_in name = Pervasives.open_in name, None
  let close_in (ic, _) = Pervasives.close_in ic
  let close_in_noerr (ic, _) = Pervasives.close_in_noerr ic
  let in_channel_of_descr fd = Unix.in_channel_of_descr fd, None
  let descr_of_in_channel (ic, _) = Unix.descr_of_in_channel ic
  let open_process cmd args =
    let child_in_fd, out_fd = Unix.pipe () in
    let in_fd, child_out_fd = Unix.pipe () in
    Unix.set_close_on_exec in_fd;
    Unix.set_close_on_exec out_fd;
    let pid =
      Unix.(create_process cmd args child_in_fd child_out_fd stderr) in
    Unix.close child_out_fd;
    Unix.close child_in_fd;
    let ic = (Unix.in_channel_of_descr in_fd, Some pid) in
    let oc = Unix.out_channel_of_descr out_fd in
    (ic, oc)

  let open_process_in cmd args =
    let child_in_fd, out_fd = Unix.pipe () in
    let in_fd, child_out_fd = Unix.pipe () in
    Unix.set_close_on_exec in_fd;
    Unix.set_close_on_exec out_fd;
    Unix.close out_fd;
    let pid =
      Unix.(create_process cmd args child_in_fd child_out_fd stderr) in
    Unix.close child_out_fd;
    Unix.close child_in_fd;
    let ic = (Unix.in_channel_of_descr in_fd, Some pid) in
    ic

  let close_process_in (ic, pid) =
    match pid with
    | None -> invalid_arg "Timeout.close_process_in"
    | Some pid ->
        Pervasives.close_in ic;
        snd(Unix.waitpid [] pid)

  let read_process ~timeout ?on_timeout ~reader cmd args =
    let (ic, oc) = open_process cmd args in
    with_timeout ~timeout ?on_timeout
      ~do_:(fun timeout ->
          try reader timeout ic oc
          with exn -> close_in ic; close_out oc; raise exn)

  let open_connection ?timeout sockaddr =
    let (ic, oc) = Unix.open_connection sockaddr in
    ((ic, None), oc)

  let shutdown_connection (ic, _) =
    Unix.shutdown_connection ic

end

module Select_timeout = struct

  (** Timeout *)

  type t = {
    timeout: float;
  }
  let create timeout =
    { timeout = Unix.gettimeofday () +. timeout }
  let with_timeout ~timeout ?on_timeout ~do_ =
    let t = create (float timeout) in
    try do_ t
    with Timeout as exn ->
      match on_timeout with
      | None -> raise exn
      | Some ft -> ft ()
  let check_timeout t =
    if Unix.gettimeofday () > t.timeout then raise Timeout
  let get_current_timeout = function
    | None -> -. 1.
    | Some { timeout }  ->
        let timeout = timeout -. Unix.gettimeofday () in
        if timeout < 0. then raise Timeout;
        timeout

  (** Channel *)

  type channel = {
    fd: Unix.file_descr;
    buf: String.t;
    mutable curr: int;
    mutable max: int;
    mutable pid: int option;
  }

  type in_channel = channel

  let buffer_size = 65536-9  (* From ocaml/byterun/io.h *)

  let in_channel_of_descr fd =
    begin
      try Unix.set_nonblock fd
      with _ ->
        (* On windows, only 'socket' need to ba tagged non-blocking. *)
        ()
    end;
    let buf = String.create buffer_size in
    { fd; buf; curr = 0; max = 0; pid = None }

  let descr_of_in_channel { fd; _ } = fd

  let open_in name =
    let fd = Unix.openfile name [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o640 in
    in_channel_of_descr fd

  let close_in tic = Unix.close tic.fd

  let close_in_noerr tic =
    try Unix.close tic.fd
    with _ -> ()

  let rec waitpid_non_intr pid =
    try Unix.waitpid [] pid
    with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

  let close_process_in tic =
    match tic.pid with
    | None -> invalid_arg "Timeout.close_process_in"
    | Some pid ->
        close_in tic;
        snd (waitpid_non_intr pid)

  let do_read ?timeout tic =
    let timeout = get_current_timeout timeout in
    match Unix.select [ tic.fd ] [] [] timeout with
    | [], _, _ -> raise Timeout
    | [_], _, _ ->
        let read = Unix.read tic.fd tic.buf tic.max (buffer_size - tic.max) in
        tic.max <- tic.max + read;
        read
    | _ :: _, _, _-> assert false (* Should never happen *)

  let refill ?timeout tic =
    tic.curr <- 0;
    tic.max <- 0;
    let nread = do_read ?timeout tic in
    if nread = 0 then raise End_of_file;
    nread

  let unsafe_input ?timeout tic s ofs len =
    let n = if len > max_int then max_int else len in
    let avail = tic.max - tic.curr in
    if n <= avail then begin (* There is enough to read in the buffer. *)
      String.blit tic.buf tic.curr s ofs n;
      tic.curr <- tic.curr + n;
      n
    end else if avail > 0 then begin (* Read the rest of the buffer. *)
      String.blit tic.buf tic.curr s ofs avail;
      tic.curr <- tic.curr + avail;
      avail
    end else begin (* No input to read, refill buffer. *)
      let nread = refill ?timeout tic in
      let n = min nread n in
      String.blit tic.buf tic.curr s ofs n;
      tic.curr <- tic.curr + n;
      n
    end

  let input ?timeout tic s ofs len =
    if ofs < 0 || len < 0 || ofs > String.length s - len then
      invalid_arg "input"
    else
      unsafe_input ?timeout tic s ofs len

  let input_char ?timeout tic =
    if tic.curr = tic.max then ignore (refill ?timeout tic);
    tic.curr <- tic.curr + 1;
    tic.buf.[tic.curr - 1]

  (* Read in channel until we discover a '\n' *)
  let input_scan_line ?timeout tic =
    let rec scan_line tic pos =
      if pos < tic.max then
        if tic.buf.[pos] = '\n' then
          pos - tic.curr + 1
        else
          scan_line tic (pos+1)
      else begin
        let pos =
          if tic.curr <> 0 then begin
            tic.max <- tic.max - tic.curr;
            String.blit tic.buf tic.curr tic.buf 0 tic.max;
            tic.curr <- 0;
            tic.max
          end else
            pos
        in
        if tic.max = buffer_size then
          - (tic.max - tic.curr)
        else
          let nread = do_read ?timeout tic in
          if nread = 0 then
            - (tic.max - tic.curr)
          else begin
            scan_line tic pos
          end
      end in
    scan_line tic tic.curr

  let input_line ?timeout tic  =

    let rec build_result buf pos = function
      | [] -> buf
      | hd :: tl ->
          let len = String.length hd in
          String.blit hd 0 buf (pos - len) len;
          build_result buf (pos - len) tl in

    let rec scan accu len =

      let n = input_scan_line ?timeout tic in

      (* End of file, if accu is not empty, return the last line. *)
      if n = 0 then begin
        match accu with
        | [] -> raise End_of_file
        | _ -> build_result (String.create len) len accu

        (* New line found in the buffer. *)
      end else if n > 0 then begin
        let result = String.create (n - 1) in (* No need to keep '\n' *)
        ignore (unsafe_input tic result 0 (n - 1));
        ignore (input_char tic); (* Skip newline *)
        match accu with
        | [] -> result
        | _ ->
            let len = len + n - 1 in
            build_result (String.create len) len (result :: accu)

        (* New line not found in the buffer *)
      end else begin
        let ofs = String.create (-n) in
        ignore (unsafe_input tic ofs 0 (-n));
        scan (ofs :: accu) (len - n)
      end in

    scan [] 0

  let rec unsafe_really_input ?timeout tic buf ofs len =
    if len = 0 then
      ()
    else
      let r = unsafe_input ?timeout tic buf ofs len in
      if r = 0
      then raise End_of_file
      else unsafe_really_input ?timeout tic buf (ofs + r) (len - r)

  let really_input ?timeout tic buf ofs len =
    if ofs < 0 || len < 0 || ofs > String.length buf - len then
      invalid_arg "really_input"
    else
      unsafe_really_input ?timeout tic buf ofs len

  (** Marshal *)

  let marshal_magic =  "\x84\x95\xA6\xBE"
  let input_value ?timeout tic =
    let magic = String.create 4 in
    magic.[0] <- input_char ?timeout tic;
    magic.[1] <- input_char ?timeout tic;
    magic.[2] <- input_char ?timeout tic;
    magic.[3] <- input_char ?timeout tic;
    if magic <> marshal_magic then
      failwith "Select.input_value: bad object.";
    let b1 = int_of_char (input_char ?timeout tic) in
    let b2 = int_of_char (input_char ?timeout tic) in
    let b3 = int_of_char (input_char ?timeout tic) in
    let b4 = int_of_char (input_char ?timeout tic) in
    let len = ((b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4) + 12 in
    let data = String.create (len + 8) in
    String.blit magic 0 data 0 4;
    data.[4] <- char_of_int b1;
    data.[5] <- char_of_int b2;
    data.[6] <- char_of_int b3;
    data.[7] <- char_of_int b4;
    begin
      try unsafe_really_input ?timeout tic data 8 len
      with End_of_file ->
        failwith "Select.input_value: truncated object."
    end;
    Marshal.from_string data 0

  (** Process *)

  let open_process cmd args =
    let child_in_fd, out_fd = Unix.pipe () in
    let in_fd, child_out_fd = Unix.pipe () in
    Unix.set_close_on_exec in_fd;
    Unix.set_close_on_exec out_fd;
    let pid =
      Unix.(create_process cmd args child_in_fd child_out_fd stderr) in
    Unix.close child_out_fd;
    Unix.close child_in_fd;
    let tic = in_channel_of_descr in_fd in
    tic.pid <- Some pid;
    let oc = Unix.out_channel_of_descr out_fd in
    (tic, oc)

  let open_process_in cmd args =
    let child_in_fd, out_fd = Unix.pipe () in
    let in_fd, child_out_fd = Unix.pipe () in
    Unix.set_close_on_exec in_fd;
    Unix.set_close_on_exec out_fd;
    Unix.close out_fd;
    let pid =
      Unix.(create_process cmd args child_in_fd child_out_fd stderr) in
    Unix.close child_out_fd;
    Unix.close child_in_fd;
    let tic = in_channel_of_descr in_fd in
    tic.pid <- Some pid;
    tic

  let read_process ~timeout ?on_timeout ~reader cmd args =
    let (tic, oc) = open_process cmd args in
    let on_timeout () =
      Option.iter ~f:Sys_utils.terminate_process tic.pid;
      tic.pid <- None;
      match on_timeout with
      | None -> raise Timeout
      | Some f -> f () in
    with_timeout ~timeout ~on_timeout
      ~do_:(fun timeout ->
          try reader timeout tic oc
          with exn ->
            Option.iter ~f:Sys_utils.terminate_process tic.pid;
            tic.pid <- None;
            close_in tic;
            close_out oc;
            raise exn)

  (** Socket *)

  let open_connection ?timeout sockaddr =
    let connect sock sockaddr =
      try
        Unix.connect sock sockaddr;
      with
      | Unix.Unix_error ((Unix.EINPROGRESS | Unix.EWOULDBLOCK), _, _) -> begin
          let timeout = get_current_timeout timeout in
          match Unix.select [] [sock] [] timeout with
          | _, [], _ -> raise Timeout
          | _, [sock], _ -> ()
          | _, _, _ -> assert false
        end
      | exn -> Unix.close sock; raise exn in
    let sock =
      Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
    Unix.set_nonblock sock;
    connect sock sockaddr;
    Unix.clear_nonblock sock;
    Unix.set_close_on_exec sock;
    let tic = in_channel_of_descr sock in
    let oc = Unix.out_channel_of_descr sock in
    (tic, oc)

  let shutdown_connection { fd; _ } =
    Unix.(shutdown fd SHUTDOWN_SEND)

end

module type S = sig

  type t
  val with_timeout:
    timeout:int ->
    ?on_timeout:(unit -> 'a) ->
    do_:(t -> 'a) -> 'a
  val check_timeout: t -> unit

  type in_channel
  val in_channel_of_descr: Unix.file_descr -> in_channel
  val descr_of_in_channel: in_channel -> Unix.file_descr
  val open_in: string -> in_channel
  val close_in: in_channel -> unit
  val close_in_noerr: in_channel -> unit
  val input: ?timeout:t -> in_channel -> string -> int -> int -> int
  val really_input: ?timeout:t -> in_channel -> string -> int -> int -> unit
  val input_char: ?timeout:t -> in_channel -> char
  val input_line: ?timeout:t -> in_channel -> string
  val input_value: ?timeout:t -> in_channel -> 'a
  val open_process: string -> string array -> in_channel * out_channel
  val open_process_in: string -> string array -> in_channel
  val close_process_in: in_channel -> Unix.process_status
  val read_process:
    timeout:int ->
    ?on_timeout:(unit -> 'a) ->
    reader:(t -> in_channel -> out_channel -> 'a) ->
    string -> string array -> 'a
  val open_connection:
    ?timeout:t -> Unix.sockaddr -> in_channel * out_channel
  val shutdown_connection: in_channel -> unit
end

let select = (module Select_timeout : S)
let alarm = (module Alarm_timeout : S)

include (val (if Sys.win32 then select else alarm))

let read_connection ~timeout ?on_timeout ~reader sockaddr =
  with_timeout ~timeout ?on_timeout
    ~do_:(fun timeout ->
       let (tic, oc) = open_connection ~timeout sockaddr in
       try reader timeout tic oc
       with exn ->
         close_out oc;
         raise exn)
