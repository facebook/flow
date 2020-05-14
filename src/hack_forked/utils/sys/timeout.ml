(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Timeout of int

(* The IDs are used to tell the difference between timeout A timing out and timeout B timing out.
 * So they only really need to be unique between any two active timeouts in the same process. *)
let id_counter = ref 0

let mk_id () =
  incr id_counter;
  !id_counter

module Alarm_timeout = struct
  (** Timeout *)

  type t = int

  let with_timeout ~timeout ~on_timeout ~do_ =
    let id = mk_id () in
    let callback () = raise (Timeout id) in
    try
      let timer = Timer.set_timer ~interval:(float_of_int timeout) ~callback in
      let ret =
        try do_ id
        with exn ->
          let stack = Printexc.get_raw_backtrace () in
          (* Any uncaught exception will cancel the timeout *)
          Timer.cancel_timer timer;
          Printexc.raise_with_backtrace exn stack
      in
      Timer.cancel_timer timer;
      ret
    with Timeout exn_id when exn_id = id -> on_timeout ()

  let check_timeout _ = ()

  let select ?timeout:_ = Sys_utils.select_non_intr

  (** Channel *)

  type in_channel = Stdlib.in_channel * int option

  let ignore_timeout f ?timeout:_ (ic, _pid) = f ic

  let input = ignore_timeout Stdlib.input

  let really_input = ignore_timeout Stdlib.really_input

  let input_char = ignore_timeout Stdlib.input_char

  let input_line = ignore_timeout Stdlib.input_line

  let input_value_with_workaround ic =
    (* OCaml 4.03.0 changed the behavior of input_value to no longer
     * throw End_of_file when the pipe has closed. We can simulate that
     * behavior, however, by trying to read a byte afterwards, which WILL
     * raise End_of_file if the pipe has closed
     * http://caml.inria.fr/mantis/view.php?id=7142 *)
    try Stdlib.input_value ic
    with Failure msg as e ->
      if msg = "input_value: truncated object" then Stdlib.input_char ic |> ignore;
      raise e

  let input_value = ignore_timeout input_value_with_workaround

  let open_in name = (Stdlib.open_in name, None)

  let close_in (ic, _) = Stdlib.close_in ic

  let close_in_noerr (ic, _) = Stdlib.close_in_noerr ic

  let in_channel_of_descr fd = (Unix.in_channel_of_descr fd, None)

  let descr_of_in_channel (ic, _) = Unix.descr_of_in_channel ic

  let open_process cmd args =
    let (child_in_fd, out_fd) = Unix.pipe () in
    let (in_fd, child_out_fd) = Unix.pipe () in
    Unix.set_close_on_exec in_fd;
    Unix.set_close_on_exec out_fd;
    let pid = Unix.(create_process cmd args child_in_fd child_out_fd stderr) in
    Unix.close child_out_fd;
    Unix.close child_in_fd;
    let ic = (Unix.in_channel_of_descr in_fd, Some pid) in
    let oc = Unix.out_channel_of_descr out_fd in
    (ic, oc)

  let open_process_in cmd args =
    let (child_in_fd, out_fd) = Unix.pipe () in
    let (in_fd, child_out_fd) = Unix.pipe () in
    Unix.set_close_on_exec in_fd;
    Unix.set_close_on_exec out_fd;
    Unix.close out_fd;
    let pid = Unix.(create_process cmd args child_in_fd child_out_fd stderr) in
    Unix.close child_out_fd;
    Unix.close child_in_fd;
    let ic = (Unix.in_channel_of_descr in_fd, Some pid) in
    ic

  let close_process_in (ic, pid) =
    match pid with
    | None -> invalid_arg "Timeout.close_process_in"
    | Some pid ->
      Stdlib.close_in ic;
      snd (Sys_utils.waitpid_non_intr [] pid)

  let read_process ~timeout ~on_timeout ~reader cmd args =
    let (ic, oc) = open_process cmd args in
    with_timeout ~timeout ~on_timeout ~do_:(fun timeout ->
        try reader timeout ic oc
        with exn ->
          close_in ic;
          close_out oc;
          raise exn)

  let open_connection ?timeout:_ sockaddr =
    (* timeout isn't used in this Alarm_timeout implementation, but is used in Select_timeout *)
    let (ic, oc) = Unix.open_connection sockaddr in
    ((ic, None), oc)

  let shutdown_connection (ic, _) = Unix.shutdown_connection ic

  let is_timeout_exn id = function
    | Timeout exn_id -> exn_id = id
    | _ -> false
end

module Select_timeout = struct
  (** Timeout *)

  type t = {
    timeout: float;
    id: int;
  }

  let create timeout = { timeout = Unix.gettimeofday () +. timeout; id = mk_id () }

  let with_timeout ~timeout ~on_timeout ~do_ =
    let t = create (float timeout) in
    (try do_ t with Timeout exn_id when exn_id = t.id -> on_timeout ())

  let check_timeout t = if Unix.gettimeofday () > t.timeout then raise (Timeout t.id)

  (** Channel *)

  type channel = {
    fd: Unix.file_descr;
    buf: Bytes.t;
    mutable curr: int;
    mutable max: int;
    mutable pid: int option;
  }

  type in_channel = channel

  let buffer_size = 65536 - 9 (* From ocaml/byterun/io.h *)

  let in_channel_of_descr fd =
    let buf = Bytes.create buffer_size in
    { fd; buf; curr = 0; max = 0; pid = None }

  let descr_of_in_channel { fd; _ } = fd

  let open_in name =
    let fd = Unix.openfile name [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o640 in
    in_channel_of_descr fd

  let close_in tic = Unix.close tic.fd

  let close_in_noerr tic = (try Unix.close tic.fd with _ -> ())

  let close_process_in tic =
    match tic.pid with
    | None -> invalid_arg "Timeout.close_process_in"
    | Some pid ->
      close_in tic;
      snd (Sys_utils.waitpid_non_intr [] pid)

  (* A negative timeout for select means block until a fd is ready *)
  let no_select_timeout = ~-.1.0

  (* A wrapper around Sys_utils.select_non_intr. If timeout would fire before the select's timeout,
   * then change the select's timeout and throw an exception when it fires *)
  let select ?timeout rfds wfds xfds select_timeout =
    match timeout with
    (* No timeout set, fallback to Sys_utils.select_non_intr *)
    | None -> Sys_utils.select_non_intr rfds wfds xfds select_timeout
    | Some { timeout; id } ->
      let timeout = timeout -. Unix.gettimeofday () in
      (* Whoops, timeout already fired, throw right away! *)
      if timeout < 0. then raise (Timeout id);

      (* A negative select_timeout would mean wait forever *)
      if
        select_timeout >= 0.0 && select_timeout < timeout
        (* The select's timeout is smaller than our timeout, so leave it alone *)
      then
        Sys_utils.select_non_intr rfds wfds xfds select_timeout
      else (
        (* Our timeout is smaller, so use that *)
        match Sys_utils.select_non_intr rfds wfds xfds timeout with
        (* Timeout hit! Throw an exception! *)
        | ([], [], []) -> raise (Timeout id)
        (* Got a result before the timeout fired, so just return that *)
        | ret -> ret
      )

  let do_read ?timeout tic =
    match select ?timeout [tic.fd] [] [] no_select_timeout with
    | ([], _, _) ->
      failwith
        "This should be unreachable. How did select return with no fd when there is no timeout?"
    | ([_], _, _) ->
      let read =
        try Unix.read tic.fd tic.buf tic.max (buffer_size - tic.max)
        with Unix.Unix_error (Unix.EPIPE, _, _) -> raise End_of_file
      in
      tic.max <- tic.max + read;
      read
    | (_ :: _, _, _) -> assert false

  (* Should never happen *)

  let refill ?timeout tic =
    tic.curr <- 0;
    tic.max <- 0;
    let nread = do_read ?timeout tic in
    if nread = 0 then raise End_of_file;
    nread

  let unsafe_input ?timeout tic s ofs len =
    let n =
      if len > max_int then
        max_int
      else
        len
    in
    let avail = tic.max - tic.curr in
    if n <= avail then (
      (* There is enough to read in the buffer. *)
      Bytes.blit tic.buf tic.curr s ofs n;
      tic.curr <- tic.curr + n;
      n
    ) else if avail > 0 then (
      (* Read the rest of the buffer. *)
      Bytes.blit tic.buf tic.curr s ofs avail;
      tic.curr <- tic.curr + avail;
      avail
    ) else
      (* No input to read, refill buffer. *)
      let nread = refill ?timeout tic in
      let n = min nread n in
      Bytes.blit tic.buf tic.curr s ofs n;
      tic.curr <- tic.curr + n;
      n

  let input ?timeout tic s ofs len =
    if ofs < 0 || len < 0 || ofs > Bytes.length s - len then
      invalid_arg "input"
    else
      unsafe_input ?timeout tic s ofs len

  let input_char ?timeout tic =
    if tic.curr = tic.max then ignore (refill ?timeout tic);
    tic.curr <- tic.curr + 1;
    Bytes.get tic.buf (tic.curr - 1)

  (* Read in channel until we discover a '\n' *)
  let input_scan_line ?timeout tic =
    let rec scan_line tic pos =
      if pos < tic.max then
        if Bytes.get tic.buf pos = '\n' then
          pos - tic.curr + 1
        else
          scan_line tic (pos + 1)
      else
        let pos =
          if tic.curr <> 0 then (
            tic.max <- tic.max - tic.curr;
            Bytes.blit tic.buf tic.curr tic.buf 0 tic.max;
            tic.curr <- 0;
            tic.max
          ) else
            pos
        in
        if tic.max = buffer_size then
          -(tic.max - tic.curr)
        else
          let nread = do_read ?timeout tic in
          if nread = 0 then
            -(tic.max - tic.curr)
          else
            scan_line tic pos
    in
    scan_line tic tic.curr

  let input_line ?timeout tic =
    let rec build_result buf pos = function
      | [] -> buf
      | hd :: tl ->
        let len = Bytes.length hd in
        Bytes.blit hd 0 buf (pos - len) len;
        build_result buf (pos - len) tl
    in
    let rec scan accu len =
      let n = input_scan_line ?timeout tic in
      (* End of file, if accu is not empty, return the last line. *)
      if n = 0 then
        match accu with
        | [] -> raise End_of_file
        | _ -> build_result (Bytes.create len) len accu
      (* New line found in the buffer. *)
      else if n > 0 then (
        let result = Bytes.create (n - 1) in
        (* No need to keep '\n' *)
        ignore (unsafe_input tic result 0 (n - 1));
        ignore (input_char tic);

        (* Skip newline *)
        match accu with
        | [] -> result
        | _ ->
          let len = len + n - 1 in
          build_result (Bytes.create len) len (result :: accu)
        (* New line not found in the buffer *)
      ) else
        let ofs = Bytes.create (-n) in
        ignore (unsafe_input tic ofs 0 (-n));
        scan (ofs :: accu) (len - n)
    in
    Bytes.unsafe_to_string (scan [] 0)

  let rec unsafe_really_input ?timeout tic buf ofs len =
    if len = 0 then
      ()
    else
      let r = unsafe_input ?timeout tic buf ofs len in
      if r = 0 then
        raise End_of_file
      else
        unsafe_really_input ?timeout tic buf (ofs + r) (len - r)

  let really_input ?timeout tic buf ofs len =
    if ofs < 0 || len < 0 || ofs > Bytes.length buf - len then
      invalid_arg "really_input"
    else
      unsafe_really_input ?timeout tic buf ofs len

  (** Marshal *)

  let marshal_magic = Bytes.of_string "\x84\x95\xA6\xBE"

  let input_value ?timeout tic =
    let magic = Bytes.create 4 in
    Bytes.set magic 0 (input_char ?timeout tic);
    Bytes.set magic 1 (input_char ?timeout tic);
    Bytes.set magic 2 (input_char ?timeout tic);
    Bytes.set magic 3 (input_char ?timeout tic);
    if magic <> marshal_magic then failwith "Select.input_value: bad object.";
    let b1 = int_of_char (input_char ?timeout tic) in
    let b2 = int_of_char (input_char ?timeout tic) in
    let b3 = int_of_char (input_char ?timeout tic) in
    let b4 = int_of_char (input_char ?timeout tic) in
    let len = ((b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4) + 12 in
    let data = Bytes.create (len + 8) in
    Bytes.blit magic 0 data 0 4;
    Bytes.set data 4 (char_of_int b1);
    Bytes.set data 5 (char_of_int b2);
    Bytes.set data 6 (char_of_int b3);
    Bytes.set data 7 (char_of_int b4);
    begin
      try unsafe_really_input ?timeout tic data 8 len
      with End_of_file -> failwith "Select.input_value: truncated object."
    end;
    Marshal.from_bytes data 0

  (** Process *)

  let open_process cmd args =
    let (child_in_fd, out_fd) = Unix.pipe () in
    let (in_fd, child_out_fd) = Unix.pipe () in
    Unix.set_close_on_exec in_fd;
    Unix.set_close_on_exec out_fd;
    let pid = Unix.(create_process cmd args child_in_fd child_out_fd stderr) in
    Unix.close child_out_fd;
    Unix.close child_in_fd;
    let tic = in_channel_of_descr in_fd in
    tic.pid <- Some pid;
    let oc = Unix.out_channel_of_descr out_fd in
    (tic, oc)

  let open_process_in cmd args =
    let (child_in_fd, out_fd) = Unix.pipe () in
    let (in_fd, child_out_fd) = Unix.pipe () in
    Unix.set_close_on_exec in_fd;
    Unix.set_close_on_exec out_fd;
    Unix.close out_fd;
    let pid = Unix.(create_process cmd args child_in_fd child_out_fd stderr) in
    Unix.close child_out_fd;
    Unix.close child_in_fd;
    let tic = in_channel_of_descr in_fd in
    tic.pid <- Some pid;
    tic

  let read_process ~timeout ~on_timeout ~reader cmd args =
    let (tic, oc) = open_process cmd args in
    let on_timeout () =
      Base.Option.iter ~f:Sys_utils.terminate_process tic.pid;
      tic.pid <- None;
      on_timeout ()
    in
    with_timeout ~timeout ~on_timeout ~do_:(fun timeout ->
        try reader timeout tic oc
        with exn ->
          Base.Option.iter ~f:Sys_utils.terminate_process tic.pid;
          tic.pid <- None;
          close_in tic;
          close_out oc;
          raise exn)

  (** Socket *)

  let open_connection ?timeout sockaddr =
    let connect sock sockaddr =
      (* connect binds the fd sock to the socket at sockaddr. If sock is nonblocking, and the
       * connect call would block, it errors. You can then use select to wait for the connect
       * to finish.
       *
       * On Windows, if the connect succeeds, sock will be returned in the writable fd set.
       * If the connect fails, the sock will be returned in the exception fd set.
       * https://msdn.microsoft.com/en-us/library/windows/desktop/ms737625(v=vs.85).aspx
       *
       * On Linux, the sock will always be returned in the writable fd set, and you're supposed
       * to use getsockopt to read the SO_ERROR option at level SOL_SOCKET to figure out if the
       * connect worked. However, this code is only used on Windows, so that's fine *)
      try Unix.connect sock sockaddr with
      | Unix.Unix_error ((Unix.EINPROGRESS | Unix.EWOULDBLOCK), _, _) ->
        begin
          match select ?timeout [] [sock] [] no_select_timeout with
          | (_, [], [exn_sock]) when exn_sock = sock -> failwith "Failed to connect to socket"
          | (_, [], _) ->
            failwith
              "This should be unreachable. How did select return with no fd when there is no timeout?"
          | (_, [_sock], _) -> ()
          | (_, _, _) -> assert false
        end
      | exn ->
        Unix.close sock;
        raise exn
    in
    let sock = Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
    Unix.set_nonblock sock;
    connect sock sockaddr;
    Unix.clear_nonblock sock;
    Unix.set_close_on_exec sock;
    let tic = in_channel_of_descr sock in
    let oc = Unix.out_channel_of_descr sock in
    (tic, oc)

  let shutdown_connection { fd; _ } = Unix.(shutdown fd SHUTDOWN_SEND)

  let is_timeout_exn { id; timeout = _ } = function
    | Timeout exn_id -> exn_id = id
    | _ -> false
end

module type S = sig
  type t

  val with_timeout : timeout:int -> on_timeout:(unit -> 'a) -> do_:(t -> 'a) -> 'a

  val check_timeout : t -> unit

  type in_channel

  val in_channel_of_descr : Unix.file_descr -> in_channel

  val descr_of_in_channel : in_channel -> Unix.file_descr

  val open_in : string -> in_channel

  val close_in : in_channel -> unit

  val close_in_noerr : in_channel -> unit

  val select :
    ?timeout:t ->
    Unix.file_descr list ->
    Unix.file_descr list ->
    Unix.file_descr list ->
    float ->
    Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

  val input : ?timeout:t -> in_channel -> bytes -> int -> int -> int

  val really_input : ?timeout:t -> in_channel -> bytes -> int -> int -> unit

  val input_char : ?timeout:t -> in_channel -> char

  val input_line : ?timeout:t -> in_channel -> string

  val input_value : ?timeout:t -> in_channel -> 'a

  val open_process : string -> string array -> in_channel * out_channel

  val open_process_in : string -> string array -> in_channel

  val close_process_in : in_channel -> Unix.process_status

  val read_process :
    timeout:int ->
    on_timeout:(unit -> 'a) ->
    reader:(t -> in_channel -> out_channel -> 'a) ->
    string ->
    string array ->
    'a

  val open_connection : ?timeout:t -> Unix.sockaddr -> in_channel * out_channel

  val shutdown_connection : in_channel -> unit

  val is_timeout_exn : t -> exn -> bool
end

let select = (module Select_timeout : S)

let alarm = (module Alarm_timeout : S)

include ( val if Sys.win32 then
                select
              else
                alarm )

let read_connection ~timeout ~on_timeout ~reader sockaddr =
  with_timeout ~timeout ~on_timeout ~do_:(fun timeout ->
      let (tic, oc) = open_connection ~timeout sockaddr in
      try reader timeout tic oc
      with exn ->
        close_out oc;
        raise exn)
