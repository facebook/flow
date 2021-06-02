(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Wrapper for handling JSON-RPC *)
(* Spec: http://www.jsonrpc.org/specification *)
(* Practical readbable guide: https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#base-protocol-json-structures *)

open Hh_core
module J = Hh_json_helpers.AdhocJsonHelpers

type writer = Hh_json.json -> unit

type kind =
  | Request
  | Notification
  | Response

let kind_to_string (kind : kind) : string =
  match kind with
  | Request -> "Request"
  | Notification -> "Notification"
  | Response -> "Response"

type message = {
  json: Hh_json.json;
  (* the json payload *)
  timestamp: float;
  (* time this message arrived at stdin *)
  (* Following fields are decompositions of 'json'... *)
  kind: kind;
  method_: string;
  (* mandatory for request+notification; empty otherwise *)
  id: Hh_json.json option;
  (* mandatory for request+response *)
  params: Hh_json.json option;
  (* optional for request+notification *)
  result: Hh_json.json option;
  (* optional for response *)
  error: Hh_json.json option; (* optional for response *)
}

let message_to_short_string (c : message) : string =
  Hh_json.(
    let disposition =
      match (c.kind, c.result, c.error) with
      | (Response, Some _, None) -> "[result]"
      | (Response, None, Some _) -> "[error]"
      | (_, _, _) -> ""
    in
    let method_ =
      match c.method_ with
      | "" -> ""
      | s -> Printf.sprintf "method=%s," s
    in
    let id =
      match c.id with
      | Some (JSON_String s) -> Printf.sprintf "id=\"%s\"" s
      | Some (JSON_Number n) -> Printf.sprintf "id=#%s" n
      | Some json -> Printf.sprintf "id=%s" (json_to_string json)
      | None -> "id=[None]"
    in
    Printf.sprintf "{%s%s,%s%s}" (kind_to_string c.kind) disposition method_ id)

let parse_message ~(json : Hh_json.json) ~(timestamp : float) : message =
  let id = J.try_get_val "id" json in
  let method_opt = J.try_get_val "method" json |> Base.Option.map ~f:Hh_json.get_string_exn in
  let method_ = Base.Option.value method_opt ~default:"" in
  (* is easier to consume *)
  let params = J.try_get_val "params" json in
  let result = J.try_get_val "result" json in
  let error = J.try_get_val "error" json in
  (* Following categorization mostly mirrors that of VSCode except that     *)
  (* VSCode allows number+string+null ID for response, but we allow any ID. *)
  let kind =
    match (id, method_opt, result, error) with
    | (Some _id, Some _method, _, _) -> Request
    | (None, Some _method, _, _) -> Notification
    | (_, _, Some _result, _) -> Response
    | (_, _, _, Some _error) -> Response
    | _ -> raise (Hh_json.Syntax_error "Not JsonRPC")
  in
  { json; timestamp; id; method_; params; result; error; kind }

(***************************************************************)
(* Internal queue functions that run in the daemon process.    *)
(***************************************************************)

type queue = {
  daemon_in_fd: Unix.file_descr;
  (* fd used by main process to read messages from queue *)
  messages: queue_message Queue.t;
}

and timestamped_json = {
  tj_json: Hh_json.json;
  tj_timestamp: float;
}

and queue_message =
  | Timestamped_json of timestamped_json
  | Fatal_exception of Marshal_tools.remote_exception_data
  | Recoverable_exception of Marshal_tools.remote_exception_data

and daemon_operation =
  | Read
  | Write

(* Try to read a message from the daemon's stdin, which is where all of the
   editor messages can be read from. May throw if the message is malformed. *)
let internal_read_message (reader : Buffered_line_reader.t) : timestamped_json =
  let message = reader |> Http_lite.read_message_utf8 in
  let tj_json = Hh_json.json_of_string message in
  let tj_timestamp = Unix.gettimeofday () in
  { tj_json; tj_timestamp }

(* Reads messages from the editor on stdin, parses them, and sends them to the
   main process.
   This runs in a different process because we also timestamp the messages, so
   we need to read them as soon as they come in. That is, we can't wait for any
   server computation to finish if we want to get an accurate timestamp. *)
let internal_run_daemon' (oc : queue_message Daemon.out_channel) : unit =
  let out_fd = Daemon.descr_of_out_channel oc in
  let reader = Buffered_line_reader.create Unix.stdin in
  let messages_to_send = Queue.create () in
  let rec loop () =
    let operation =
      if Buffered_line_reader.has_buffered_content reader then
        Read
      else
        let read_fds = [Unix.stdin] in
        let has_messages_to_send = not (Queue.is_empty messages_to_send) in
        let write_fds =
          if has_messages_to_send then
            [out_fd]
          else
            []
        in
        (* Note that if there are no queued messages, this will always block
           until we're ready to read, rather than returning `Write`, even if
           stdout is capable of being written to. Furthermore, we will never
           need to queue a message to be written until we have read
           something. *)
        let (readable_fds, _, _) = Unix.select read_fds write_fds [] (-1.0) in
        let ready_for_read = not (List.is_empty readable_fds) in
        if ready_for_read then
          Read
        else
          Write
    in
    let should_continue =
      match operation with
      | Read ->
        begin
          try
            let timestamped_json = internal_read_message reader in
            Queue.push timestamped_json messages_to_send;
            true
          with exn ->
            let e = Exception.wrap exn in
            let message = Exception.get_ctor_string e in
            let stack = Exception.get_full_backtrace_string 500 e in
            let edata = { Marshal_tools.message; stack } in
            let (should_continue, marshal) =
              match exn with
              | Hh_json.Syntax_error _ -> (true, Recoverable_exception edata)
              | _ -> (false, Fatal_exception edata)
            in
            Marshal_tools.to_fd_with_preamble out_fd marshal |> ignore;
            should_continue
        end
      | Write ->
        assert (not (Queue.is_empty messages_to_send));
        let timestamped_json = Queue.pop messages_to_send in
        (* We can assume that the entire write will succeed, since otherwise
           Marshal_tools.to_fd_with_preamble will throw an exception. *)
        Marshal_tools.to_fd_with_preamble out_fd (Timestamped_json timestamped_json) |> ignore;
        true
    in
    if should_continue then loop ()
  in
  loop ()

(*  Main function for the daemon process. *)
let internal_run_daemon (_dummy_param : unit) (_ic, (oc : queue_message Daemon.out_channel)) =
  Exception.record_backtrace true;
  try internal_run_daemon' oc
  with exn ->
    (* An exception that's gotten here is not simply a parse error, but
       something else, so we should terminate the daemon at this point. *)
    let e = Exception.wrap exn in
    let message = Exception.get_ctor_string e in
    let stack = Exception.get_full_backtrace_string 500 e in
    (try
       let out_fd = Daemon.descr_of_out_channel oc in
       Marshal_tools.to_fd_with_preamble out_fd (Fatal_exception { Marshal_tools.message; stack })
       |> ignore
     with _ ->
       (* There may be a broken pipe, for example. We should just give up on
          reporting the error. *)
       ())

let internal_entry_point : (unit, unit, queue_message) Daemon.entry =
  Daemon.register_entry_point "Jsonrpc" internal_run_daemon

(************************************************)
(* Queue functions that run in the main process *)
(************************************************)

let make_queue () : queue =
  let handle =
    Daemon.spawn
      ~channel_mode:`pipe
      (* We don't technically need to inherit stdout or stderr, but this might be
         useful in the event that we throw an unexpected exception in the daemon.
         It's also useful for print-statement debugging of the daemon. *)
      (Unix.stdin, Unix.stdout, Unix.stderr)
      internal_entry_point
      ()
  in
  let (ic, _) = handle.Daemon.channels in
  { daemon_in_fd = Daemon.descr_of_in_channel ic; messages = Queue.create () }

let get_read_fd (queue : queue) : Unix.file_descr = queue.daemon_in_fd

(* Read a message into the queue, and return the just-read message. *)
let read_single_message_into_queue_wait (message_queue : queue) : queue_message Lwt.t =
  let%lwt message =
    try%lwt
      let%lwt message =
        Marshal_tools_lwt.from_fd_with_preamble
          (Lwt_unix.of_unix_file_descr message_queue.daemon_in_fd)
      in
      Lwt.return message
    with End_of_file as e ->
      (* This is different from when the client hangs up. It handles the case
         that the daemon process exited: for example, if it was killed. *)
      let e = Exception.wrap e in
      let message = Exception.get_ctor_string e in
      let stack = Exception.get_full_backtrace_string 500 e in
      Lwt.return (Fatal_exception { Marshal_tools.message; stack })
  in
  Queue.push message message_queue.messages;
  Lwt.return message

let rec read_messages_into_queue_no_wait (message_queue : queue) : unit Lwt.t =
  let is_readable = Lwt_unix.readable (Lwt_unix.of_unix_file_descr message_queue.daemon_in_fd) in
  let%lwt () =
    if is_readable then
      (* We're expecting this not to block because we just checked
         to make sure that there's something there. *)
        let%lwt message = read_single_message_into_queue_wait message_queue in
        (* Now read any more messages that might be queued up. Only try to read more
           messages if the daemon is still available to read from. Otherwise, we may
           infinite loop as a result of `Unix.select` returning that a file
           descriptor is available to read on. *)
        match message with
        | Fatal_exception _ -> Lwt.return_unit
        | _ ->
          let%lwt () = read_messages_into_queue_no_wait message_queue in
          Lwt.return_unit
    else
      Lwt.return_unit
  in
  Lwt.return_unit

let has_message (queue : queue) : bool =
  let is_readable = Lwt_unix.readable (Lwt_unix.of_unix_file_descr queue.daemon_in_fd) in
  is_readable || not (Queue.is_empty queue.messages)

let get_message (queue : queue) =
  (* Read one in a blocking manner to ensure that we have one. *)
  let%lwt () =
    if Queue.is_empty queue.messages then
      let%lwt (_message : queue_message) = read_single_message_into_queue_wait queue in
      Lwt.return_unit
    else
      Lwt.return_unit
  in
  (* Then read any others that got queued up so that we can see the maximum
     number of messages at once for invalidation purposes. *)
  let%lwt () = read_messages_into_queue_no_wait queue in
  let item = Queue.pop queue.messages in
  match item with
  | Timestamped_json { tj_json; tj_timestamp } ->
    Lwt.return (`Message (parse_message tj_json tj_timestamp))
  | Fatal_exception data -> Lwt.return (`Fatal_exception data)
  | Recoverable_exception data -> Lwt.return (`Recoverable_exception data)

(************************************************)
(* Output functions for respond+notify          *)
(************************************************)

let last_sent_ref : Hh_json.json option ref = ref None

let clear_last_sent () : unit = last_sent_ref := None

let last_sent () : Hh_json.json option = !last_sent_ref

(* respond: sends either a Response or an Error message, according
   to whether the json has an error-code or not. *)
let respond
    (writer : writer)
    ?(powered_by : string option)
    (in_response_to : message)
    (result_or_error : Hh_json.json) : unit =
  Hh_json.(
    let is_error =
      match result_or_error with
      | JSON_Object _ -> J.try_get_val "code" result_or_error |> Base.Option.is_some
      | _ -> false
    in
    let response =
      JSON_Object
        ( [("jsonrpc", JSON_String "2.0")]
        @ [("id", Base.Option.value in_response_to.id ~default:JSON_Null)]
        @ ( if is_error then
            [("error", result_or_error)]
          else
            [("result", result_or_error)] )
        @
        match powered_by with
        | Some powered_by -> [("powered_by", JSON_String powered_by)]
        | None -> [] )
    in
    last_sent_ref := Some response;
    writer response)

(* notify: sends a Notify message *)
let notify
    (writer : writer) ?(powered_by : string option) (method_ : string) (params : Hh_json.json) :
    unit =
  Hh_json.(
    let message =
      JSON_Object
        ( [("jsonrpc", JSON_String "2.0"); ("method", JSON_String method_); ("params", params)]
        @
        match powered_by with
        | Some powered_by -> [("powered_by", JSON_String powered_by)]
        | None -> [] )
    in
    last_sent_ref := Some message;
    writer message)

(************************************************)
(* Output functions for request                 *)
(************************************************)

let requests_counter : IMap.key ref = ref 0

let get_next_request_id () : int =
  incr requests_counter;
  !requests_counter
