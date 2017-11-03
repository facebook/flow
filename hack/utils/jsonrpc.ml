(* Wrapper over stdin/stdout for handling JSON-RPC *)
(* Spec: http://www.jsonrpc.org/specification *)
(* Practical readbable guide: https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#base-protocol-json-structures *)

open Hh_core


(***************************************************************)
(* Internal queue functions that run in the daemon process.    *)
(* The public API for this module comes from Jsonrpc.Make(...) *)
(***************************************************************)

type internal_queue = {
  daemon_in_fd : Unix.file_descr; (* fd used by main process to read messages from queue *)
  messages : queue_message Queue.t;
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
  let tj_timestamp = Unix.gettimeofday ()
  in
  { tj_json; tj_timestamp; }


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
      if Buffered_line_reader.has_buffered_content reader
      then Read
      else begin
        let read_fds = [Unix.stdin] in
        let has_messages_to_send = not (Queue.is_empty messages_to_send) in
        let write_fds =
          if has_messages_to_send
          then [out_fd]
          else []
        in

        (* Note that if there are no queued messages, this will always block
           until we're ready to read, rather than returning `Write`, even if
           stdout is capable of being written to. Furthermore, we will never
           need to queue a message to be written until we have read
           something. *)
        let readable_fds, _, _ = Unix.select read_fds write_fds [] (-1.0) in
        let ready_for_read = not (List.is_empty readable_fds) in
        if ready_for_read
        then Read
        else Write
      end
    in

    let should_continue = match operation with
      | Read -> begin
        try
          let timestamped_json = internal_read_message reader in
          Queue.push timestamped_json messages_to_send;
          true
        with e ->
          let message = Printexc.to_string e in
          let stack = Printexc.get_backtrace () in
          let edata = { Marshal_tools.message; stack; } in
          let (should_continue, marshal) = match e with
            | Hh_json.Syntax_error _ -> true, Recoverable_exception edata
            | _ -> false, Fatal_exception edata
          in
          Marshal_tools.to_fd_with_preamble out_fd marshal;
          should_continue
        end
      | Write ->
        assert (not (Queue.is_empty messages_to_send));
        let timestamped_json = Queue.pop messages_to_send in
        (* We can assume that the entire write will succeed, since otherwise
           Marshal_tools.to_fd_with_preamble will throw an exception. *)
        Marshal_tools.to_fd_with_preamble out_fd (Timestamped_json timestamped_json);
        true
    in
    if should_continue then loop ()
  in
  loop ()

(*  Main function for the daemon process. *)
let internal_run_daemon
    (_dummy_param : unit)
    (_ic, (oc : queue_message Daemon.out_channel)) =
  Printexc.record_backtrace true;
  try
    internal_run_daemon' oc
  with e ->
    (* An exception that's gotten here is not simply a parse error, but
       something else, so we should terminate the daemon at this point. *)
    let message = Printexc.to_string e in
    let stack = Printexc.get_backtrace () in
    try
      let out_fd = Daemon.descr_of_out_channel oc in
      Marshal_tools.to_fd_with_preamble out_fd
        (Fatal_exception { Marshal_tools.message; stack; })
    with _ ->
      (* There may be a broken pipe, for example. We should just give up on
         reporting the error. *)
      ()

let internal_entry_point : (unit, unit, queue_message) Daemon.entry =
  Daemon.register_entry_point "Jsonrpc" internal_run_daemon

let internal_make_queue () : internal_queue =
  let handle = Daemon.spawn
    ~channel_mode:`pipe
    (* We don't technically need to inherit stdout or stderr, but this might be
       useful in the event that we throw an unexpected exception in the daemon.
       It's also useful for print-statement debugging of the daemon. *)
    (Unix.stdin, Unix.stdout, Unix.stderr)
    internal_entry_point
    ()
  in
  let (ic, _) = handle.Daemon.channels in
  {
    daemon_in_fd = Daemon.descr_of_in_channel ic;
    messages = Queue.create ();
  }


module Make (State: sig type t end) : sig
  type kind = Request | Notification | Response
  val kind_to_string : kind -> string

  type message = {
    json : Hh_json.json; (* the json payload *)
    timestamp : float; (* time this message arrived at stdin *)
    (* Following fields are decompositions of 'json'... *)
    kind : kind;
    method_ : string; (* mandatory for request+notification; empty otherwise *)
    id : Hh_json.json option; (* mandatory for request+response *)
    params : Hh_json.json option; (* optional for request+notification *)
    result : Hh_json.json option; (* optional for response *)
    error: Hh_json.json option; (* optional for response *)
  }

  val parse_message : json:Hh_json.json -> timestamp:float -> message

  type queue
  val make_queue : unit -> queue (* must call Daemon.entry_point at start of your main *)
  val get_read_fd : queue -> Unix.file_descr (* can be used for 'select' *)
  val has_message : queue -> bool
  val get_message : queue -> [>
  | `Message of message
  | `Fatal_exception of Marshal_tools.remote_exception_data
  | `Recoverable_exception of Marshal_tools.remote_exception_data ]

  type on_result = result:Hh_json.json option -> State.t -> State.t
  type on_error = code:int -> message:string -> data:Hh_json.json option -> State.t -> State.t
  type cancellation_token = unit -> unit

  (* 'respond to_this with_that' is for replying to a JsonRPC request. It will send either *)
  (* a response or an error depending on whether 'with_that' has an error id in it.        *)
  val respond : message -> Hh_json.json -> unit
  (* notify/request are for initiating JsonRPC messages *)
  val notify : string -> Hh_json.json -> unit
  val request : on_result -> on_error -> string -> Hh_json.json -> cancellation_token

  (* For logging purposes, you can get a copy of which JsonRPC message was last    *)
  (* sent by this module - be it a response, notification, request or cancellation *)
  val last_sent : unit -> Hh_json.json option
  val clear_last_sent : unit -> unit

  (* if the controlling loop received a response message, it should call  *)
  (* into dispatch_response, to trigger the appropriate callback that had *)
  (* been passed to the corresponding 'request' method.                   *)
  val dispatch_response : message -> State.t -> State.t
  (* For logging purposes, when you receive a response message, you can   *)
  (* also see what outgoing request method it is in response to.          *)
  val get_method_for_response : message -> string
end = struct

  type kind = Request | Notification | Response

  let kind_to_string (kind: kind) : string =
    match kind with
    | Request -> "Request"
    | Notification -> "Notification"
    | Response -> "Response"

  type message = {
    json : Hh_json.json; (* the json payload *)
    timestamp : float; (* time this message arrived at stdin *)
    (* Following fields are decompositions of 'json'... *)
    kind : kind;
    method_ : string; (* mandatory for request+notification; empty otherwise *)
    id : Hh_json.json option; (* mandatory for request+response *)
    params : Hh_json.json option; (* optional for request+notification *)
    result : Hh_json.json option; (* optional for response *)
    error: Hh_json.json option; (* optional for response *)
  }


  let parse_message ~(json: Hh_json.json) ~(timestamp: float) : message =
    let id = Hh_json_helpers.try_get_val "id" json in
    let method_opt = Hh_json_helpers.try_get_val "method" json
      |> Option.map ~f:Hh_json.get_string_exn in
    let method_ = Option.value method_opt ~default:"" in (* is easier to consume *)
    let params = Hh_json_helpers.try_get_val "params" json in
    let result = Hh_json_helpers.try_get_val "result" json in
    let error = Hh_json_helpers.try_get_val "error" json in
    (* Following categorization mostly mirrors that of VSCode except that     *)
    (* VSCode allows number+string+null ID for response, but we allow any ID. *)
    let kind = match id, method_opt, result, error with
      | Some _id, Some _method, _, _            -> Request
      | None,     Some _method, _, _            -> Notification
      | _,        _,            Some _result, _ -> Response
      | _,        _,            _, Some _error  -> Response
      | _ -> raise (Hh_json.Syntax_error "Not JsonRPC")
    in
    { json; timestamp; id; method_; params; result; error; kind; }


  (************************************************)
  (* Queue functions that run in the main process *)
  (************************************************)

  type queue = internal_queue

  let make_queue () =
    internal_make_queue ()

  let get_read_fd (queue : queue) : Unix.file_descr =
    queue.daemon_in_fd

  (* Read a message into the queue, and return the just-read message. *)
  let read_single_message_into_queue_blocking (message_queue : queue) =
    let message =
      try Marshal_tools.from_fd_with_preamble message_queue.daemon_in_fd
      with End_of_file as e ->
        (* This is different from when the client hangs up. It handles the case
           that the daemon process exited: for example, if it was killed. *)
        let message = Printexc.to_string e in
        let stack = Printexc.get_backtrace () in
        Fatal_exception { Marshal_tools.message; stack; }
    in

    Queue.push message message_queue.messages;
    message

  let rec read_messages_into_queue_nonblocking (message_queue : queue) : unit =
    let readable_fds, _, _ = Unix.select [message_queue.daemon_in_fd] [] [] 0.0 in
    if not (List.is_empty readable_fds) then begin
      (* We're expecting this not to block because we just checked `Unix.select`
         to make sure that there's something there. *)
      let message = read_single_message_into_queue_blocking message_queue in

      (* Now read any more messages that might be queued up. Only try to read more
         messages if the daemon is still available to read from. Otherwise, we may
         infinite loop as a result of `Unix.select` returning that a file
         descriptor is available to read on. *)
      match message with
      | Fatal_exception _ -> ()
      | _ -> read_messages_into_queue_nonblocking message_queue;
    end

  let has_message (queue : queue) : bool =
    read_messages_into_queue_nonblocking queue;
    not (Queue.is_empty queue.messages)

  let get_message (queue : queue) =
    (* Read one in a blocking manner to ensure that we have one. *)
    if Queue.is_empty queue.messages
    then ignore (read_single_message_into_queue_blocking queue);
    (* Then read any others that got queued up so that we can see the maximum
       number of messages at once for invalidation purposes. *)
    read_messages_into_queue_nonblocking queue;

    let item = Queue.pop queue.messages in
    match item with
    | Timestamped_json {tj_json; tj_timestamp;} -> `Message (parse_message tj_json tj_timestamp)
    | Fatal_exception data -> `Fatal_exception data
    | Recoverable_exception data -> `Recoverable_exception data


  (************************************************)
  (* Output functions for respond+notify          *)
  (************************************************)

  let last_sent_ref : Hh_json.json option ref = ref None

  let clear_last_sent () : unit =
    last_sent_ref := None

  let last_sent () : Hh_json.json option =
    !last_sent_ref

  (* respond: sends either a Response or an Error message, according
     to whether the json has an error-code or not. *)
  let respond
      (in_response_to: message)
      (result_or_error: Hh_json.json)
    : unit =
    let open Hh_json in
    let is_error = match result_or_error with
      | JSON_Object _ ->
        Hh_json_helpers.try_get_val "code" result_or_error
          |> Option.is_some
      | _ -> false in
    let response = JSON_Object (
      ["jsonrpc", JSON_String "2.0"]
      @
        ["id", Option.value in_response_to.id ~default:JSON_Null]
      @
        (if is_error then ["error", result_or_error] else ["result", result_or_error])
    )
    in
    last_sent_ref := Some response;
    response |> Hh_json.json_to_string |> Http_lite.write_message stdout


  (* notify: sends a Notify message *)
  let notify (method_: string) (params: Hh_json.json)
    : unit =
    let open Hh_json in
    let message = JSON_Object [
      "jsonrpc", JSON_String "2.0";
      "method", JSON_String method_;
      "params", params;
    ]
    in
    last_sent_ref := Some message;
    message |> Hh_json.json_to_string |> Http_lite.write_message stdout


  (************************************************)
  (* Output functions for request                 *)
  (************************************************)

  type on_result = result:Hh_json.json option -> State.t -> State.t
  type on_error = code:int -> message:string -> data:Hh_json.json option -> State.t -> State.t
  type cancellation_token = unit -> unit

  module Callback = struct
    type t = {
      method_: string;
      on_result: on_result;
      on_error: on_error;
    }
  end

  let requests_counter: IMap.key ref = ref 0
  let requests_outstanding: Callback.t IMap.t ref = ref IMap.empty

  (* request: produce a Request message; returns a method you can call to cancel it *)
  let request
      (on_result: on_result)
      (on_error: on_error)
      (method_: string)
      (params: Hh_json.json)
    : cancellation_token =
    incr requests_counter;
    let callback = { Callback.method_; on_result; on_error; } in
    let request_id = !requests_counter in
    requests_outstanding := IMap.add request_id callback !requests_outstanding;

    let open Hh_json in
    let message = JSON_Object [
      "jsonrpc", string_ "2.0";
      "id", int_ request_id;
      "method", string_ method_;
      "params", params;
    ]
    in
    let cancel_message = JSON_Object [
      "jsonrpc", string_ "2.0";
      "method", string_ "$/cancelRequest";
      "params", JSON_Object [
        "id", int_ request_id;
      ]
    ]
    in
    last_sent_ref := Some message;
    message |> Hh_json.json_to_string |> Http_lite.write_message stdout;

    let cancel () =
      last_sent_ref := Some cancel_message;
      cancel_message |> Hh_json.json_to_string |> Http_lite.write_message stdout
    in
    cancel

  let get_request_for_response (response: message) =
    match response.id with
    | Some (Hh_json.JSON_Number s) -> begin
        try
          let id = int_of_string s in
          Option.map (IMap.get id !requests_outstanding) ~f:(fun v -> (id, v))
        with Failure _ -> None
      end
    | _ -> None

  let get_method_for_response (response: message) : string =
    match (get_request_for_response response) with
    | Some (_, callback) -> callback.Callback.method_
    | None -> ""

  let dispatch_response
      (response: message)
      (state: State.t)
    : State.t =
    let open Callback in
    let id, on_result, on_error = match (get_request_for_response response) with
      | Some (id, callback) -> (id, callback.on_result, callback.on_error)
      | None -> failwith "response to non-existent id"
    in
    requests_outstanding := IMap.remove id !requests_outstanding;
    if Option.is_some response.error then
      let code = Option.bind response.error (Hh_json_helpers.try_get_val "code")
        |> Option.map ~f:Hh_json.get_number_int_exn in
      let message = Option.bind response.error (Hh_json_helpers.try_get_val "message")
        |> Option.map ~f:Hh_json.get_string_exn in
      let data = Option.bind response.error (Hh_json_helpers.try_get_val "data") in
      match code, message, data with
      | Some code, Some message, data -> on_error code message data state
      | _ -> failwith "malformed error response"
    else
      on_result response.result state

end
