open Core

type jsonrpc_message_kind =
 | Request
 | Notification
 | Response

let kind_to_string = function
 | Request -> "Request"
 | Notification -> "Notification"
 | Response -> "Response"

type jsonrpc_message = {
  timestamp : float;
  message_json_for_logging : string; (* the json payload *)
  kind : jsonrpc_message_kind;
  method_ : string; (* mandatory for request+notification; empty otherwise *)
  id : Hh_json.json option; (* mandatory for request+response *)
  params : Hh_json.json option; (* optional for request+notification *)
  result : Hh_json.json option; (* optional for response *)
  error: Hh_json.json option; (* optional for response *)
}

type result =
  | Message of jsonrpc_message
  | Fatal_exception of Marshal_tools.remote_exception_data
  | Recoverable_exception of Marshal_tools.remote_exception_data

type daemon_message =
  | Daemon_message of jsonrpc_message
  | Daemon_fatal_exception of Marshal_tools.remote_exception_data
  | Daemon_recoverable_exception of Marshal_tools.remote_exception_data

type daemon_operation =
  | Read
  | Write

type t = {
  (* The file descriptor that the main process uses to read the messages sent by
     the daemon. *)
  daemon_in_fd : Unix.file_descr;

  messages : result Queue.t;
}

(********************************************)
(* Functions that run in the daemon process *)
(********************************************)

(* Try to read a message from the daemon's stdin, which is where all of the
   editor messages can be read from. May throw if the message is malformed. *)
let read_message (reader : Buffered_line_reader.t) : jsonrpc_message =
  let message = reader |> Http_lite.read_message_utf8 in
  let json = Hh_json.json_of_string message in

  let id = Hh_json_helpers.try_get_val "id" json in
  let method_ = Hh_json_helpers.try_get_val "method" json
    |> Option.map ~f:Hh_json.get_string_exn in
  let params = Hh_json_helpers.try_get_val "params" json in
  let result = Hh_json_helpers.try_get_val "result" json in
  let error = Hh_json_helpers.try_get_val "error" json in
  (* Following categorization mostly mirrors that of VSCode except that     *)
  (* VSCode allows number+string+null ID for response, but we allow any ID. *)
  (* https://github.com/Microsoft/vscode-languageserver-node/blob/master/jsonrpc/src/messages.ts *)
  let kind = match id, method_, result, error with
    | Some _id, Some _method, _, _            -> Request
    | None,     Some _method, _, _            -> Notification
    | _,        _,            Some _result, _ -> Response
    | _,        _,            _, Some _error  -> Response
    | _ -> raise (Hh_json.Syntax_error "Not JsonRPC")
  in
  {
    timestamp = Unix.gettimeofday ();
    message_json_for_logging = Hh_json.json_truncate_string message;
    id;
    method_ = Option.value method_ ~default:""; (* is easier to consume *)
    params;
    result;
    error;
    kind;
  }


(* Reads messages from the editor on stdin, parses them, and sends them to the
   main process.

   This runs in a different process because we also timestamp the messages, so
   we need to read them as soon as they come in. That is, we can't wait for any
   server computation to finish if we want to get an accurate timestamp. *)
let run_daemon' (oc : daemon_message Daemon.out_channel) : unit =
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
          let message = read_message reader in
          Queue.push message messages_to_send;
          true
        with e ->
          let message = Printexc.to_string e in
          let stack = Printexc.get_backtrace () in
          let edata = { Marshal_tools.message; stack; } in
          let (should_continue, marshal) = match e with
            | Hh_json.Syntax_error _ -> true, Daemon_recoverable_exception edata
            | _ -> false, Daemon_fatal_exception edata
          in
          Marshal_tools.to_fd_with_preamble out_fd marshal;
          should_continue
        end
      | Write ->
        assert (not (Queue.is_empty messages_to_send));
        let message = Queue.pop messages_to_send in
        (* We can assume that the entire write will succeed, since otherwise
           Marshal_tools.to_fd_with_preamble will throw an exception. *)
        Marshal_tools.to_fd_with_preamble out_fd (Daemon_message message);
        true
    in
    if should_continue then loop ()
  in
  loop ()

(*  Main function for the daemon process. *)
let run_daemon
    (_dummy_param : unit)
    (_ic, (oc : daemon_message Daemon.out_channel)) =
  Printexc.record_backtrace true;
  try
    run_daemon' oc
  with e ->
    (* An exception that's gotten here is not simply a parse error, but
       something else, so we should terminate the daemon at this point. *)
    let message = Printexc.to_string e in
    let stack = Printexc.get_backtrace () in
    try
      let out_fd = Daemon.descr_of_out_channel oc in
      Marshal_tools.to_fd_with_preamble out_fd
        (Daemon_fatal_exception { Marshal_tools.message; stack; })
    with _ ->
      (* There may be a broken pipe, for example. We should just give up on
         reporting the error. *)
      ()

let entry_point = Daemon.register_entry_point "ClientMessageQueue" run_daemon

(******************************************)
(* Functions that run in the main process *)
(******************************************)

let make () : t =
  let handle = Daemon.spawn
    ~channel_mode:`pipe
    (* We don't technically need to inherit stdout or stderr, but this might be
       useful in the event that we throw an unexpected exception in the daemon.
       It's also useful for print-statement debugging of the daemon. *)
    (Unix.stdin, Unix.stdout, Unix.stderr)
    entry_point
    ()
  in
  let (ic, _) = handle.Daemon.channels in
  {
    daemon_in_fd = Daemon.descr_of_in_channel ic;
    messages = Queue.create ();
  }

let get_read_fd (message_queue : t) : Unix.file_descr =
  message_queue.daemon_in_fd

(* Read a message into the queue, and return the just-read message. *)
let read_single_message_into_queue_blocking (message_queue : t) : result =
  let message =
    try Marshal_tools.from_fd_with_preamble message_queue.daemon_in_fd
    with End_of_file as e ->
      (* This is different from when the client hangs up. It handles the case
         that the daemon process exited: for example, if it was killed. *)
      let message = Printexc.to_string e in
      let stack = Printexc.get_backtrace () in
      Daemon_fatal_exception { Marshal_tools.message; stack; }
  in

  let message = match message with
    | Daemon_message message -> Message message
    | Daemon_fatal_exception edata -> Fatal_exception edata
    | Daemon_recoverable_exception edata -> Recoverable_exception edata
  in
  Queue.push message message_queue.messages;
  message

let rec read_messages_into_queue_nonblocking (message_queue : t) : unit =
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

let has_message (queue : t) : bool =
  read_messages_into_queue_nonblocking queue;
  not (Queue.is_empty queue.messages)

let get_message (queue : t) : result =
  (* Read one in a blocking manner to ensure that we have one. *)
  if Queue.is_empty queue.messages
  then ignore (read_single_message_into_queue_blocking queue);
  (* Then read any others that got queued up so that we can see the maximum
     number of messages at once for invalidation purposes. *)
  read_messages_into_queue_nonblocking queue;

  Queue.pop queue.messages
