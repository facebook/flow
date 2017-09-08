(* Reads JSON RPC messages from stdin in the background and timestamps them as
   they come in. *)

type t

type jsonrpc_message_kind =
 | Request
 | Notification
 | Response

val kind_to_string : jsonrpc_message_kind -> string

type jsonrpc_message = {
  (* The timestamp field is added when this message is read. It's not part of
     the JSON RPC spec. *)
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

(* Under the hood, this uses the Daemon module, so you must be sure to have
called `Daemon.entry_point` before trying to make a queue. *)
val make : unit -> t

(* Get the file descriptor that can be watched for a message coming in. *)
val get_read_fd : t -> Unix.file_descr

(* Whether we have a buffered message. *)
val has_message : t -> bool

(* Blocks until there is a message available, or an error occurs. *)
val get_message : t -> result
