(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Ephemeral socket connections expect a response to their requests. We use request_id to indicate
 * to which request a given response is replying *)
type request_id = string

type file_watcher_metadata = {
  changed_mergebase: bool option;
      (** [Some _] if we checked whether the mergebase changed, [None] if we didn't/can't ask the VCS *)
  missed_changes: bool;
}

let empty_file_watcher_metadata = { changed_mergebase = None; missed_changes = false }

let merge_file_watcher_metadata a b =
  let changed_mergebase =
    match (a.changed_mergebase, b.changed_mergebase) with
    | (None, None) -> None
    | (Some x, Some y) -> Some (x || y)
    | (Some true, None)
    | (None, Some true) ->
      Some true
    | (Some false, None)
    | (None, Some false) ->
      (* we don't know for sure, so return [None] *)
      None
  in
  { changed_mergebase; missed_changes = a.missed_changes || b.missed_changes }

type please_die_reason = MonitorExiting of (Exit.t * string)

(* These are the messages that the monitor sends to the server *)
type monitor_to_server_message =
  (* A request from an ephemeral socket connection. It expects a response *)
  | Request of request_id * ServerProt.Request.command_with_context
  (* A notification that there is a new persistent socket connection *)
  | NewPersistentConnection of LspProt.client_id * Lsp.Initialize.params
  (* A request from a persistent socket connection. It does not expect a response *)
  | PersistentConnectionRequest of LspProt.client_id * LspProt.request_with_metadata
  (* A notification that a persistent socket connection is dead *)
  | DeadPersistentConnection of LspProt.client_id
  (* The file watcher has noticed changes *)
  | FileWatcherNotification of {
      files: SSet.t;
      metadata: file_watcher_metadata option;
      initial: bool;
    }
  (* Monitor wants to kill the server but first asks nicely for the server to honorably kill itself *)
  | PleaseDie of please_die_reason

(* These are the messages that the server sends to the monitor *)
type server_to_monitor_message =
  (* A response to an ephemeral socket's request *)
  | Response of request_id * ServerProt.Response.response
  (* An exception was thrown while processing the request *)
  | RequestFailed of request_id * string
  (* A response to a persistent socket connection *)
  | PersistentConnectionResponse of LspProt.client_id * LspProt.message_from_server
  (* A notification of the server's current status *)
  | StatusUpdate of ServerStatus.status

(* These are the messages that the server sends to an ephemeral socket connection *)
type monitor_to_client_message =
  (* The response from the server *)
  | Data of ServerProt.Response.response
  (* The server threw an exception while processing the request *)
  | ServerException of string
  (* The server is currently busy. Please wait for a response *)
  | Please_hold of (ServerStatus.status * FileWatcherStatus.status)
