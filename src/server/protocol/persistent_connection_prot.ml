(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type client_id = int

type error_kind = ExpectedError | UnexpectedError
type error_info = error_kind * string * Utils.callstack

(** For LSP work-items, we keep metadata about requests, to help us log better telemetry.
   After the work has been handled, we fill out the second part of the metadata.
*)
type metadata = {
  (* when did this work-item get triggered? *)
  start_wall_time: float;
  (* What was the thing that triggered this work-item *)
  start_json_truncated: Hh_json.json;
  (* What was the state of the server at the time the work-item was triggered? *)
  (* Might be None e.g. if the server was down at the time or if we don't know *)
  start_server_status: ServerStatus.status option;
  start_watcher_status: FileWatcherStatus.status option;
  (* And what was the state of the lspCommand client? Is optional only to save *)
  (* space in the obvious cases that don't need explanation. *)
  start_lsp_state: string option;
  start_lsp_state_reason: string option;

  (* If handling the workitem resulted in error, what was that error? *)
  error_info: error_info option;
  (* Once the workitem has been handling, how long did it take on the server? *)
  profiling: Profiling_js.finished option;
  (* and on the client? *)
  client_profiling: Profiling_js.finished option;
  (* Did the handler for this workitem provide any extra data? *)
  extra_data: (string * Hh_json.json) list;
}


type request =
  | Subscribe
  | Autocomplete of (File_input.t * (* request id *) int)
  | DidOpen of (* filename *) string Nel.t
  | DidClose of (* filename *) string Nel.t
  | LspToServer of Lsp.lsp_message * metadata (* requests, notifications, responses from client *)

let string_of_request = function
| Subscribe -> "subscribe"
| Autocomplete _ -> "autocomplete"
| DidOpen _ -> "didOpen"
| DidClose _ -> "didClose"
| LspToServer _ -> "lspToServer"

(* Whereas string_of_request returns a normalized description of the request, this returns a more
 * detailed description *)
let denorm_string_of_request = function
| Subscribe -> "subscribe"
| Autocomplete (f, _) -> Printf.sprintf "autocomplete %s" (File_input.filename_of_file_input f)
| DidOpen files -> Printf.sprintf "didOpen %s" (String.concat " " (Nel.to_list files))
| DidClose files -> Printf.sprintf "didClose %s" (String.concat " " (Nel.to_list files))
| LspToServer (incoming, _) -> Printf.sprintf "lspToServer %s"
    (Lsp_fmt.denorm_message_to_string incoming)


type response =
  | Errors of {errors: Errors.ErrorSet.t; warnings: Errors.ErrorSet.t}
  | StartRecheck
  | EndRecheck of ServerProt.Response.lazy_stats
  | AutocompleteResult of (ServerProt.Response.autocomplete_response * (* request id *) int)
  | DidOpenAck
  | DidCloseAck
  | ServerExit of FlowExitStatus.t (* only used for the subset of exists which client handles *)
  | LspFromServer of Lsp.lsp_message option * metadata
  | Please_hold of (ServerStatus.status * FileWatcherStatus.status)
  | EOF (* monitor is about to close the connection *)

let string_of_response = function
| Errors _ -> "errors"
| StartRecheck -> "startRecheck"
| EndRecheck _ -> "endRecheck"
| AutocompleteResult _ -> "autocompleteResult"
| DidOpenAck -> "didOpenAck"
| DidCloseAck -> "didCloseAck"
| ServerExit code -> "serverExit_" ^ (FlowExitStatus.to_string code)
| LspFromServer (None,_) -> "lspFromServer None"
| LspFromServer (Some msg,_) -> Printf.sprintf "lspFromServer %s"
    (Lsp_fmt.message_name_to_string msg)
| Please_hold (server_status, watcher_status) -> Printf.sprintf "pleaseHold_server=%s_watcher=%s"
    (ServerStatus.string_of_status server_status)
    (FileWatcherStatus.string_of_status watcher_status)
| EOF -> "EOF"
