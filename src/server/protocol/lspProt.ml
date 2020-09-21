(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type client_id = int

type error_kind =
  | ExpectedError
  | UnexpectedError

type error_info = error_kind * string * Utils.callstack

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
  (* If the workitem was handled on the server, how long did it take there? *)
  server_profiling: Profiling_js.finished option;
  (* and if it had work done on the client, how long there? *)
  client_duration: float option;
  (* Did the handler for this workitem provide any extra data? *)
  extra_data: (string * Hh_json.json) list;
  (* The logging context for the server *)
  server_logging_context: FlowEventLogger.logging_context option;
  (* LSP method (e.g. 'textDocument/completion') *)
  lsp_method_name: string;
  (* If we're tracking an interaction in the lsp process, this is the id of the interaction *)
  interaction_tracking_id: int option;
}

(** For LSP work-items, we keep metadata about requests, to help us log better telemetry.
   After the work has been handled, we fill out the second part of the metadata.
*)
let empty_metadata =
  {
    start_wall_time = 0.0;
    start_server_status = None;
    start_watcher_status = None;
    start_json_truncated = Hh_json.JSON_Object [];
    start_lsp_state = None;
    start_lsp_state_reason = None;
    error_info = None;
    server_profiling = None;
    client_duration = None;
    extra_data = [];
    server_logging_context = None;
    lsp_method_name = "";
    interaction_tracking_id = None;
  }

(* This is the reason why we start to do a recheck. Since rechecks can be combined together, there
 * may be multiple reasons for a single recheck *)
type recheck_reason =
  (* One file changed on disk. *)
  | Single_file_changed of { filename: string }
  (* More than one file changed on disk *)
  | Many_files_changed of { file_count: int }
  (* If we're using Watchman as the filewatcher, we can tell when the mergebase changed.
   * We can differentiate that from Many_files_changed *)
  | Rebased of {
      distance: int;
      file_count: int;
    }
  (* If try to autocomplete in foo.js and it's dependencies are unchecked, then we start a recheck
   * with a reason of Unchecked_dependencies { filename = "/path/to/foo.js"; } *)
  | Unchecked_dependencies of { filename: string }
  (* A lazy server started from saved state has an old dependency graph and has to update it *)
  | Lazy_init_update_deps
  (* A lazy server may decided to typecheck some files during init (like Watchman lazy mode will
   * typecheck files which have changed since the mergebase) *)
  | Lazy_init_typecheck
  (* At init when we do a full check *)
  | Full_init

let verbose_string_of_recheck_reason = function
  | Single_file_changed { filename } -> Printf.sprintf "1 file changed (%s)" filename
  | Many_files_changed { file_count } -> Printf.sprintf "%d files changed" file_count
  | Rebased { distance; file_count } ->
    Printf.sprintf "Rebased %d commits & %d files changed" distance file_count
  | Unchecked_dependencies { filename } -> Printf.sprintf "Unchecked dependencies of %s" filename
  | Lazy_init_update_deps -> "Lazy init update deps"
  | Lazy_init_typecheck -> "Lazy init typecheck"
  | Full_init -> "Full init"

let normalized_string_of_recheck_reason = function
  | Single_file_changed { filename = _ } -> "singleFileChanged"
  | Many_files_changed { file_count = _ } -> "manyFilesChanged"
  | Rebased { distance = _; file_count = _ } -> "rebased"
  | Unchecked_dependencies { filename = _ } -> "uncheckedDependencies"
  | Lazy_init_update_deps -> "lazyInitUpdateDeps"
  | Lazy_init_typecheck -> "lazyInitTypecheck"
  | Full_init -> "fullInit"

type request =
  | Subscribe
  | LspToServer of Lsp.lsp_message
  | LiveErrorsRequest of Lsp.DocumentUri.t

type request_with_metadata = request * metadata

(* requests, notifications, responses from client *)

let string_of_request = function
  | Subscribe -> "subscribe"
  | LspToServer msg -> Printf.sprintf "lspToServer %s" (Lsp_fmt.message_name_to_string msg)
  | LiveErrorsRequest uri -> Printf.sprintf "liveErrorsRequest %s" (Lsp.DocumentUri.to_string uri)

let string_of_request_with_metadata (request, _) = string_of_request request

let json_of_request =
  Hh_json.(
    function
    | (Subscribe, _) -> JSON_Object [("method", JSON_String "subscribe")]
    | (LspToServer _, metadata) -> metadata.start_json_truncated
    | (LiveErrorsRequest uri, metadata) ->
      JSON_Object
        [
          ("method", JSON_String "liveErrorsRequest");
          ("params", JSON_Object [("uri", JSON_String (Lsp.DocumentUri.to_string uri))]);
          ("trigger", metadata.start_json_truncated);
        ])

(* Why is the server sending us a list of errors *)
type errors_reason =
  (* Sending all the errors at the end of the recheck *)
  | End_of_recheck of { recheck_reasons: recheck_reason list }
  (* Streaming errors during recheck *)
  | Recheck_streaming of { recheck_reasons: recheck_reason list }
  (* Sometimes the env changes, which influences which errors we send to the lsp. For example, we
   * only send warnings for open files. When a file is opened or closed, we have to recalculate
   * which warnings to send and send the updated set. *)
  | Env_change
  (* The persistent client just subscribed to errors, so was sent the initial error list *)
  | New_subscription

type error_response_kind =
  | Canceled_error_response
  | Errored_error_response

type live_errors_failure = {
  live_errors_failure_kind: error_response_kind;
  live_errors_failure_reason: string;
  live_errors_failure_uri: Lsp.DocumentUri.t;
}

type live_errors_response = {
  live_errors: Errors.ConcreteLocPrintableErrorSet.t;
  live_warnings: Errors.ConcreteLocPrintableErrorSet.t;
  live_errors_uri: Lsp.DocumentUri.t;
}

type response =
  | LspFromServer of Lsp.lsp_message option
  | LiveErrorsResponse of (live_errors_response, live_errors_failure) result
  | UncaughtException of {
      request: request;
      exception_constructor: string;
      stack: string;
    }

type response_with_metadata = response * metadata

type notification_from_server =
  | Errors of {
      errors: Errors.ConcreteLocPrintableErrorSet.t;
      warnings: Errors.ConcreteLocPrintableErrorSet.t;
      errors_reason: errors_reason;
    }
  | StartRecheck
  | EndRecheck of ServerProt.Response.lazy_stats
  | ServerExit of FlowExitStatus.t  (** only used for the subset of exits which client handles *)
  | Please_hold of (ServerStatus.status * FileWatcherStatus.status)
  | EOF  (** monitor is about to close the connection *)

type message_from_server =
  | RequestResponse of response_with_metadata
  | NotificationFromServer of notification_from_server

let string_of_response = function
  | LspFromServer None -> "lspFromServer None"
  | LspFromServer (Some msg) ->
    Printf.sprintf "lspFromServer %s" (Lsp_fmt.message_name_to_string msg)
  | LiveErrorsResponse (Ok { live_errors; live_warnings; live_errors_uri; _ }) ->
    Printf.sprintf
      "liveErrorsResponse OK (%d errors, %d warnings) %s"
      (Errors.ConcreteLocPrintableErrorSet.cardinal live_errors)
      (Errors.ConcreteLocPrintableErrorSet.cardinal live_warnings)
      (Lsp.DocumentUri.to_string live_errors_uri)
  | LiveErrorsResponse
      (Error { live_errors_failure_kind; live_errors_failure_reason; live_errors_failure_uri }) ->
    Printf.sprintf
      "liveErrorsResponse %s %s %s"
      (match live_errors_failure_kind with
      | Canceled_error_response -> "CANCELED"
      | Errored_error_response -> "ERRORED")
      live_errors_failure_reason
      (Lsp.DocumentUri.to_string live_errors_failure_uri)
  | UncaughtException { request; exception_constructor; stack } ->
    Printf.sprintf
      "UncaughtException %s in handling `%s`: %s"
      exception_constructor
      (string_of_request request)
      stack

let string_of_message_from_server = function
  | RequestResponse (response, _) -> string_of_response response
  | NotificationFromServer notification ->
    begin
      match notification with
      | Errors _ -> "errors"
      | StartRecheck -> "startRecheck"
      | EndRecheck _ -> "endRecheck"
      | ServerExit code -> "serverExit_" ^ FlowExitStatus.to_string code
      | Please_hold (server_status, watcher_status) ->
        Printf.sprintf
          "pleaseHold_server=%s_watcher=%s"
          (ServerStatus.string_of_status server_status)
          (FileWatcherStatus.string_of_status watcher_status)
      | EOF -> "EOF"
    end
