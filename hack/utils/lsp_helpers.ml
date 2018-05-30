(* A few helpful wrappers around LSP *)

open Lsp
open Lsp_fmt

let progress_and_actionRequired_counter = ref 0

(************************************************************************)
(** Conversions                                                        **)
(************************************************************************)

let url_scheme_regex = Str.regexp "^\\([a-zA-Z][a-zA-Z0-9+.-]+\\):"
(* this requires schemes with 2+ characters, so "c:\path" isn't considered a scheme *)

let lsp_uri_to_path (uri: string) : string =
  if Str.string_match url_scheme_regex uri 0 then
    let scheme = Str.matched_group 1 uri in
    if scheme = "file" then
      File_url.parse uri
    else
      raise (Error.InvalidParams (Printf.sprintf "Not a valid file url '%s'" uri))
  else
    uri

let path_to_lsp_uri (path: string) ~(default_path: string): string =
  if path = "" then File_url.create default_path
  else File_url.create path

let lsp_textDocumentIdentifier_to_filename
    (identifier: Lsp.TextDocumentIdentifier.t)
  : string =
  let open Lsp.TextDocumentIdentifier in
  lsp_uri_to_path identifier.uri

let apply_changes (text: string) (contentChanges: DidChange.textDocumentContentChangeEvent list)
  : (string, string * Utils.callstack) result =
  let lsp_position_to_fc (pos: Lsp.position) : File_content.position =
    { File_content.
      line = pos.Lsp.line + 1;  (* LSP is 0-based; File_content is 1-based. *)
      column = pos.Lsp.character + 1;
    } in
  let lsp_range_to_fc (range: Lsp.range) : File_content.range =
    { File_content.
      st = lsp_position_to_fc range.Lsp.start;
      ed = lsp_position_to_fc range.Lsp.end_;
    } in
  let lsp_edit_to_fc (edit: Lsp.DidChange.textDocumentContentChangeEvent) : File_content.text_edit =
    { File_content.
      range = Option.map edit.DidChange.range ~f:lsp_range_to_fc;
      text = edit.DidChange.text;
    } in
  let edits = List.map lsp_edit_to_fc contentChanges
  in
  File_content.edit_file text edits

let apply_changes_unsafe text (contentChanges: DidChange.textDocumentContentChangeEvent list)
  : string =
  match apply_changes text contentChanges with
  | Ok r -> r
  | Error (e, _stack) -> failwith e


(************************************************************************)
(** Accessors                                                          **)
(************************************************************************)

let get_root (p: Lsp.Initialize.params) : string =
  let open Lsp.Initialize in
  match p.rootUri, p.rootPath with
  | Some uri, _ -> lsp_uri_to_path uri
  | None, Some path -> path
  | None, None -> failwith "Initialize params missing root"

let supports_progress (p: Lsp.Initialize.params) : bool =
  let open Lsp.Initialize in
  p.client_capabilities.window.progress

let supports_actionRequired (p: Lsp.Initialize.params) : bool =
  let open Lsp.Initialize in
  p.client_capabilities.window.actionRequired

let supports_snippets (p: Lsp.Initialize.params) : bool =
  let open Lsp.Initialize in
  p.client_capabilities.textDocument.completion.completionItem.snippetSupport

let supports_connectionStatus (p: Lsp.Initialize.params) : bool =
  let open Lsp.Initialize in
  p.client_capabilities.telemetry.connectionStatus

(************************************************************************)
(** Wrappers for some LSP methods                                      **)
(************************************************************************)

let telemetry (writer: Jsonrpc.writer) (level: MessageType.t) (message: string) : unit =
  print_logMessage level message |> Jsonrpc.notify writer "telemetry/event"

let telemetry_error (writer: Jsonrpc.writer) = telemetry writer MessageType.ErrorMessage
let telemetry_log (writer: Jsonrpc.writer) = telemetry writer MessageType.LogMessage

let log (writer: Jsonrpc.writer) (level: MessageType.t) (message: string) : unit =
  print_logMessage level message |> Jsonrpc.notify writer "window/logMessage"

let log_error (writer: Jsonrpc.writer) = log writer MessageType.ErrorMessage
let log_warning (writer: Jsonrpc.writer) = log writer MessageType.WarningMessage
let log_info (writer: Jsonrpc.writer) = log writer MessageType.InfoMessage

let dismiss_diagnostics (writer: Jsonrpc.writer) (diagnostic_uris: SSet.t) : SSet.t =
  let dismiss_one (uri: string) : unit =
    let message = { Lsp.PublishDiagnostics.uri; diagnostics = []; } in
    message |> print_diagnostics |> Jsonrpc.notify writer "textDocument/publishDiagnostics"
  in
  SSet.iter dismiss_one diagnostic_uris;
  SSet.empty

let notify_connectionStatus
    (p: Lsp.Initialize.params)
    (writer: Jsonrpc.writer)
    (wasConnected: bool)
    (isConnected: bool)
  : bool =
  if supports_connectionStatus p && wasConnected <> isConnected then begin
    let message = { Lsp.ConnectionStatus.isConnected; } in
    message |> print_connectionStatus |> Jsonrpc.notify writer "telemetry/connectionStatus"
  end;
  isConnected

(* notify_progress: for sending/updating/closing progress messages.         *)
(* To start a new indicator: id=None, message=Some, and get back the new id *)
(* To update an existing one: id=Some, message=Some, and get back same id   *)
(* To close an existing one: id=Some, message=None, and get back None       *)
(* No-op, for convenience: id=None, message=None, and you get back None     *)
(* messages. To start a new progress notifier, put id=None and message=Some *)

let notify_progress_raw
    (state: 'a)
    (p: Lsp.Initialize.params)
    (writer: 'a -> Progress.params -> 'a)
    (id: Progress.t)
    (label: string option)
  : 'a * Progress.t =
  match id, label with
  | Progress.None, Some label ->
    if supports_progress p then
      let () = incr progress_and_actionRequired_counter in
      let id = !progress_and_actionRequired_counter in
      let msg = { Progress.id; label = Some label; } in
      let state = writer state msg in
      (state, Progress.Some { id; label; })
    else
      (state, Progress.None)
  | Progress.Some { id; label; }, Some new_label when label = new_label ->
    (state, Progress.Some { id; label; })
  | Progress.Some { id; _ }, Some label ->
    let msg = { Progress.id; label = Some label; } in
    let state = writer state msg in
    (state, Progress.Some { id; label; })
  | Progress.Some { id; _ }, None ->
    let msg = { Progress.id; label = None; } in
    let state = writer state msg in
    (state, Progress.None)
  | Progress.None, None ->
    (state, Progress.None)

let notify_progress
    (p: Lsp.Initialize.params)
    (writer: Jsonrpc.writer)
    (id: Progress.t)
    (label: string option)
  : Progress.t =
  let writer_wrapper () params =
    let json = print_progress params.Progress.id params.Progress.label in
    Jsonrpc.notify writer "window/progress" json in
  let ((), id) = notify_progress_raw () p writer_wrapper id label
  in
  id

let notify_actionRequired
    (p: Lsp.Initialize.params)
    (writer: Jsonrpc.writer)
    (id: ActionRequired.t)
    (label: string option)
  : ActionRequired.t =
  match id, label with
  | ActionRequired.None, Some label ->
    if supports_actionRequired p then
      let () = incr progress_and_actionRequired_counter in
      let id = !progress_and_actionRequired_counter in
      let () = print_actionRequired id (Some label)
        |> Jsonrpc.notify writer "window/actionRequired" in
      ActionRequired.Some { id; label; }
    else
      ActionRequired.None
  | ActionRequired.Some { id; label; }, Some new_label when label = new_label ->
    ActionRequired.Some { id; label; }
  | ActionRequired.Some { id; _ }, Some label ->
    print_actionRequired id (Some label) |> Jsonrpc.notify writer "window/actionRequired";
    ActionRequired.Some { id; label; }
  | ActionRequired.Some { id; _ }, None ->
    print_actionRequired id None |> Jsonrpc.notify writer "window/actionRequired";
    ActionRequired.None
  | ActionRequired.None, None ->
    ActionRequired.None
