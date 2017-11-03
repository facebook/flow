(* A few helpful wrappers around LSP *)

open Lsp
open Lsp_fmt

module Make (P: sig
  type t  (* State type, used for request_showMessage *)
  val get : unit -> Hh_json.json option  (* params of the initialize request *)
end) = struct

  module Jsonrpc = Jsonrpc.Make(struct type t = P.t end)

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


  (************************************************************************)
  (** Accessors                                                          **)
  (************************************************************************)

  let initialize_params_memoized : Initialize.params option ref = ref None

  let get_initialize_params () : Initialize.params option =
    if Option.is_none !initialize_params_memoized then begin
      let json_params = P.get () in
      let parsed_params = Lsp_fmt.parse_initialize json_params in
      initialize_params_memoized := Some parsed_params
    end;
    !initialize_params_memoized

  let get_root () : string option =
    let open Lsp.Initialize in
    match get_initialize_params () with
    | None -> None
    | Some params ->
      match params.rootUri with
      | Some uri -> Some (lsp_uri_to_path uri)
      | None -> params.rootPath

  let supports_progress () : bool =
    let open Lsp.Initialize in
    Option.value_map (get_initialize_params ())
      ~default:false ~f:(fun params -> params.client_capabilities.window.progress)

  let supports_actionRequired () : bool =
    let open Lsp.Initialize in
    Option.value_map (get_initialize_params ())
      ~default:false ~f:(fun params -> params.client_capabilities.window.actionRequired)

  let supports_snippets () : bool =
    let open Lsp.Initialize in
    Option.value_map (get_initialize_params ())
      ~default:false ~f:(fun params ->
      params.client_capabilities.textDocument.completion.completionItem.snippetSupport)


  (************************************************************************)
  (** Wrappers for some LSP methods                                      **)
  (************************************************************************)

  let telemetry (level: MessageType.t) (message: string) : unit =
    print_logMessage level message |> Jsonrpc.notify "telemetry/event"

  let telemetry_error = telemetry MessageType.ErrorMessage
  let telemetry_log = telemetry MessageType.LogMessage

  let log (level: MessageType.t) (message: string) : unit =
    print_logMessage level message |> Jsonrpc.notify "window/logMessage"

  let log_error = log MessageType.ErrorMessage
  let log_warning = log MessageType.WarningMessage
  let log_info = log MessageType.InfoMessage

  let dismiss_diagnostics (diagnostic_uris: SSet.t) : SSet.t =
    let dismiss_one (uri: string) : unit =
      let message = { Lsp.PublishDiagnostics.uri; diagnostics = []; } in
      message |> print_diagnostics |> Jsonrpc.notify "textDocument/publishDiagnostics"
    in
    SSet.iter dismiss_one diagnostic_uris;
    SSet.empty


  (* request_showMessage: pops up a dialog *)
  let request_showMessage
      (on_result: Jsonrpc.on_result)
      (on_error: Jsonrpc.on_error)
      (type_: MessageType.t)
      (message: string)
      (titles: string list)
    : ShowMessageRequest.t =
    let req = Lsp_fmt.print_showMessageRequest type_ message titles in
    let cancel = Jsonrpc.request on_result on_error "window/showMessageRequest" req in
    ShowMessageRequest.Some { cancel; }

  let dismiss_showMessageRequest (dialog: ShowMessageRequest.t) : ShowMessageRequest.t =
    begin match dialog with
      | ShowMessageRequest.Some { cancel; _ } -> cancel ()
      | ShowMessageRequest.None -> ()
    end;
    ShowMessageRequest.None


  (* notify_progress: for sending/updating/closing progress messages.         *)
  (* To start a new indicator: id=None, message=Some, and get back the new id *)
  (* To update an existing one: id=Some, message=Some, and get back same id   *)
  (* To close an existing one: id=Some, message=None, and get back None       *)
  (* No-op, for convenience: id=None, message=None, and you get back None     *)
  (* messages. To start a new progress notifier, put id=None and message=Some *)
  let progress_and_actionRequired_counter = ref 0

  let notify_progress (id: Progress.t) (label: string option) : Progress.t =
    match id, label with
    | Progress.None, Some label ->
      if supports_progress () then
        let () = incr progress_and_actionRequired_counter in
        let id = !progress_and_actionRequired_counter in
        let () = print_progress id (Some label) |> Jsonrpc.notify "window/progress" in
        Progress.Some { id; label; }
      else
        Progress.None
    | Progress.Some { id; label; }, Some new_label when label = new_label ->
      Progress.Some { id; label; }
    | Progress.Some { id; _ }, Some label ->
      print_progress id (Some label) |> Jsonrpc.notify "window/progress";
      Progress.Some { id; label; }
    | Progress.Some { id; _ }, None ->
      print_progress id None |> Jsonrpc.notify "window/progress";
      Progress.None
    | Progress.None, None ->
      Progress.None

  let notify_actionRequired (id: ActionRequired.t) (label: string option) : ActionRequired.t =
    match id, label with
    | ActionRequired.None, Some label ->
      if supports_actionRequired () then
        let () = incr progress_and_actionRequired_counter in
        let id = !progress_and_actionRequired_counter in
        let () = print_actionRequired id (Some label) |> Jsonrpc.notify "window/actionRequired" in
        ActionRequired.Some { id; label; }
      else
        ActionRequired.None
    | ActionRequired.Some { id; label; }, Some new_label when label = new_label ->
      ActionRequired.Some { id; label; }
    | ActionRequired.Some { id; _ }, Some label ->
      print_actionRequired id (Some label) |> Jsonrpc.notify "window/actionRequired";
      ActionRequired.Some { id; label; }
    | ActionRequired.Some { id; _ }, None ->
      print_actionRequired id None |> Jsonrpc.notify "window/actionRequired";
      ActionRequired.None
    | ActionRequired.None, None ->
      ActionRequired.None

end
