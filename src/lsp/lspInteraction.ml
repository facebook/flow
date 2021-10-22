(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module List = Base.List
open LspProt

(* Each interaction gets a unique id. *)
type id = int

(* What initiated this interaction *)
type trigger =
  | CodeAction of Lsp.lsp_id
  | Completion of Lsp.lsp_id
  | Definition of Lsp.lsp_id
  | DidChange
  | DidClose
  | DidOpen
  | DidSave
  | DocumentHighlight of Lsp.lsp_id
  | DocumentSymbol of Lsp.lsp_id
  | FindReferences of Lsp.lsp_id
  | Hover of Lsp.lsp_id
  | PushedErrorsEndOfRecheck of recheck_reason
  | PushedErrorsEnvChange
  | PushedErrorsNewSubscription
  | PushedErrorsRecheckStreaming of recheck_reason
  | Rage of Lsp.lsp_id
  | Rename of Lsp.lsp_id
  | ServerConnected
  | SelectionRange of Lsp.lsp_id
  | SignatureHelp of Lsp.lsp_id
  | TypeCoverage of Lsp.lsp_id
  | ExecuteCommand of Lsp.lsp_id
  | UnknownTrigger

(* Source of the trigger *)
type source =
  | Server
  | Client
  | UnknownSource

(* What was the result of this interaction. *)
type ux =
  | Canceled
  | CanceledPushingLiveNonParseErrors
  | Dismissed
  | Errored
  | ErroredPushingLiveNonParseErrors
  | ErroredPushingLiveParseErrors
  | PushedErrors
  | PushedLiveNonParseErrors
  | PushedLiveParseErrors
  | Responded
  | Timeout

(* What was the Flow server doing at a specific point in time *)
type server_status =
  | Stopped
  | Initializing
  | Rechecking
  | Ready

(* What were the IDE's buffers like at a specific point in time *)
type buffer_status =
  | NoOpenBuffers
  | NoUnsavedBuffers
  | UnsavedBuffers

(* A snapshot of the state of the world at a specific point in time. We record this at the start
 * and end of an interaction *)
type state = {
  time: float;
  server_status: server_status;
  buffer_status: buffer_status;
}

let string_of_trigger = function
  | CodeAction _ -> "codeAction"
  | Completion _ -> "completion"
  | Definition _ -> "definition"
  | DidChange -> "didChange"
  | DidClose -> "didClose"
  | DidOpen -> "didOpen"
  | DidSave -> "didSave"
  | DocumentHighlight _ -> "documentHighlight"
  | DocumentSymbol _ -> "documentSymbol"
  | FindReferences _ -> "findReferences"
  | Hover _ -> "hover"
  | PushedErrorsEndOfRecheck recheck_reason ->
    Printf.sprintf "endOfRecheck/%s" (normalized_string_of_recheck_reason recheck_reason)
  | PushedErrorsRecheckStreaming recheck_reason ->
    Printf.sprintf "recheckStreaming/%s" (normalized_string_of_recheck_reason recheck_reason)
  | PushedErrorsEnvChange -> "envChange"
  | PushedErrorsNewSubscription -> "newSubscription"
  | Rage _ -> "Rage"
  | Rename _ -> "Rename"
  | ServerConnected -> "ServerConnected"
  | SelectionRange _ -> "SelectionRange"
  | SignatureHelp _ -> "SignatureHelp"
  | TypeCoverage _ -> "TypeCoverage"
  | ExecuteCommand _ -> "ExecuteCommand"
  | UnknownTrigger -> "UnknownTrigger"

let lsp_id_of_trigger = function
  | CodeAction lsp_id
  | Completion lsp_id
  | Definition lsp_id
  | DocumentHighlight lsp_id
  | DocumentSymbol lsp_id
  | FindReferences lsp_id
  | Hover lsp_id
  | Rage lsp_id
  | Rename lsp_id
  | SelectionRange lsp_id
  | SignatureHelp lsp_id
  | TypeCoverage lsp_id
  | ExecuteCommand lsp_id ->
    Some lsp_id
  | DidChange
  | DidClose
  | DidOpen
  | DidSave
  | PushedErrorsEndOfRecheck _
  | PushedErrorsRecheckStreaming _
  | PushedErrorsEnvChange
  | PushedErrorsNewSubscription
  | ServerConnected
  | UnknownTrigger ->
    None

let string_of_ux = function
  | Canceled -> "Canceled"
  | CanceledPushingLiveNonParseErrors -> "CanceledPushingLiveNonParseErrors"
  | Dismissed -> "Dismissed"
  | Errored -> "Errored"
  | ErroredPushingLiveNonParseErrors -> "ErroredPushingLiveNonParseErrors"
  | ErroredPushingLiveParseErrors -> "ErroredPushingLiveParseErrors"
  | PushedErrors -> "PushedErrors"
  | PushedLiveNonParseErrors -> "PushedLiveNonParseErrors"
  | PushedLiveParseErrors -> "PushedLiveParseErrors"
  | Responded -> "Responded"
  | Timeout -> "Timeout"

let string_of_server_status = function
  | Stopped -> "Stopped"
  | Initializing -> "Initializing"
  | Rechecking -> "Rechecking"
  | Ready -> "Ready"

let string_of_buffer_status = function
  | NoOpenBuffers -> "NoOpenBuffers"
  | NoUnsavedBuffers -> "NoUnsavedBuffers"
  | UnsavedBuffers -> "UnsavedBuffers"

let source_of_trigger = function
  | CodeAction _
  | Completion _
  | Definition _
  | DidChange
  | DidClose
  | DidOpen
  | DidSave
  | DocumentHighlight _
  | DocumentSymbol _
  | FindReferences _
  | Hover _
  | Rage _
  | Rename _
  | SelectionRange _
  | SignatureHelp _
  | TypeCoverage _
  | ExecuteCommand _ ->
    Client
  | PushedErrorsEndOfRecheck _
  | PushedErrorsEnvChange
  | PushedErrorsNewSubscription
  | PushedErrorsRecheckStreaming _
  | ServerConnected ->
    Server
  | UnknownTrigger -> UnknownSource

let string_of_source = function
  | Client -> "Client"
  | Server -> "Server"
  | UnknownSource -> "UnknownSource"

(* An interaction which has been triggered but which hasn't yet produced the UX for this trigger *)
type pending_interaction = {
  start_state: state;
  trigger: trigger;
}

(* The internal state for LspInteraction *)
type internal_state = {
  mutable next_id: int;
  mutable pending_interactions: pending_interaction IMap.t;
  mutable lowest_pending_id: int;
  mutable last_recheck_start_state: state option;
}

let internal_state =
  {
    next_id = 0;
    pending_interactions = IMap.empty;
    lowest_pending_id = 0;
    last_recheck_start_state = None;
  }

(* Call this to start tracking an interaction *)
let start ~start_state ~trigger =
  let id = internal_state.next_id in
  internal_state.next_id <- internal_state.next_id + 1;
  let interaction = { start_state; trigger } in
  internal_state.pending_interactions <- IMap.add id interaction internal_state.pending_interactions;
  id

(* Call this to note that a recheck has started *)
let recheck_start ~start_state = internal_state.last_recheck_start_state <- Some start_state

let log ~ux ~trigger ~start_state ~end_state =
  let is_timeout_ux =
    match ux with
    | Timeout -> true
    | _ -> false
  in
  let lsp_id = lsp_id_of_trigger trigger |> Base.Option.map ~f:Lsp_fmt.id_to_string in
  FlowInteractionLogger.interaction
    ~lsp_id
    ~is_timeout_ux
    ~source:(trigger |> source_of_trigger |> string_of_source)
    ~trigger:(trigger |> string_of_trigger)
    ~ux:(ux |> string_of_ux)
    ~start_time_ms:(start_state.time *. 1000. |> int_of_float)
    ~end_time_ms:(end_state.time *. 1000. |> int_of_float)
    ~start_server_status:(start_state.server_status |> string_of_server_status)
    ~end_server_status:(end_state.server_status |> string_of_server_status)
    ~start_buffer_status:(start_state.buffer_status |> string_of_buffer_status)
    ~end_buffer_status:(end_state.buffer_status |> string_of_buffer_status)

(* Most interactions are triggered by the IDE sending a request and the server sending a response.
 * Those are logged via start & log. However, when we push errors to the client, we log those using
 * this method. *)
let log_pushed_errors ~end_state ~errors_reason =
  let (triggers, start_state) =
    match errors_reason with
    | End_of_recheck { recheck_reasons } ->
      ( List.map recheck_reasons ~f:(fun reason -> PushedErrorsEndOfRecheck reason),
        Base.Option.value ~default:end_state internal_state.last_recheck_start_state
      )
    | Recheck_streaming { recheck_reasons } ->
      ( List.map recheck_reasons ~f:(fun reason -> PushedErrorsRecheckStreaming reason),
        Base.Option.value ~default:end_state internal_state.last_recheck_start_state
      )
    | Env_change -> ([PushedErrorsEnvChange], end_state)
    | New_subscription -> ([PushedErrorsNewSubscription], end_state)
  in
  List.iter triggers ~f:(fun trigger -> log ~ux:PushedErrors ~trigger ~start_state ~end_state)

let log ~end_state ~ux ~id =
  Base.Option.iter (IMap.find_opt id internal_state.pending_interactions) ~f:(fun interaction ->
      internal_state.pending_interactions <- IMap.remove id internal_state.pending_interactions;
      let { start_state; trigger } = interaction in
      log ~ux ~trigger ~start_state ~end_state
  )

let rec gc ~get_state oldest_allowed =
  let s = internal_state in
  if s.lowest_pending_id < s.next_id then
    match IMap.find_opt s.lowest_pending_id s.pending_interactions with
    | None ->
      s.lowest_pending_id <- s.lowest_pending_id + 1;
      gc ~get_state oldest_allowed
    | Some interaction ->
      if interaction.start_state.time < oldest_allowed then (
        log ~end_state:(get_state ()) ~ux:Timeout ~id:s.lowest_pending_id;
        s.pending_interactions <- IMap.remove s.lowest_pending_id s.pending_interactions;
        s.lowest_pending_id <- s.lowest_pending_id + 1;
        gc ~get_state oldest_allowed
      ) else
        Some interaction.start_state.time
  else
    None

(* If an interaction is over 10 minutes old we'll stop tracking it. *)
let max_age = 600.0

(* Garbage collect every pending interaction that started more than `max_age` seconds ago.
 * Return when we should call gc again *)
let gc ~get_state =
  let now = Unix.gettimeofday () in
  (* gc any interaction that started more than max_age seconds ago *)
  let oldest_remaining_interaction = gc ~get_state (now -. max_age) in
  match oldest_remaining_interaction with
  | None ->
    (* If there are no pending interactions, then nothing will expire for at least max_age secs *)
    now +. max_age
  | Some start_time ->
    (* Otherwise let's check back in when the oldest pending interaction is set to expire *)
    start_time +. max_age

let rec dismiss_tracks end_state =
  let s = internal_state in
  if s.lowest_pending_id < s.next_id then begin
    log ~end_state ~ux:Dismissed ~id:s.lowest_pending_id;
    s.lowest_pending_id <- s.lowest_pending_id + 1;
    dismiss_tracks end_state
  end

let init () = FlowInteractionLogger.init ()

let flush () = FlowInteractionLogger.flush ()

(* Not every message the lsp process receives triggers an interaction. This function
 * enumerates which methods we care about and what trigger they correspond to *)
let trigger_of_lsp_msg =
  let open Lsp in
  function
  (* Requests from the client which we care about *)
  | RequestMessage (lsp_id, CodeActionRequest _) -> Some (CodeAction lsp_id)
  | RequestMessage (lsp_id, CompletionRequest _) -> Some (Completion lsp_id)
  | RequestMessage (lsp_id, DefinitionRequest _) -> Some (Definition lsp_id)
  | RequestMessage (lsp_id, DocumentHighlightRequest _) -> Some (DocumentHighlight lsp_id)
  | RequestMessage (lsp_id, DocumentSymbolRequest _) -> Some (DocumentSymbol lsp_id)
  | RequestMessage (lsp_id, FindReferencesRequest _) -> Some (FindReferences lsp_id)
  | RequestMessage (lsp_id, HoverRequest _) -> Some (Hover lsp_id)
  | RequestMessage (lsp_id, RageRequest) -> Some (Rage lsp_id)
  | RequestMessage (lsp_id, RenameRequest _) -> Some (Rename lsp_id)
  | RequestMessage (lsp_id, TypeCoverageRequest _) -> Some (TypeCoverage lsp_id)
  | RequestMessage (lsp_id, SelectionRangeRequest _) -> Some (SelectionRange lsp_id)
  | RequestMessage (lsp_id, SignatureHelpRequest _) -> Some (SignatureHelp lsp_id)
  | RequestMessage (lsp_id, ExecuteCommandRequest _) -> Some (ExecuteCommand lsp_id)
  (* Requests which we don't care about. Some are unsupported and some are sent from the lsp to
     * the client *)
  | RequestMessage (_, ApplyWorkspaceEditRequest _)
  | RequestMessage (_, CompletionItemResolveRequest _)
  | RequestMessage (_, ConfigurationRequest _)
  | RequestMessage (_, DocumentFormattingRequest _)
  | RequestMessage (_, DocumentOnTypeFormattingRequest _)
  | RequestMessage (_, DocumentRangeFormattingRequest _)
  | RequestMessage (_, InitializeRequest _)
  | RequestMessage (_, ShowMessageRequestRequest _)
  | RequestMessage (_, ShowStatusRequest _)
  | RequestMessage (_, ShutdownRequest)
  | RequestMessage (_, CodeLensResolveRequest _)
  | RequestMessage (_, DocumentCodeLensRequest _)
  (* TODO not sure if this is right, just need to unbreak the build. *)
  | RequestMessage (_, TypeDefinitionRequest _)
  | RequestMessage (_, UnknownRequest _)
  | RequestMessage (_, WorkspaceSymbolRequest _)
  | RequestMessage (_, RegisterCapabilityRequest _) ->
    None
  (* No responses trigger interactions *)
  | ResponseMessage (_, InitializeResult _)
  | ResponseMessage (_, ShutdownResult)
  | ResponseMessage (_, CodeLensResolveResult _)
  | ResponseMessage (_, HoverResult _)
  | ResponseMessage (_, DefinitionResult _)
  | ResponseMessage (_, CompletionResult _)
  | ResponseMessage (_, CompletionItemResolveResult _)
  | ResponseMessage (_, ConfigurationResult _)
  | ResponseMessage (_, SelectionRangeResult _)
  | ResponseMessage (_, SignatureHelpResult _)
  | ResponseMessage (_, WorkspaceSymbolResult _)
  | ResponseMessage (_, DocumentSymbolResult _)
  | ResponseMessage (_, FindReferencesResult _)
  | ResponseMessage (_, GoToImplementationResult _)
  | ResponseMessage (_, DocumentHighlightResult _)
  | ResponseMessage (_, DocumentCodeLensResult _)
  | ResponseMessage (_, TypeCoverageResult _)
  | ResponseMessage (_, ExecuteCommandResult _)
  | ResponseMessage (_, ApplyWorkspaceEditResult _)
  | ResponseMessage (_, RegisterCapabilityResult)
  | ResponseMessage (_, TypeDefinitionResult _)
  | ResponseMessage (_, DocumentFormattingResult _)
  | ResponseMessage (_, DocumentRangeFormattingResult _)
  | ResponseMessage (_, DocumentOnTypeFormattingResult _)
  | ResponseMessage (_, ShowMessageRequestResult _)
  | ResponseMessage (_, ShowStatusResult _)
  | ResponseMessage (_, RageResult _)
  | ResponseMessage (_, RenameResult _)
  | ResponseMessage (_, ErrorResult _)
  | ResponseMessage (_, CodeActionResult _) ->
    None
  (* Only a few notifications can trigger an interaction *)
  | NotificationMessage (DidOpenNotification _) -> Some DidOpen
  | NotificationMessage (DidCloseNotification _) -> Some DidClose
  | NotificationMessage (DidSaveNotification _) -> Some DidSave
  | NotificationMessage (DidChangeNotification _) -> Some DidChange
  (* Most notifications we ignore *)
  | NotificationMessage ExitNotification
  | NotificationMessage (CancelRequestNotification _)
  | NotificationMessage (PublishDiagnosticsNotification _)
  | NotificationMessage (LogMessageNotification _)
  | NotificationMessage (TelemetryNotification _)
  | NotificationMessage (ShowMessageNotification _)
  | NotificationMessage (ConnectionStatusNotification _)
  | NotificationMessage InitializedNotification
  | NotificationMessage SetTraceNotification
  | NotificationMessage LogTraceNotification
  | NotificationMessage (UnknownNotification _)
  | NotificationMessage (DidChangeConfigurationNotification _)
  | NotificationMessage (DidChangeWatchedFilesNotification _) ->
    None
