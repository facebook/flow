(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id = int

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
  | PushedErrorsEndOfRecheck
  | PushedErrorsEnvChange
  | PushedErrorsNewSubscription
  | PushedErrorsRecheckStreaming
  | Rage of Lsp.lsp_id
  | Rename of Lsp.lsp_id
  | ServerConnected
  | SelectionRange of Lsp.lsp_id
  | SignatureHelp of Lsp.lsp_id
  | TypeCoverage of Lsp.lsp_id
  | ExecuteCommand of Lsp.lsp_id
  | WillRenameFiles of Lsp.lsp_id
  | AutoCloseJsx of Lsp.lsp_id
  | LinkedEditingRange of Lsp.lsp_id
  | RenameFileImports of Lsp.lsp_id
  | UnknownTrigger

type ux =
  | Canceled
  | CanceledPushingLiveNonParseErrors
  | Dismissed
  | Errored
  | ErroredPushingLiveNonParseErrors
  | ErroredPushingLiveParseErrors
  | PushedErrors
  | PushedLiveNonParseErrors of Lsp.DocumentUri.t
  | PushedLiveParseErrors of Lsp.DocumentUri.t
  | Responded
  | Timeout

type server_status =
  | Stopped
  | Initializing
  | Rechecking
  | Ready

type buffer_status =
  | NoOpenBuffers
  | NoUnsavedBuffers
  | UnsavedBuffers

type state = {
  time: float;
  server_status: server_status;
  buffer_status: buffer_status;
}

val init : unit -> unit

val start : start_state:state -> trigger:trigger -> id

val recheck_start : start_state:state -> unit

val log : end_state:state -> ux:ux -> id:id -> unit

val log_pushed_errors : end_state:state -> errors_reason:LspProt.errors_reason -> unit

val trigger_of_lsp_msg : Lsp.lsp_message -> trigger option

val gc : get_state:(unit -> state) -> float

val flush : unit -> unit Lwt.t

val dismiss_tracks : state -> unit
