(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id = int

type trigger =
  | CodeAction
  | Completion
  | Definition
  | DidChange
  | DidClose
  | DidOpen
  | DidSave
  | DocumentHighlight
  | DocumentSymbol
  | FindReferences
  | Hover
  | PushedErrorsEndOfRecheck of LspProt.recheck_reason
  | PushedErrorsEnvChange
  | PushedErrorsNewSubscription
  | PushedErrorsRecheckStreaming of LspProt.recheck_reason
  | Rage
  | Rename
  | ServerConnected
  | SignatureHelp
  | TypeCoverage
  | UnknownTrigger

type ux =
  | Canceled
  | CanceledPushingLiveNonParseErrors
  | Errored
  | ErroredPushingLiveNonParseErrors
  | ErroredPushingLiveParseErrors
  | PushedErrors
  | PushedLiveNonParseErrors
  | PushedLiveParseErrors
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
