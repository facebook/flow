(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lsp
open Lsp_fmt

(************************************************************************)
(* Wrappers for some LSP methods                                        *)
(************************************************************************)

let telemetry (writer : Jsonrpc.writer) (level : MessageType.t) (message : string) : unit =
  print_logMessage level message |> Jsonrpc.notify writer "telemetry/event"

let telemetry_error (writer : Jsonrpc.writer) = telemetry writer MessageType.ErrorMessage

let telemetry_log (writer : Jsonrpc.writer) = telemetry writer MessageType.LogMessage

let log (writer : Jsonrpc.writer) (level : MessageType.t) (message : string) : unit =
  print_logMessage level message |> Jsonrpc.notify writer "window/logMessage"

let log_error (writer : Jsonrpc.writer) = log writer MessageType.ErrorMessage

let log_warning (writer : Jsonrpc.writer) = log writer MessageType.WarningMessage

let log_info (writer : Jsonrpc.writer) = log writer MessageType.InfoMessage

let dismiss_diagnostics (writer : Jsonrpc.writer) (diagnostic_uris : UriSet.t) : UriSet.t =
  let dismiss_one (uri : DocumentUri.t) : unit =
    let message = { Lsp.PublishDiagnostics.uri; diagnostics = [] } in
    message |> print_diagnostics |> Jsonrpc.notify writer "textDocument/publishDiagnostics"
  in
  UriSet.iter dismiss_one diagnostic_uris;
  UriSet.empty

let notify_connectionStatus
    (p : Lsp.Initialize.params) (writer : Jsonrpc.writer) (wasConnected : bool) (isConnected : bool)
    : bool =
  ( if Lsp_helpers.supports_connectionStatus p && wasConnected <> isConnected then
    let message = { Lsp.ConnectionStatus.isConnected } in
    message |> print_connectionStatus |> Jsonrpc.notify writer "telemetry/connectionStatus"
  );
  isConnected
