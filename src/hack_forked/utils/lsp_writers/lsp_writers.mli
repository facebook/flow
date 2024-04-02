(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val telemetry : Jsonrpc.writer -> Lsp.MessageType.t -> string -> unit

val telemetry_error : Jsonrpc.writer -> string -> unit

val telemetry_log : Jsonrpc.writer -> string -> unit

val log : Jsonrpc.writer -> Lsp.MessageType.t -> string -> unit

val log_error : Jsonrpc.writer -> string -> unit

val log_warning : Jsonrpc.writer -> string -> unit

val log_info : Jsonrpc.writer -> string -> unit

val dismiss_diagnostics : Jsonrpc.writer -> Lsp.UriSet.t -> Lsp.UriSet.t

val notify_connectionStatus : Lsp.Initialize.params -> Jsonrpc.writer -> bool -> bool -> bool
