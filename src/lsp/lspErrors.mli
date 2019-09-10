(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val empty : t

val set_live_parse_errors_and_send :
  (Hh_json.json -> unit) -> string -> Lsp.PublishDiagnostics.diagnostic list -> t -> t

val add_streamed_server_errors_and_send :
  (Hh_json.json -> unit) -> Lsp.PublishDiagnostics.diagnostic list SMap.t -> t -> t

val set_finalized_server_errors_and_send :
  (Hh_json.json -> unit) -> Lsp.PublishDiagnostics.diagnostic list SMap.t -> t -> t

val update_errors_due_to_change_and_send :
  (Hh_json.json -> unit) -> Lsp.DidChange.didChangeTextDocumentParams -> t -> t

val clear_all_live_errors_and_send : (Hh_json.json -> unit) -> string -> t -> t

val clear_all_errors_and_send : (Hh_json.json -> unit) -> t -> t
