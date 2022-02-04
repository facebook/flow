(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Stores all the necessary information about current persistent connections *)
type t
type single_client

module Client_config : sig
  type t = { suggest_autoimports: bool }

  val suggest_autoimports : t -> bool
end

val empty : t
val add_client : LspProt.client_id -> Lsp.Initialize.params -> unit
val remove_client : LspProt.client_id -> unit
val add_client_to_clients : t -> LspProt.client_id -> t
val remove_client_from_clients : t -> LspProt.client_id -> t

(* Send updates to all clients that are subscribed *)
val update_clients :
  clients:t ->
  errors_reason:LspProt.errors_reason ->
  calc_errors_and_warnings:
    (unit ->
    Errors.ConcreteLocPrintableErrorSet.t
    * Errors.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t
    ) ->
  unit

val send_lsp : t -> Lsp.lsp_message option * LspProt.metadata -> unit
val send_start_recheck : t -> unit
val send_end_recheck : lazy_stats:ServerProt.Response.lazy_stats -> t -> unit

(* Send a message to just one client *)
val send_response : LspProt.response_with_metadata -> single_client -> unit

val send_errors_if_subscribed :
  client:single_client ->
  errors_reason:LspProt.errors_reason ->
  errors:Errors.ConcreteLocPrintableErrorSet.t ->
  warnings:Errors.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t ->
  unit

(* getters/setters on single_client *)
val subscribe_client :
  client:single_client ->
  current_errors:Errors.ConcreteLocPrintableErrorSet.t ->
  current_warnings:Errors.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t ->
  unit

val client_did_open : single_client -> files:(string * string) Nel.t -> bool

val client_did_change :
  single_client ->
  string ->
  Lsp.DidChange.textDocumentContentChangeEvent list ->
  (unit, string * Utils.callstack) result

val client_did_close : single_client -> filenames:string Nel.t -> bool
val client_did_change_configuration : single_client -> Client_config.t -> unit

(** Returns either FileContent for this file if it was opened by the persistent
    client, or FileName if it wasn't. *)
val get_file : single_client -> string -> File_input.t

val get_client : LspProt.client_id -> single_client option
val get_id : single_client -> LspProt.client_id
val lsp_initialize_params : single_client -> Lsp.Initialize.params
val client_config : single_client -> Client_config.t

val type_parse_artifacts_cache :
  single_client -> (Types_js_types.file_artifacts, Flow_error.ErrorSet.t) result FilenameCache.t

val clear_type_parse_artifacts_caches : unit -> unit
val push_outstanding_handler : single_client -> Lsp.lsp_id -> unit Lsp.lsp_handler -> unit
val pop_outstanding_handler : single_client -> Lsp.lsp_id -> unit Lsp.lsp_handler option
