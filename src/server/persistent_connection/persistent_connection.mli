(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Stores all the necessary information about current persistent connections *)
type t

type single_client

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
    * Errors.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t) ->
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

val get_opened_files : t -> SSet.t
(** Returns the set of all opened files across all clients.
   It's not meaningful to talk about the *content* of those opened files in
   cases where clients differ. *)

val get_file : single_client -> string -> File_input.t
(** Returns either FileContent for this file if it was opened by the persistent
   client, or FileName if it wasn't. *)

val get_client : LspProt.client_id -> single_client option

val get_id : single_client -> LspProt.client_id

val client_snippet_support : single_client -> bool
