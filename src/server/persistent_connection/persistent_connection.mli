(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Prot = Persistent_connection_prot

(* Stores all the necessary information about current persistent connections *)
type t

type single_client

val empty: t

val add_client:
  Prot.client_id -> FlowEventLogger.logging_context -> Lsp.Initialize.params option -> unit
val remove_client: Prot.client_id -> unit
val add_client_to_clients: t -> Prot.client_id -> t
val remove_client_from_clients: t -> Prot.client_id -> t

(* Send updates to all clients that are subscribed *)
val update_clients:
  clients:t ->
  errors_reason:Persistent_connection_prot.errors_reason ->
  calc_errors_and_warnings:(
    unit ->
    Errors.ConcreteLocPrintableErrorSet.t *
      (Errors.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t)
  ) ->
  unit
val send_lsp:
  t -> Lsp.lsp_message option * Prot.metadata -> unit
val send_start_recheck:
  t -> unit
val send_end_recheck:
  lazy_stats:ServerProt.Response.lazy_stats -> t -> unit

(* Send a message to just one client *)
val send_message: Prot.response -> single_client -> unit
val send_errors_if_subscribed:
  client:single_client ->
  errors_reason:Persistent_connection_prot.errors_reason ->
  errors:Errors.ConcreteLocPrintableErrorSet.t ->
  warnings:Errors.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t ->
  unit

(* getters/setters on single_client *)
val subscribe_client:
  client:single_client ->
  current_errors:Errors.ConcreteLocPrintableErrorSet.t ->
  current_warnings:Errors.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t ->
  unit

val client_did_open:
  single_client ->
  files:(string * string) Nel.t ->
  bool

val client_did_change:
  single_client ->
  string ->
  Lsp.DidChange.textDocumentContentChangeEvent list ->
  (unit, string * Utils.callstack) result

val client_did_close:
  single_client ->
  filenames:string Nel.t
  -> bool

val get_logging_context: single_client -> FlowEventLogger.logging_context

(** Returns the set of all opened files across all clients.
   It's not meaningful to talk about the *content* of those opened files in
   cases where clients differ. *)
val get_opened_files: t -> SSet.t

(** Returns either FileContent for this file if it was opened by the persistent
   client, or FileName if it wasn't. *)
val get_file: single_client -> string -> File_input.t

val get_client: Prot.client_id -> single_client option
val get_id: single_client -> Prot.client_id

val client_snippet_support: single_client -> bool
