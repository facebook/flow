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

val to_string: t -> string

val empty: t

val add_client:
  t -> Prot.client_id -> FlowEventLogger.logging_context -> Lsp.Initialize.params option -> t
val remove_client:
  t -> Prot.client_id -> t

(* Send updates to all clients that are subscribed *)
val update_clients:
  clients:t ->
  calc_errors_and_warnings:(unit -> Errors.ErrorSet.t * Errors.ErrorSet.t Utils_js.FilenameMap.t) ->
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
  errors:Errors.ErrorSet.t ->
  warnings:Errors.ErrorSet.t Utils_js.FilenameMap.t -> unit

(* getters/setters on single_client *)
val subscribe_client:
  clients:t ->
  client:single_client ->
  current_errors:Errors.ErrorSet.t ->
  current_warnings:Errors.ErrorSet.t Utils_js.FilenameMap.t -> t

val client_did_open:
  t ->
  single_client ->
  files:(string * string) Nel.t ->
  (t * single_client) option

val client_did_change:
  t ->
  single_client ->
  string ->
  Lsp.DidChange.textDocumentContentChangeEvent list ->
  (t * single_client, string * Utils.callstack) result

val client_did_close:
  t ->
  single_client ->
  filenames:string Nel.t
  -> (t * single_client) option

val get_logging_context: single_client -> FlowEventLogger.logging_context

(** Returns the set of all opened files across all clients.
   It's not meaningful to talk about the *content* of those opened files in
   cases where clients differ. *)
val get_opened_files: t -> SSet.t

(** Returns either FileContent for this file if it was opened by the persistent
   client, or FileName if it wasn't. *)
val get_file: single_client -> string -> File_input.t

val get_client: t -> Prot.client_id -> single_client option
