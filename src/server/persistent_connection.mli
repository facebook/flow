(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Prot = Persistent_connection_prot

(* Stores all the necessary information about current persistent connections *)
type t

type single_client

val empty: t

val add_client: t -> Prot.client_id -> FlowEventLogger.logging_context -> t
val remove_client: t -> Prot.client_id -> t

(* Send updates to all clients that are subscribed *)
val update_clients:
  clients:t ->
  errors:Errors.ErrorSet.t ->
  warnings:Errors.ErrorSet.t Utils_js.FilenameMap.t -> unit
val send_start_recheck:
  t -> unit
val send_end_recheck:
  t -> unit

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
val client_did_open: t -> single_client -> filenames:string Nel.t -> (t * single_client) option
val client_did_close: t -> single_client -> filenames:string Nel.t -> (t * single_client) option
val get_logging_context: single_client -> FlowEventLogger.logging_context

val get_opened_files: t -> SSet.t
val get_client: t -> Prot.client_id -> single_client
