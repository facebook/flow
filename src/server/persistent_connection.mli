(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Prot = Persistent_connection_prot

(* Stores all the necessary information about current persistent connections *)
type t

type single_client

val empty: t

val add_client: t -> ServerUtils.client -> FlowEventLogger.logging_context -> (t * single_client)
val remove_client: t -> single_client -> t

(* Send updates to all clients that are subscribed *)
val update_clients: t -> errors:Errors.ErrorSet.t ->
  warnings:Errors.ErrorSet.t Utils_js.FilenameMap.t -> unit
val send_start_recheck: t -> unit
val send_end_recheck: t -> unit

(* Send a message to just one client *)
val send_message: Prot.response -> single_client -> unit
val send_ready: single_client -> unit

val client_fd_list: t -> Unix.file_descr list

val client_of_fd: t -> Unix.file_descr -> single_client

(* getters/setters on single_client *)
val subscribe_client: t -> single_client -> current_errors:Errors.ErrorSet.t ->
  current_warnings:Errors.ErrorSet.t Utils_js.FilenameMap.t -> t
val client_did_open: t -> single_client -> filenames:string Nel.t ->
  current_errors:Errors.ErrorSet.t -> current_warnings:Errors.ErrorSet.t Utils_js.FilenameMap.t-> t
val client_did_close: t -> single_client -> filenames:string Nel.t ->
  current_errors:Errors.ErrorSet.t -> current_warnings:Errors.ErrorSet.t Utils_js.FilenameMap.t-> t
val get_logging_context: single_client -> FlowEventLogger.logging_context

val input_value: single_client -> Prot.request
