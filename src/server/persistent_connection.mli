(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Prot = ServerProt.Persistent_connection_prot

(* Stores all the necessary information about current persistent connections *)
type t

type single_client

val empty: t

val add_client: t -> ServerUtils.client -> t
val remove_client: t -> single_client -> t

(* Send error updates to any clients that are subscribed *)
val update_clients: t -> Errors.ErrorSet.t -> unit
val send_start_recheck: t -> unit
val send_end_recheck: t -> unit

val client_fd_list: t -> Unix.file_descr list

val client_of_fd: t -> Unix.file_descr -> single_client

val subscribe_client: t -> single_client -> Errors.ErrorSet.t -> t

val input_value: single_client -> Prot.request
