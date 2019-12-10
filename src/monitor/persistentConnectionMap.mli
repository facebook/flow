(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val add : client_id:LspProt.client_id -> client:PersistentConnection.t -> unit

val get : client_id:LspProt.client_id -> PersistentConnection.t option

val remove : client_id:LspProt.client_id -> unit

val cardinal : unit -> int

val get_all_clients : unit -> PersistentConnection.t list
