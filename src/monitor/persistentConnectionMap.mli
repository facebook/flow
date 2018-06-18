(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val add:
  client_id:Persistent_connection_prot.client_id ->
  client:PersistentConnection.t ->
  unit

val get:
  client_id:Persistent_connection_prot.client_id ->
  PersistentConnection.t option

val remove:
  client_id:Persistent_connection_prot.client_id ->
  unit

val cardinal:
  unit ->
  int

val get_all_clients:
  unit ->
  PersistentConnection.t list
