(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The Flow server monitor uses this module to communicate with the server and with clients *)

module PersistentProt = Persistent_connection_prot

(* Ephemeral socket connections expect a response to their requests. We use request_id to indicate
 * to which request a given response is replying *)
type request_id = string

(* These are the messages that the monitor sends to the server *)
type monitor_to_server_message =
(* A request from an ephemeral socket connection. It expects a response *)
| Request of request_id * ServerProt.command_with_context
(* A notification that there is a new persistent socket connection *)
| NewPersistentConnection of PersistentProt.client_id * FlowEventLogger.logging_context
(* A request from a persistent socket connection. It does not expect a response *)
| PersistentConnectionRequest of PersistentProt.client_id * PersistentProt.request
(* A notification that a persistent socket connection is dead *)
| DeadPersistentConnection of PersistentProt.client_id

(* These are the messages that the server sends to the monitor *)
type server_to_monitor_message =
(* A response to an ephemeral socket's request *)
| Response of request_id * bytes
(* A response to a persistent socket connection *)
| PersistentConnectionResponse of PersistentProt.client_id * PersistentProt.response
(* A notification of the server's current status *)
| StatusUpdate of ServerStatus.status

(* These are the messages that the server sends to an ephemeral socket connection *)
type monitor_to_client_message =
(* ServerProt is currently a mess. We should combine all the responses into a single type so that
 * we don't need to pass around Marshal'd bytes like this *)
| Data of bytes
(* The server is currently busy. Please wait for a response *)
| Please_hold of ServerStatus.status
