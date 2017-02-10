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

type single_client = {
  client: ServerUtils.client;
  infd: Unix.file_descr;
  outfd: Unix.file_descr;
  subscribed: bool;
}

type t = single_client list

let empty = []

let send_errors errors connection =
  let message = Prot.Errors errors in
  Marshal_tools.to_fd_with_preamble connection.outfd (message : Prot.response)

let add_client connections client =
  let new_connection =
    {
      client;
      infd = Unix.descr_of_in_channel client.ServerUtils.ic;
      outfd = Unix.descr_of_out_channel client.ServerUtils.oc;
      subscribed = false;
    }
  in
  new_connection :: connections

(* Uses identity *)
let remove_item lst item = List.filter (fun e -> e != item) lst

let remove_client connections client =
  ServerUtils.(begin
    (* TODO figure out which of these is actually necessary/actually does something *)
    client.client.close ();
    shutdown_client (client.client.ic, client.client.oc)
  end);
  remove_item connections client

let client_fd_list = List.map (fun conn -> conn.infd)

let client_of_fd connections fd =
  List.find (fun client -> client.infd = fd) connections

let update_clients connections errors =
  let error_count = Errors.ErrorSet.cardinal errors in
  let subscribed_connections = List.filter (fun c -> c.subscribed) connections in
  let client_count = List.length subscribed_connections in
  Printf.printf "sending %d errors to %d subscribed clients" error_count client_count;
  print_newline ();
  List.iter (send_errors errors) subscribed_connections

let rec modify_item lst item f = match lst with
  | [] -> raise Not_found
  | hd::tl ->
      (* Use identity, not structural equality *)
      if hd == item then
        (f hd)::tl
      else
        hd::(modify_item tl item f)

let subscribe_client connections client current_errors =
  if client.subscribed then
    (* noop *)
    connections
  else begin
    send_errors current_errors client;
    modify_item connections client (fun c -> { c with subscribed = true })
  end

let input_value client = Marshal_tools.from_fd_with_preamble client.infd
