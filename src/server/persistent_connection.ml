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
  logging_context: FlowEventLogger.logging_context;
  subscribed: bool;
}

type t = single_client list

let empty = []

let send_message message connection =
  Marshal_tools.to_fd_with_preamble connection.outfd (message : Prot.response)

let send_errors errors connection =
  send_message (Prot.Errors errors) connection

let send_single_start_recheck connection =
  send_message (Prot.StartRecheck) connection

let send_single_end_recheck connection =
  send_message (Prot.EndRecheck) connection

let add_client connections client logging_context =
  Hh_logger.info "Adding new persistent connection";
  let new_connection =
    {
      client;
      infd = Unix.descr_of_in_channel client.ServerUtils.ic;
      outfd = Unix.descr_of_out_channel client.ServerUtils.oc;
      logging_context;
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

let get_subscribed_connections = List.filter (fun c -> c.subscribed)

let update_clients connections errors =
  let error_count = Errors.ErrorSet.cardinal errors in
  let subscribed_connections = get_subscribed_connections connections in
  let subscribed_client_count = List.length subscribed_connections in
  let all_client_count = List.length connections in
  Hh_logger.info
    "sending %d errors to %d subscribed clients (of %d total)"
    error_count subscribed_client_count all_client_count;
  List.iter (send_errors errors) subscribed_connections

let send_start_recheck connections =
  connections
    |> get_subscribed_connections
    |> List.iter send_single_start_recheck

let send_end_recheck connections =
  connections
    |> get_subscribed_connections
    |> List.iter send_single_end_recheck

let rec modify_item lst item f = match lst with
  | [] -> raise Not_found
  | hd::tl ->
      (* Use identity, not structural equality *)
      if hd == item then
        (f hd)::tl
      else
        hd::(modify_item tl item f)

let subscribe_client connections client current_errors =
  Hh_logger.info "Subscribing client to push diagnostics";
  if client.subscribed then
    (* noop *)
    connections
  else begin
    send_errors current_errors client;
    modify_item connections client (fun c -> { c with subscribed = true })
  end

let get_logging_context client = client.logging_context

let input_value client = Marshal_tools.from_fd_with_preamble client.infd
