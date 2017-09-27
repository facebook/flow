(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Prot = Persistent_connection_prot

type single_client = {
  client: ServerUtils.client;
  infd: Unix.file_descr;
  outfd: Unix.file_descr;
  logging_context: FlowEventLogger.logging_context;
  subscribed: bool;
  opened_files: SSet.t;
}

type t = single_client list

let empty = []

let send_message message connection =
  Marshal_tools.to_fd_with_preamble connection.outfd (message : Prot.response)

let send_ready connection =
  Marshal_tools.to_fd_with_preamble connection.outfd ()

let send_errors =
  (* We don't know what kind of file the filename represents,
   * so we have to try (almost) all of them. *)
  let get_warnings_for_file =
    let rec get_first_contained warn_map = function
      | [] -> Errors.ErrorSet.empty
      | filename::filenames ->
        match Utils_js.FilenameMap.get filename warn_map with
        | Some errs -> errs
        | None -> get_first_contained warn_map filenames
    in
    fun filename warn_map ->
      get_first_contained warn_map [
        File_key.SourceFile filename;
        File_key.LibFile filename;
        File_key.JsonFile filename;
        File_key.ResourceFile filename;
      ]
  in

  fun ~errors ~warnings connection ->
    let warnings = SSet.fold
      (fun filename warn_acc ->
        let file_warns = get_warnings_for_file filename warnings in
        Errors.ErrorSet.union file_warns warn_acc)
      connection.opened_files Errors.ErrorSet.empty
    in
    send_message (Prot.Errors {errors; warnings}) connection

let send_errors_if_subscribed client ~errors ~warnings =
  if client.subscribed
  then send_errors ~errors ~warnings client

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
      opened_files = SSet.empty;
    }
  in
  (new_connection :: connections, new_connection)

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

let update_clients connections ~errors ~warnings =
  let error_count = Errors.ErrorSet.cardinal errors in
  let warning_file_count = Utils_js.FilenameMap.cardinal warnings in
  let subscribed_connections = get_subscribed_connections connections in
  let subscribed_client_count = List.length subscribed_connections in
  let all_client_count = List.length connections in
  Hh_logger.info
    "sending (%d errors) and (warnings from %d files) to %d subscribed clients (of %d total)"
    error_count warning_file_count subscribed_client_count all_client_count;
  List.iter (send_errors ~errors ~warnings) subscribed_connections

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

let subscribe_client connections client ~current_errors ~current_warnings =
  Hh_logger.info "Subscribing client to push diagnostics";
  if client.subscribed then
    (* noop *)
    connections
  else begin
    send_errors ~errors:current_errors ~warnings:current_warnings client;
    modify_item connections client (fun c -> { c with subscribed = true })
  end

let client_did_open connections client ~filenames =
  Hh_logger.info "Client opened %d file(s)" (Nel.length filenames);
  let new_opened_files = Nel.fold_left (Fn.flip SSet.add) client.opened_files filenames in
  (* SSet.add ensures physical equality if the set is unchanged,
   * so == is appropriate. *)
  if new_opened_files == client.opened_files then
    (* noop *)
    None
  else
    let update_opened_files c = {c with opened_files = new_opened_files} in
    let new_client = update_opened_files client in
    let new_connections = modify_item connections client update_opened_files in
    Some (new_connections, new_client)

let client_did_close connections client ~filenames =
  Hh_logger.info "Client closed %d file(s)" (Nel.length filenames);
  let new_opened_files = Nel.fold_left (Fn.flip SSet.remove) client.opened_files filenames in
  (* SSet.remove ensures physical equality if the set is unchanged,
   * so == is appropriate. *)
  if new_opened_files == client.opened_files then
    (* noop *)
    None
  else
    let update_opened_files c = {c with opened_files = new_opened_files} in
    let new_client = update_opened_files client in
    let new_connections = modify_item connections client update_opened_files in
    Some (new_connections, new_client)

let get_logging_context client = client.logging_context

let input_value client = Marshal_tools.from_fd_with_preamble client.infd

let get_opened_files clients =
  List.fold_left (fun acc client -> SSet.union acc (client.opened_files)) SSet.empty clients
