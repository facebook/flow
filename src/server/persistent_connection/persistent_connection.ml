(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Prot = Persistent_connection_prot

type single_client = {
  is_lsp: bool;
  logging_context: FlowEventLogger.logging_context;
  subscribed: bool;
  opened_files: string SMap.t; (* map from filename to content *)
  client_id: Persistent_connection_prot.client_id;
  lsp_initialize_params: Lsp.Initialize.params option;
}

type t = single_client list

let to_string (clients: t) : string =
  let client_to_string (client: single_client) : string =
    Printf.sprintf "{id:%d opened:%d subscribed:%B context:%f}"
      client.client_id (SMap.cardinal client.opened_files)
      client.subscribed client.logging_context.FlowEventLogger.start_time
  in
  let clients_str = List.map client_to_string clients in
  Printf.sprintf "[%s]" (String.concat ", " clients_str)

let empty = []

let send_message_to_client response client =
  MonitorRPC.respond_to_persistent_connection ~client_id:client.client_id ~response

let send_message message client = send_message_to_client (message : Prot.response) client

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

  fun ~errors ~warnings client ->
    let opened_filenames = SMap.bindings client.opened_files |> List.map fst in
    let warnings = List.fold_right
      (fun filename warn_acc ->
        let file_warns = get_warnings_for_file filename warnings in
        Errors.ErrorSet.union file_warns warn_acc)
      opened_filenames Errors.ErrorSet.empty
    in
    send_message (Prot.Errors {errors; warnings}) client

let send_errors_if_subscribed ~client ~errors ~warnings =
  if client.subscribed
  then send_errors ~errors ~warnings client

let send_single_exit code client =
  send_message (Prot.ServerExit code) client

let send_single_lsp message client =
  send_message (Prot.LspFromServer message) client

let send_single_start_recheck client =
  send_message (Prot.StartRecheck) client

let send_single_end_recheck ~lazy_stats client  =
  send_message (Prot.EndRecheck lazy_stats) client

let add_client clients client_id logging_context lsp =
  let new_client =
    {
      is_lsp = (lsp <> None);
      logging_context;
      subscribed = false;
      opened_files = SMap.empty;
      client_id;
      lsp_initialize_params = lsp;
    }
  in
  Hh_logger.info "Adding new persistent connection #%d" new_client.client_id;
  (new_client :: clients)

let remove_client clients client_id =
  Hh_logger.info "Removing persistent connection client #%d" client_id;
  List.filter (fun client -> client.client_id != client_id) clients

let get_subscribed_clients = List.filter (fun c -> c.subscribed)

let get_subscribed_lsp_clients = List.filter (fun c -> c.subscribed && c.is_lsp)

let update_clients ~clients ~calc_errors_and_warnings =
  let subscribed_clients = get_subscribed_clients clients in
  let subscribed_client_count = List.length subscribed_clients in
  let all_client_count = List.length clients in
  if subscribed_clients <> []
  then begin
    let errors, warnings = calc_errors_and_warnings () in
    let error_count = Errors.ErrorSet.cardinal errors in
    let warning_file_count = Utils_js.FilenameMap.cardinal warnings in
    Hh_logger.info
      "sending (%d errors) and (warnings from %d files) to %d subscribed clients (of %d total)"
      error_count warning_file_count subscribed_client_count all_client_count;
    List.iter (send_errors ~errors ~warnings) subscribed_clients
  end

let send_exit clients code =
  clients
    |> get_subscribed_clients
    |> List.iter (send_single_exit code)

let send_lsp clients json =
  clients
    |> get_subscribed_lsp_clients
    |> List.iter (send_single_lsp json)

let send_start_recheck clients =
  clients
    |> get_subscribed_clients
    |> List.iter send_single_start_recheck

let send_end_recheck ~lazy_stats clients =
  clients
    |> get_subscribed_clients
    |> List.iter (send_single_end_recheck ~lazy_stats)

let rec modify_item lst item f = match lst with
  | [] -> raise Not_found
  | hd::tl ->
      (* Use identity, not structural equality *)
      if hd == item then
        (f hd)::tl
      else
        hd::(modify_item tl item f)

let subscribe_client ~clients ~client ~current_errors ~current_warnings =
  Hh_logger.info "Subscribing client #%d to push diagnostics" client.client_id;
  if client.subscribed then
    (* noop *)
    clients
  else begin
    send_errors ~errors:current_errors ~warnings:current_warnings client;
    modify_item clients client (fun c -> { c with subscribed = true })
  end

let client_did_open
    (clients: single_client list)
    (client: single_client)
    ~(files: (string * string) Nel.t)
  : (single_client list * single_client) option =
  Hh_logger.info "Client #%d opened %d file(s)" client.client_id (Nel.length files);
  let add_file acc (filename, content) = SMap.add filename content acc in
  let new_opened_files = Nel.fold_left add_file client.opened_files files in
  (* SMap.add ensures physical equality if the map is unchanged, since 4.0.3,
   * so == is appropriate. *)
  if new_opened_files == client.opened_files then
    (* noop *)
    None
  else
    let update_opened_files c = {c with opened_files = new_opened_files} in
    let new_client = update_opened_files client in
    let new_connections = modify_item clients client update_opened_files in
    Some (new_connections, new_client)

let client_did_change
    (clients: single_client list)
    (client: single_client)
    (fn: string)
    (changes: Lsp.DidChange.textDocumentContentChangeEvent list)
  : (single_client list * single_client, string * Utils.callstack) result =
  try begin
    let content = SMap.find fn client.opened_files in
    match Lsp_helpers.apply_changes content changes with
    | Error (reason, stack) -> Error (reason, stack)
    | Ok new_content ->
      let new_opened_files = SMap.add fn new_content client.opened_files in
      let update_opened_files c = {c with opened_files = new_opened_files} in
      let new_client = update_opened_files client in
      let new_connections = modify_item clients client update_opened_files in
      Ok (new_connections, new_client)
  end with Not_found ->
    let stack = Printexc.get_backtrace () in
    Error (Printf.sprintf "File %s wasn't open to change" fn, Utils.Callstack stack)


let client_did_close
    (clients: single_client list)
    (client: single_client)
    ~(filenames: string Nel.t)
  : (single_client list * single_client) option =
  Hh_logger.info "Client #%d closed %d file(s)" client.client_id (Nel.length filenames);
  let remove_file acc filename = SMap.remove filename acc in
  let new_opened_files = Nel.fold_left remove_file client.opened_files filenames in
  (* SMap.remove ensures physical equality if the set is unchanged,
   * so == is appropriate. *)
  if new_opened_files == client.opened_files then
    (* noop *)
    None
  else
    let update_opened_files c = {c with opened_files = new_opened_files} in
    let new_client = update_opened_files client in
    let new_connections = modify_item clients client update_opened_files in
    Some (new_connections, new_client)

let get_file (client: single_client) (fn: string) : File_input.t =
  let content_opt = SMap.get fn client.opened_files in
  match content_opt with
  | None -> File_input.FileName fn
  | Some content -> File_input.FileContent (Some fn, content)

let get_logging_context client = client.logging_context

let get_opened_files (clients: single_client list) : SSet.t =
  let per_file filename _content acc = SSet.add filename acc in
  let per_client acc client = SMap.fold per_file client.opened_files acc
  in
  List.fold_left per_client SSet.empty clients

let get_client clients client_id = List.find (fun c -> c.client_id = client_id) clients
