(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Prot = LspProt

type type_contents_artifacts =
  Context.t
  * Docblock.t
  * File_sig.With_Loc.t
  * (ALoc.t, ALoc.t * Type.t) Flow_ast.program
  * (Loc.t * Parse_error.t) list

type single_client = {
  client_id: Prot.client_id;
  lsp_initialize_params: Lsp.Initialize.params;
  mutable subscribed: bool;
  (* map from filename to content *)
  mutable opened_files: string SMap.t;
  type_contents_cache: (type_contents_artifacts, string) result Lwt.t FilenameCache.t;
}

type t = Prot.client_id list

let cache_max_size = 10

let remove_cache_entry client filename =
  (* get_def, coverage, etc. all construct a File_key.SourceFile, which is then used as a key
    * here. *)
  let file_key = File_key.SourceFile filename in
  FilenameCache.remove_entry file_key client.type_contents_cache

let active_clients : single_client IMap.t ref = ref IMap.empty

let get_client client_id = IMap.find_opt client_id !active_clients

let empty = []

let send_message_to_client (response : Prot.message_from_server) client =
  MonitorRPC.respond_to_persistent_connection ~client_id:client.client_id ~response

let send_response (response : Prot.response_with_metadata) client =
  send_message_to_client (Prot.RequestResponse response) client

let send_notification (response : Prot.notification_from_server) client =
  send_message_to_client (Prot.NotificationFromServer response) client

let send_errors =
  (* We don't know what kind of file the filename represents,
   * so we have to try (almost) all of them. *)
  let get_warnings_for_file =
    let rec get_first_contained warn_map = function
      | [] -> Errors.ConcreteLocPrintableErrorSet.empty
      | filename :: filenames ->
        (match Utils_js.FilenameMap.find_opt filename warn_map with
        | Some errs -> errs
        | None -> get_first_contained warn_map filenames)
    in
    fun filename warn_map ->
      get_first_contained
        warn_map
        [
          File_key.SourceFile filename;
          File_key.LibFile filename;
          File_key.JsonFile filename;
          File_key.ResourceFile filename;
        ]
  in
  fun ~errors_reason ~errors ~warnings client ->
    let opened_filenames = SMap.bindings client.opened_files |> Base.List.map ~f:fst in
    let warnings =
      List.fold_right
        (fun filename warn_acc ->
          let file_warns = get_warnings_for_file filename warnings in
          Errors.ConcreteLocPrintableErrorSet.union file_warns warn_acc)
        opened_filenames
        Errors.ConcreteLocPrintableErrorSet.empty
    in
    send_notification (Prot.Errors { errors; warnings; errors_reason }) client

let send_errors_if_subscribed ~client ~errors_reason ~errors ~warnings =
  if client.subscribed then send_errors ~errors_reason ~errors ~warnings client

let send_single_lsp (message, metadata) client =
  send_response (Prot.LspFromServer message, metadata) client

let send_single_start_recheck client = send_notification Prot.StartRecheck client

let send_single_end_recheck ~lazy_stats client =
  send_notification (Prot.EndRecheck lazy_stats) client

let add_client client_id lsp_initialize_params =
  let new_client =
    {
      subscribed = false;
      opened_files = SMap.empty;
      client_id;
      lsp_initialize_params;
      type_contents_cache = FilenameCache.make ~max_size:cache_max_size;
    }
  in
  active_clients := IMap.add client_id new_client !active_clients;
  Hh_logger.info "Adding new persistent connection #%d" new_client.client_id

let remove_client client_id =
  Hh_logger.info "Removing persistent connection client #%d" client_id;
  active_clients := IMap.remove client_id !active_clients

let add_client_to_clients clients client_id = client_id :: clients

let remove_client_from_clients clients client_id = List.filter (fun id -> id != client_id) clients

let get_subscribed_clients =
  List.fold_left
    (fun acc client_id ->
      match get_client client_id with
      | Some client when client.subscribed -> client :: acc
      | _ -> acc)
    []

let get_subscribed_lsp_clients =
  List.fold_left
    (fun acc client_id ->
      match get_client client_id with
      | Some client when client.subscribed -> client :: acc
      | _ -> acc)
    []

let update_clients ~clients ~errors_reason ~calc_errors_and_warnings =
  let subscribed_clients = get_subscribed_clients clients in
  let subscribed_client_count = List.length subscribed_clients in
  let all_client_count = List.length clients in
  if subscribed_clients <> [] then (
    let (errors, warnings) = calc_errors_and_warnings () in
    let error_count = Errors.ConcreteLocPrintableErrorSet.cardinal errors in
    let warning_file_count = Utils_js.FilenameMap.cardinal warnings in
    Hh_logger.info
      "sending (%d errors) and (warnings from %d files) to %d subscribed clients (of %d total)"
      error_count
      warning_file_count
      subscribed_client_count
      all_client_count;
    List.iter (send_errors ~errors_reason ~errors ~warnings) subscribed_clients
  )

let send_lsp clients json =
  clients |> get_subscribed_lsp_clients |> List.iter (send_single_lsp json)

let send_start_recheck clients =
  clients |> get_subscribed_clients |> List.iter send_single_start_recheck

let send_end_recheck ~lazy_stats clients =
  clients |> get_subscribed_clients |> List.iter (send_single_end_recheck ~lazy_stats)

let subscribe_client ~client ~current_errors ~current_warnings =
  Hh_logger.info "Subscribing client #%d to push diagnostics" client.client_id;
  if client.subscribed then
    (* noop *)
    ()
  else
    let errors_reason = Prot.New_subscription in
    send_errors ~errors_reason ~errors:current_errors ~warnings:current_warnings client;
    client.subscribed <- true

let client_did_open (client : single_client) ~(files : (string * string) Nel.t) : bool =
  (match Nel.length files with
  | 1 -> Hh_logger.info "Client #%d opened %s" client.client_id (files |> Nel.hd |> fst)
  | len -> Hh_logger.info "Client #%d opened %d files" client.client_id len);
  Nel.iter (fun (filename, _content) -> remove_cache_entry client filename) files;
  let add_file acc (filename, content) = SMap.add filename content acc in
  let new_opened_files = Nel.fold_left add_file client.opened_files files in
  (* SMap.add ensures physical equality if the map is unchanged, since 4.0.3,
   * so == is appropriate. *)
  if new_opened_files == client.opened_files then
    (* noop *)
    false
  else (
    client.opened_files <- new_opened_files;
    true
  )

let client_did_change
    (client : single_client)
    (fn : string)
    (changes : Lsp.DidChange.textDocumentContentChangeEvent list) :
    (unit, string * Utils.callstack) result =
  remove_cache_entry client fn;
  try
    let content = SMap.find fn client.opened_files in
    match Lsp_helpers.apply_changes content changes with
    | Error (reason, stack) -> Error (reason, stack)
    | Ok new_content ->
      let new_opened_files = SMap.add fn new_content client.opened_files in
      client.opened_files <- new_opened_files;
      Ok ()
  with Not_found as e ->
    let e = Exception.wrap e in
    let stack = Exception.get_backtrace_string e in
    Error (Printf.sprintf "File %s wasn't open to change" fn, Utils.Callstack stack)

let client_did_close (client : single_client) ~(filenames : string Nel.t) : bool =
  (match Nel.length filenames with
  | 1 -> Hh_logger.info "Client #%d closed %s" client.client_id (filenames |> Nel.hd)
  | len -> Hh_logger.info "Client #%d closed %d files" client.client_id len);
  Nel.iter (remove_cache_entry client) filenames;
  let remove_file acc filename = SMap.remove filename acc in
  let new_opened_files = Nel.fold_left remove_file client.opened_files filenames in
  (* SMap.remove ensures physical equality if the set is unchanged,
   * so == is appropriate. *)
  if new_opened_files == client.opened_files then
    (* noop *)
    false
  else (
    client.opened_files <- new_opened_files;
    true
  )

let get_file (client : single_client) (fn : string) : File_input.t =
  let content_opt = SMap.find_opt fn client.opened_files in
  match content_opt with
  | None -> File_input.FileName fn
  | Some content -> File_input.FileContent (Some fn, content)

let get_opened_files (clients : t) : SSet.t =
  let per_file filename _content acc = SSet.add filename acc in
  let per_client acc client_id =
    match get_client client_id with
    | None -> acc
    | Some client -> SMap.fold per_file client.opened_files acc
  in
  List.fold_left per_client SSet.empty clients

let get_id client = client.client_id

let client_snippet_support (client : single_client) =
  Lsp.Initialize.(
    client.lsp_initialize_params.client_capabilities.textDocument.completion.completionItem
      .snippetSupport)

let type_contents_cache client = client.type_contents_cache

let clear_type_contents_caches () =
  IMap.iter (fun _key client -> FilenameCache.clear client.type_contents_cache) !active_clients
