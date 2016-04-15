(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open Core
open IdeEnv
open IdeJson

(* Wrapper to ensure flushing and type safety of sent type *)
let write_string_to_channel (s : string) oc =
  let oc_fd = Unix.descr_of_out_channel oc in
  Marshal_tools.to_fd_with_preamble oc_fd s

let handle_already_has_client oc =
  let response = Hh_json.(json_to_string (
    JSON_Object [
      ("type", JSON_String "response");
      ("success", JSON_Bool false);
      ("message", JSON_String
         ("There already is a client connected in IDE mode.")
      );
    ]
  )) in
  write_string_to_channel response oc;
  close_out oc

(* This is similar to ServerCommand.handle, except that it handles only SEARCH
 * queries which cannot be handled there anymore *)
let handle_waiting_hh_client_request (ic, oc) env =
  Hh_logger.log "Handling hh_client request";
  (try
    ServerCommand.say_hello oc;
    let msg = ServerCommand.read_client_msg ic in

    match msg with
    | ServerCommand.Rpc ServerRpc.SEARCH (query, type_) ->
      let response = ServerSearch.go None query type_ in
      ServerCommand.send_response_to_client (ic, oc) response;
    | _ -> assert false

  with
  | Sys_error("Broken pipe")
  | ServerCommand.Read_command_timeout ->
    ServerUtils.shutdown_client (ic, oc));
  env

let send_call_to_typechecker env id = function
  | IdeServerCall.Find_refs_call action ->
    let msg = IdeProcessMessage.Find_refs_call (id, action) in
    IdeProcessPipe.send env.typechecker msg;
    env
  | IdeServerCall.Status_call ->
    let msg = IdeProcessMessage.Start_recheck in
    IdeProcessPipe.send env.typechecker msg;
    { env with persistent_client_requests =
      (id, Status_call) :: env.persistent_client_requests;
    }

let send_typechecker_response_to_client env id response =
  Option.iter env.client begin fun (_, oc) ->
    let response = IdeJsonUtils.json_string_of_response id response in
    write_string_to_channel response oc;
  end

(* Will return a response for the client, or None if the response is going to
 * be computed asynchronously *)
let get_call_response env id call =
  let response = IdeServerCall.get_call_response id call env in
  match response with
  | IdeServerCall.Deferred_to_typechecker call ->
      send_call_to_typechecker env id call, None
  | IdeServerCall.Result response ->
      env, Some (IdeJsonUtils.json_string_of_response id response)
  | IdeServerCall.Server_busy ->
      env, Some (IdeJsonUtils.json_string_of_server_busy id)

let handle_gone_client ic env =
  Hh_logger.log "Client went away";
  let fd = Unix.descr_of_in_channel ic in
  IdeScheduler.stop_waiting_for_channel fd;
  { env with client = None }

let handle_client_request (ic, oc) env =
  Hh_logger.log "Handling client request";
  try
    let request = Marshal.from_channel ic in
    match IdeJsonUtils.call_of_string request with
    | Parsing_error e ->
      Hh_logger.log "Received malformed request: %s" e;
      env
    | Invalid_call (id, e) ->
      let response = IdeJsonUtils.json_string_of_invalid_call id e in
      write_string_to_channel response oc;
      flush oc;
      env
    | Call (id, call) ->
      let env, response = get_call_response env id call in
      begin match response with
        | Some response -> write_string_to_channel response oc
        | None -> ()
      end;
      env
  with
  | End_of_file
  | Sys_error _ ->
    (* client went away in the meantime *)
    handle_gone_client ic env

let handle_recheck_done env =
  (* if rechecking is done, we assume that the error list is stabilized, and
   * we can answer any pending Status calls. *)
  let ready_requests, pending_requests =
    List.partition_map env.persistent_client_requests ~f:begin function
      | (id, Status_call) -> `Fst id
      | x -> `Snd x
    end in
  List.iter ready_requests (fun id ->
    let errorl = List.map (Errors.get_error_list env.errorl)
        Errors.to_absolute in
    let response = Status_response (ServerError.get_errorl_json errorl) in
    send_typechecker_response_to_client env id response
  );
  { env with persistent_client_requests = pending_requests }

let handle_new_client parent_in_fd env  =
  let socket = Libancillary.ancil_recv_fd parent_in_fd in
  let ic, oc =
    (Unix.in_channel_of_descr socket), (Unix.out_channel_of_descr socket) in
  let client_type =
    Marshal_tools.from_fd_with_preamble (Unix.descr_of_in_channel ic) in

  match client_type with
  | ServerMonitorUtils.Persistent ->
    begin match env.client with
    | None ->
      Hh_logger.log "Connected new persistent client";
      IdeScheduler.wait_for_channel
        (Unix.descr_of_in_channel ic)
        (handle_client_request (ic, oc))
        ~priority:IdePriorities.persistent_client_request;
      { env with client = Some (ic, oc) }
    | Some _ ->
      Hh_logger.log "Rejected a client";
      handle_already_has_client oc; env
    end
  | ServerMonitorUtils.Request ->
    Hh_logger.log "Connected new single request client";
    let ic, oc = Timeout.in_channel_of_descr socket, oc in
    IdeScheduler.wait_for_fun
      (fun env ->
        env.typechecker_init_done && (not (IdeSearch.updates_pending ()))
      )
      (handle_waiting_hh_client_request (ic, oc))
      ~once:true
      ~priority:IdePriorities.hh_client_request;
    env

let handle_typechecker_message env =
  match IdeProcessPipe.recv env.typechecker with
  | IdeProcessMessage.Typechecker_init_done ->
    let env = handle_recheck_done env in
    { env with typechecker_init_done = true }
  | IdeProcessMessage.Sync_file_info updated_files_info ->
    Hh_logger.log "Received file info updates for %d files"
      (Relative_path.Map.cardinal updated_files_info);
    let new_files_info = Relative_path.Map.merge begin fun _ x y ->
        Option.merge x y (fun _ y -> y)
      end
      env.files_info
      updated_files_info in
    IdeSearch.enqueue_updates (Relative_path.Map.keys updated_files_info);
    { env with files_info = new_files_info }
  | IdeProcessMessage.Sync_error_list errorl ->
    Hh_logger.log "Received error list update";
    { env with errorl = errorl }
  | IdeProcessMessage.Find_refs_response (id, response) ->
    let response = IdeJson.Find_refs_response response in
    send_typechecker_response_to_client env id response;
    env
  | IdeProcessMessage.Recheck_finished ->
    handle_recheck_done env

let init_scheduler monitor_in_fd typechecker_in_fd =
  IdeScheduler.wait_for_channel
    monitor_in_fd
    (handle_new_client monitor_in_fd)
    ~priority:IdePriorities.new_client;
  IdeScheduler.wait_for_channel
    typechecker_in_fd
    handle_typechecker_message
    ~priority:IdePriorities.typechecker_message;
  ()

let daemon_main options (parent_ic, _parent_oc) =
  Printexc.record_backtrace true;

  let gc_control = Gc.get () in
  Gc.set {gc_control with Gc.max_overhead = 200};

  SharedMem.enable_local_writes ();
  let parent_in_fd = Daemon.descr_of_in_channel parent_ic in
  let typechecker_process = IdeProcessPipeInit.ide_recv parent_in_fd in

  let config = ServerConfig.(load filename options) in
  let tcopt = ServerConfig.typechecker_options config in
  let env = ref (build_env typechecker_process tcopt) in

  init_scheduler parent_in_fd typechecker_process.IdeProcessPipe.in_fd;
  while true do
    ServerMonitorUtils.exit_if_parent_dead ();
    let new_env = try
      IdeScheduler.wait_and_run_ready !env
    with
    | IdeProcessPipe.IDE_process_pipe_broken ->
      Hh_logger.log "Typechecker has died, exiting too.";
      Exit_status.(exit IDE_typechecker_died);
    in
    env := new_env
  done

let entry =
  Daemon.register_entry_point "IdeMain.daemon_main" daemon_main
