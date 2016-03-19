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
open IdeJson

type env = {
  typechecker : IdeProcessPipe.to_typechecker;
  tcopt : TypecheckerOptions.t;
  (* Persistent client talking JSON protocol *)
  client : (in_channel * out_channel) option;
  (* Non-persistent clients awaiting to receive Hello message and send their
   * single ServerCommand request *)
  requests : (Timeout.in_channel * out_channel) list;
  (* Whether typechecker has finished full initialization. In the future, we
   * can have more granularity here, allowing some queries to run sooner. *)
  typechecker_init_done : bool;
  (* IDE process's version of ServerEnv.file_info, synchronized periodically
   * from typechecker process. *)
  files_info : FileInfo.t Relative_path.Map.t;
  (* IDE process's version of ServerEnv.errorl, synchronized periodically
   * from typechecker process. *)
  errorl : Errors.t
}

let build_env typechecker tcopt = {
  typechecker = typechecker;
  tcopt = tcopt;
  client = None;
  requests = [];
  typechecker_init_done = false;
  files_info = Relative_path.Map.empty;
  errorl = [];
}

type job = {
  priority : int;
  run : env -> env;
}

type wait_handle =
  (* Job that should be run when file descriptor is ready *)
  | Channel of Unix.file_descr * job
  (* Job that should be run if provided function tells us that there is
   * something to do *)
  | Fun of (env -> job list)

let get_ready env wait_handles =
  let funs, channels = List.partition_map wait_handles ~f:begin function
    | Fun x -> `Fst x
    | Channel (x, y) -> `Snd (x, y)
  end in
  let ready_funs = List.concat_map funs ~f:(fun f -> f env) in
  let wait_time = if ready_funs = [] then 1.0 else 0.0 in
  let fds = List.map channels ~f:fst in
  let readable, _, _ = Unix.select fds [] [] wait_time in
  let ready_channels = List.filter_map channels ~f:begin fun (fd, job) ->
    Option.map (List.find readable ~f:(fun x -> x = fd)) ~f:(fun _ -> job)
  end in
  ready_funs @ ready_channels

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
      { env with client = Some (ic, oc) }
    | Some _ ->
      Hh_logger.log "Rejected a client";
      handle_already_has_client oc; env
    end
  | ServerMonitorUtils.Request ->
    Hh_logger.log "Connected new single request client";
    let ic, oc = Timeout.in_channel_of_descr socket, oc in
    { env with requests = (ic, oc)::env.requests }

let send_call_to_typecheker env id = function
  | IdeServerCall.Find_refs_call action ->
    let msg = IdeProcessMessage.Find_refs_call (id, action) in
    IdeProcessPipe.send env.typechecker msg

let send_typechecker_response_to_client env id response =
  Option.iter env.client begin fun (_, oc) ->
    let response = IdeJsonUtils.json_string_of_response id response in
    write_string_to_channel response oc;
  end

(* Will return a response for the client, or None if the response is going to
 * be computed asynchronously *)
let get_call_response env id call =
  let busy = fun () -> IdeJsonUtils.json_string_of_server_busy id in
  if not env.typechecker_init_done then Some (busy ()) else
  let response = SharedMem.try_lock_hashtable ~do_:begin fun () ->
    IdeServerCall.get_call_response id call env.tcopt env.files_info env.errorl
  end in
  match response with
  | Some (IdeServerCall.Deferred_to_typechecker call) ->
      send_call_to_typecheker env id call;
      None
  | Some (IdeServerCall.Result response) ->
      Some (IdeJsonUtils.json_string_of_response id response)
  | None -> Some (busy ())

let handle_gone_client env =
  Hh_logger.log "Client went away";
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
      begin match get_call_response env id call with
        | Some response -> write_string_to_channel response oc
        | None -> ()
      end;
      env
  with
  | End_of_file
  | Sys_error _ ->
    (* client went away in the meantime *)
    handle_gone_client env

let handle_typechecker_message typechecker_process env  =
  match IdeProcessPipe.recv typechecker_process with
  | IdeProcessMessage.Typechecker_init_done ->
    { env with typechecker_init_done = true }
  | IdeProcessMessage.Sync_file_info updated_files_info ->
    Hh_logger.log "Received file info updates for %d files"
      (Relative_path.Map.cardinal updated_files_info);
    let new_files_info = Relative_path.Map.merge begin fun _ x y ->
        Option.merge x y (fun _ y -> y)
      end
      env.files_info
      updated_files_info in
    HackSearchService.IdeProcessApi.enqueue_updates
      (Relative_path.Map.keys updated_files_info);
    { env with files_info = new_files_info }
  | IdeProcessMessage.Sync_error_list errorl ->
    Hh_logger.log "Received error list update";
    { env with errorl = errorl }
  | IdeProcessMessage.Find_refs_response (id, response) ->
    let response = IdeJson.Find_refs_response response in
    send_typechecker_response_to_client env id response;
    env

let handle_server_idle env =
  if IdeIdle.has_tasks () then
    ignore (SharedMem.try_lock_hashtable ~do_:(fun () -> IdeIdle.go ()));
  env

(* This is similar to ServerCommand.handle, except that it handles only SEARCH
 * queries which cannot be handled there anymore *)
let handle_waiting_hh_client_requests env =
  List.iter env.requests begin fun (ic, oc) ->
    try
      ServerCommand.say_hello oc;
      let msg = ServerCommand.read_client_msg ic in

      match msg with
      | ServerCommand.Rpc ServerRpc.SEARCH (query, type_) ->
        let response = ServerSearch.go query type_ in
        ServerCommand.send_response_to_client (ic, oc) response;
      | _ -> assert false

    with
    | Sys_error("Broken pipe")
    | ServerCommand.Read_command_timeout ->
      ServerUtils.shutdown_client (ic, oc)
  end;
  {env with requests = []}

let get_jobs typechecker parent_in_fd =
  let idle_handle = Fun (
    fun _ -> if IdeIdle.has_tasks () then [{
      priority = 4;
      run = handle_server_idle
    }] else []
  ) in
  let typechecker_handle = Channel (
    typechecker.IdeProcessPipe.in_fd, {
      priority = 2;
      run =  handle_typechecker_message typechecker
    }
  ) in
  let monitor_handle = Channel (
    parent_in_fd, {
      priority = 1;
      run = handle_new_client parent_in_fd
    }
  ) in
  let hh_clients_handle = Fun (
    fun env -> if
      (not (List.is_empty env.requests)) &&
      env.typechecker_init_done &&
      (not (HackSearchService.IdeProcessApi.updates_pending ()))
    then [{
      priority = 3;
      run = handle_waiting_hh_client_requests;
    }] else []
  ) in
  [idle_handle; typechecker_handle; monitor_handle; hh_clients_handle]

let get_client_job ((client_ic, _) as client) =
  Channel (
    Unix.descr_of_in_channel client_ic, {
      priority = 0;
      run = handle_client_request client
    }
  )

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

  IdeIdle.init ();
  while true do
    ServerMonitorUtils.exit_if_parent_dead ();
    let new_env = try
      let jobs = get_jobs typechecker_process parent_in_fd in
      let jobs = match !env.client with
        | Some client -> (get_client_job client) :: jobs
        | None -> jobs
      in
      let ready_jobs = get_ready !env jobs in
      let sorted_ready_jobs = List.sort ready_jobs ~cmp:begin fun x y ->
        x.priority - y.priority
      end in
      List.fold_right sorted_ready_jobs
        ~init:!env
        ~f:(fun job env -> job.run env)
    with
    | IdeProcessPipe.IDE_process_pipe_broken ->
      Hh_logger.log "Typechecker has died, exiting too.";
      Exit_status.(exit IDE_typechecker_died);
    in
    env := new_env
  done

let entry =
  Daemon.register_entry_point "IdeMain.daemon_main" daemon_main
