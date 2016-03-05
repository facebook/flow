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
  client : (in_channel * out_channel) option;
  (* When this is true, we are between points when typechecker process
   * sent us RunIdeCommands and StopIdeCommands, and it's safe to use
   * data from shared heap. *)
  run_ide_commands : bool;
  (* IDE process's version of ServerEnv.file_info, synchronized periodically
   * from typechecker process. *)
  files_info : FileInfo.t Relative_path.Map.t;
}

let empty_env = {
  client = None;
  run_ide_commands = false;
  files_info = Relative_path.Map.empty;
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
  | Fun of (unit -> job list)

let get_ready wait_handles =
  let funs, channels = List.partition_map wait_handles ~f:begin function
    | Fun x -> `Fst x
    | Channel (x, y) -> `Snd (x, y)
  end in
  let ready_funs = List.concat_map funs ~f:(fun f -> f ()) in
  let wait_time = if ready_funs = [] then 1.0 else 0.0 in
  let fds = List.map channels ~f:fst in
  let readable, _, _ = Unix.select fds [] [] wait_time in
  let ready_channels = List.filter_map channels ~f:begin fun (fd, job) ->
    Option.map (List.find readable ~f:(fun x -> x = fd)) ~f:(fun _ -> job)
  end in
  ready_funs @ ready_channels

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
  Marshal.to_channel oc response [];
  close_out oc

let handle_new_client parent_in_fd env  =
  let socket = Libancillary.ancil_recv_fd parent_in_fd in
  let ic, oc =
    (Unix.in_channel_of_descr socket), (Unix.out_channel_of_descr socket) in
  match env.client with
  | None ->
    Hh_logger.log "Connected new client";
    { env with client = Some (ic, oc) }
  | Some _ ->
    Hh_logger.log "Rejected a client";
    handle_already_has_client oc; env

let get_call_response env id call =
  if not env.run_ide_commands then
    IdeJsonUtils.json_string_of_server_busy id
  else
    IdeServerCall.get_call_response id call env.files_info

let handle_gone_client env =
  Hh_logger.log "Client went away";
  { env with client = None }

let handle_client_request (ic, oc) env =
  Hh_logger.log "Handling client request";
  try
    let request = Marshal.from_channel ic in
    match IdeJsonUtils.call_of_string request with
    | ParsingError e ->
      Hh_logger.log "Received malformed request: %s" e;
      env
    | InvalidCall (id, e) ->
      let response = IdeJsonUtils.json_string_of_invalid_call id e in
      Marshal.to_channel oc response [];
      flush oc;
      env
    | Call (id, call) ->
      let response = get_call_response env id call in
      Marshal.to_channel oc response [];
      flush oc;
      env
  with
  | End_of_file
  | Sys_error _ ->
    (* client went away in the meantime *)
    handle_gone_client env

let handle_typechecker_message typechecker_process env  =
  match IdeProcessPipe.recv typechecker_process with
  | IdeProcessMessage.RunIdeCommands ->
    { env with run_ide_commands = true }
  | IdeProcessMessage.StopIdeCommands ->
    IdeProcessPipe.send typechecker_process IdeProcessMessage.IdeCommandsDone;
    { env with run_ide_commands = false }
  | IdeProcessMessage.SyncFileInfo updated_files_info ->
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

let handle_server_idle env =
  if env.run_ide_commands then IdeIdle.go ();
  env

let get_jobs typechecker parent_in_fd =
  let idle_handle = Fun (
    fun () -> if IdeIdle.has_tasks () then [{
      priority = 3;
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
  [idle_handle; typechecker_handle; monitor_handle]

let get_client_job ((client_ic, _) as client) =
  Channel (
    Unix.descr_of_in_channel client_ic, {
      priority = 0;
      run = handle_client_request client
    }
  )

let daemon_main _ (parent_ic, _parent_oc) =
  Printexc.record_backtrace true;
  SharedMem.enable_local_writes ();
  let parent_in_fd = Daemon.descr_of_in_channel parent_ic in
  let typechecker_process = IdeProcessPipeInit.ide_recv parent_in_fd in
  let env = ref empty_env in
  IdeIdle.init ();
  while true do
    ServerMonitorUtils.exit_if_parent_dead ();
    let new_env = try
      let jobs = get_jobs typechecker_process parent_in_fd in
      let jobs = match !env.client with
        | Some client -> (get_client_job client) :: jobs
        | None -> jobs
      in
      let ready_jobs = get_ready jobs in
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
