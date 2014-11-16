(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Code relative to the client/server communication *)
(*****************************************************************************)

open DfindEnv

type message =
  | Find_handle_follow of DfindEnv.dir * DfindEnv.handle
  | Find_handle   of DfindEnv.dir * DfindEnv.handle
  | Check
  | Kill
  | Ping

(*****************************************************************************)
(* Some helpers *)
(*****************************************************************************)

let is_prefix dir file =
  String.length file > String.length dir &&
  String.sub file 0 (String.length dir) = dir &&
  file.[String.length dir] = '/'

(*****************************************************************************)
(* Processing an fsnotify event *)
(*****************************************************************************)

(* Die if something unexpected happened *)

let (process_fsnotify_event:
       DfindEnv.t -> SSet.t -> Fsnotify.event
         -> SSet.t) = fun env dirty event ->
  let { Fsnotify.path; wpath; } = event in

  (* Tell everybody that this file has changed *)
  let dirty = SSet.add path dirty in
  (* Is it a directory? Be conservative, everything we know about this
    * directory is now "dirty"
    *)
  let dirty =
    if SMap.mem path env.dirs
    then SSet.union dirty (SMap.find path env.dirs)
    else begin
      let dir_content =
        try SMap.find wpath env.dirs
        with Not_found -> SSet.empty
      in
      env.dirs <- SMap.add wpath (SSet.add path dir_content) env.dirs;
      dirty
    end
  in
  env.new_files <- SSet.empty;
  (* Add the file, plus all of the sub elements if it is a directory *)
  DfindAddFile.path env path;
  (* Add everything new we found in this directory
    * (empty when it's a regular file)
    *)
  let dirty = SSet.union env.new_files dirty in
  dirty

(*****************************************************************************)
(* Processing a user handle
 * dfind my_dir/ my_handle
 *)
(*****************************************************************************)

let set_handle_time env handle =
  (* Replace the current time of the handle to now. *)
  let new_time = Time.get() in
  set_handle env handle new_time

let get_all_files env (dir, h as handle) =
  (* Find the time when the handle was created *)
  let time =
    match get_handle env handle with
    | None -> Time.bot
    | Some x -> x
  in
  let acc = ref SSet.empty in
  (* Now walk the tree, but cut the branches that are too old *)
  TimeFiles.walk
    (fun x -> x < time) (* Cut everything that is too old *)
    (fun (file_time, file) ->
      if file_time >= time then begin
        acc := SSet.add file !acc;
      end
    )
    env.files;
  set_handle_time env handle;
  let acc = SSet.filter (is_prefix dir) !acc in
  acc

let print_handle ~close env handle oc =
  let acc = get_all_files env handle in
  add_output env ~close oc acc;
  ()

(*****************************************************************************)
(* Section defining the functions called by the server
 * Whenever an fsnotify event is received, process_event is called
 * Whenever a new message is received, process_message is called
 *)
(*****************************************************************************)

(* Send to client sends a message to a specific client
 * the client is a triplet (directory_of_interest, handle, output_channel).
*)
let (send_to_client: DfindEnv.t -> SSet.t -> DfindEnv.client -> unit) =
  fun env dirty (dir, handle, oc) ->
    add_output env ~close:false oc dirty

let (process_fsnotify_events: DfindEnv.t -> Fsnotify.event list -> unit) =
  fun env evs ->
  (* What's new? *)
  let dirty = List.fold_left (process_fsnotify_event env) SSet.empty evs in
  let time = Time.get() in
  (* Insert the files with the current timestamp *)
  SSet.iter begin fun file ->
(*
  Printf.fprintf env.log "Adding %s[%s]\n" file (Time.to_string time);
  flush env.log;
*)
    env.files <- TimeFiles.add (time, file) env.files
  end dirty;
  let clients = get_clients env in
  (* Reset the list of clients, they will re-insert themselves *)
  (* Notify every listener that something changed *)
  List.iter (send_to_client env dirty) clients

let process_handle ~close env dir handle oc =
  if not (SMap.mem dir env.dirs)
  then DfindAddFile.path env dir;
  let dir_handle = dir, handle in
  print_handle ~close env dir_handle oc

let process_client_msg env oc = function
  | Find_handle_follow (dir, handle) ->
      process_handle ~close:false env dir handle oc;
      add_client env (dir, handle, oc)
  | Find_handle (dir, handle) ->
      process_handle ~close:true env dir handle oc
  | Check -> () (* TODO check *)
  | Kill ->
      exit 0
  | Ping ->
      let oc = Unix.out_channel_of_descr oc in
      output_string oc "OK\n";
      close_out oc

(*****************************************************************************)
(* Generic code to create a socket in Ocaml *)
(*****************************************************************************)

let server_socket, client_socket =
  let tmp = Tmp.temp_dir_name in
  let user = Sys.getenv "USER" in
  let sock_name = tmp ^ "/dfind_"^user^".sock" in
  begin fun () -> (* Server side *)
    try
      if Sys.file_exists sock_name then Sys.remove sock_name;
      let sockaddr = Unix.ADDR_UNIX sock_name in
      let domain = Unix.PF_UNIX in
      let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
      let _ = Unix.bind sock sockaddr in
      let _ = Unix.listen sock 10 in
      sock
    with Unix.Unix_error (err, _, _) ->
      exit 1
  end,
  begin fun () -> (* Client side *)
    try
      let sockaddr = Unix.ADDR_UNIX sock_name in
      let domain = Unix.PF_UNIX in
      let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
      Unix.connect sock sockaddr ;
      let ic = Unix.in_channel_of_descr sock in
      let oc = Unix.out_channel_of_descr sock in
      ic, oc
    with _ ->
      Printf.fprintf stderr "Error: could not connect to the server\n";
      exit 3
  end

(*****************************************************************************)
(* We use a pid file for 2 purposes:
 * - Know what the pid of the server is quickly
 * - Be able to tell quickly if the server is running
 *
 * The server, when it starts, creates and then locks the pid file (without
 * unlocking).
 * So we know a server is already runnning by testing the lock on the pid
 * file
*)
(*****************************************************************************)

let get_pid_file () =
  let tmp = Tmp.temp_dir_name in
  let user = Sys.getenv "USER" in
  let fn = tmp ^ "/dfind_"^user^".pid" in
  fn

let is_running () =
  let fn = get_pid_file() in
  Sys.file_exists fn &&
  try
    let fd = Unix.openfile fn [Unix.O_RDONLY] 0o640 in
    Unix.lockf fd Unix.F_TEST 1;
    false
  with _ ->
    true

(* create and the lock the pid file (lock forever) *)
let lock_pid_file () =
  let fn = get_pid_file() in
  let fd = Unix.openfile fn [Unix.O_RDWR; Unix.O_CREAT] 0o640 in
  Unix.lockf fd Unix.F_LOCK 1;
  let oc = Unix.out_channel_of_descr fd in
  output_string oc (string_of_int (Unix.getpid()));
  output_string oc "\n";
  flush oc

(*****************************************************************************)
(* Functions used to notify that the server is ready *)
(*****************************************************************************)

(* Function used by the client to make sure the server is ready to listen *)
let (wait_for_server: Unix.file_descr -> unit) = fun ready_in ->
  assert (Unix.read ready_in " " 0 1 = 1);
  Unix.close ready_in

(* Used by the server to wake up the client waiting *)
let (notify_client: Unix.file_descr -> unit) = fun ready_out ->
  assert (Unix.write ready_out "X" 0 1 = 1);
  Unix.close ready_out

(*****************************************************************************)
(* Fork and work *)
(*****************************************************************************)

let half_hour = 1800.0
let one_day = 86400.0

let exit_if_unused env =
  let time_since_start: float = Unix.time() -. env.start_time in
  let time_since_query: float = Unix.time() -. env.last_query in
  (* quit the server after a day at an opportune time *)
  if time_since_start > one_day && time_since_query >= half_hour
  then begin
    Printf.fprintf env.log "Exiting server after 24 hours\n";
    flush env.log;
    exit 5;
  end

let daemon env socket =
  Printf.fprintf env.log "Status: Starting daemon\n"; flush env.log;

  while true do
    let output_descrl = DfindEnv.get_output_descrl env in
    exit_if_unused env;
    let write_callback = fun () -> output env in
    let write_fdl = List.map (fun fd -> (fd, write_callback)) output_descrl in

    let socket_callback () = 
      try
        Printf.fprintf env.log "STATUS: message received\n"; flush env.log;
        env.last_query <- Unix.time();
        let cli, _ = Unix.accept socket in
        let ic = Unix.in_channel_of_descr cli in
        let msg = Marshal.from_channel ic in
        process_client_msg env cli msg;
        Printf.fprintf env.log "STATUS: done\n"; flush env.log;
      with e ->
        Printf.fprintf env.log "Exception: %s\n" (Printexc.to_string e);
        flush env.log;
        ()
    in let read_fdl = [
      socket, socket_callback
    ] in

    let fsnotify_callback = process_fsnotify_events env in
    let timeout = half_hour in
    Fsnotify.select env.fsnotify ~read_fdl ~write_fdl ~timeout fsnotify_callback
  done

let daemon_from_pipe env message_in result_out =
  let env = { env with log = stdout; } in
  let acc = ref SSet.empty in
  while true do
    let fsnotify_callback events = 
      acc := List.fold_left (process_fsnotify_event env) !acc events
    in let message_in_callback () = 
      let ic = Unix.in_channel_of_descr message_in in
      flush env.log;
      let msg = Marshal.from_channel ic in
      assert (msg = "Go");
      let result_out = Unix.out_channel_of_descr result_out in
      Marshal.to_channel result_out !acc [];
      flush result_out;
      acc := SSet.empty
    in let read_fdl = [(message_in, message_in_callback)] in
    let timeout = -1.0 in
    Fsnotify.select env.fsnotify ~read_fdl ~timeout fsnotify_callback
  done

let fork_in_pipe root =
  let msg_in, msg_out = Unix.pipe() in
  let result_in, result_out = Unix.pipe() in
  match Unix.fork() with
  | -1 -> failwith "Go get yourself a real computer"
  | 0 ->
      Unix.close msg_out;
      Unix.close result_in;
      let env = DfindEnv.make root in
      DfindAddFile.path env root;
      daemon_from_pipe env msg_in result_out;
      assert false
  | pid ->
      Unix.close msg_in;
      Unix.close result_out;
      msg_out, result_in, pid
