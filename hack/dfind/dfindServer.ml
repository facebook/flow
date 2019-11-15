(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(*****************************************************************************)
(* Code relative to the client/server communication *)
(*****************************************************************************)

open Hh_core
open DfindEnv

type msg =
  | Ready
  | Updates of SSet.t

(*****************************************************************************)
(* Processing an fsnotify event *)
(*****************************************************************************)

let (process_fsnotify_event : DfindEnv.t -> SSet.t -> Fsnotify.event -> SSet.t)
    =
 fun env dirty event ->
  let { Fsnotify.path; wpath } = event in
  (* Tell everybody that this file has changed *)
  let dirty = SSet.add path dirty in
  (* Is it a directory? Be conservative, everything we know about this
   * directory is now "dirty"
   *)
  let dirty =
    if SMap.mem path env.dirs then
      SSet.union dirty (SMap.find path env.dirs)
    else
      let dir_content =
        (try SMap.find wpath env.dirs with Not_found -> SSet.empty)
      in
      env.dirs <- SMap.add wpath (SSet.add path dir_content) env.dirs;
      dirty
  in
  env.new_files <- SSet.empty;

  (* Add the file, plus all of the sub elements if it is a directory *)
  DfindAddFile.path env path;

  (* Add everything new we found in this directory
    * (empty when it's a regular file)
    *)
  let dirty = SSet.union env.new_files dirty in
  dirty

let run_daemon (scuba_table, roots) (ic, oc) =
  Printexc.record_backtrace true;
  let t = Unix.gettimeofday () in
  let infd = Daemon.descr_of_in_channel ic in
  let outfd = Daemon.descr_of_out_channel oc in
  let roots = List.map roots Path.to_string in
  let env = DfindEnv.make roots in
  List.iter roots (DfindAddFile.path env);
  EventLogger.dfind_ready scuba_table t;
  Marshal_tools.to_fd_with_preamble outfd Ready |> ignore;
  ignore @@ Hh_logger.log_duration "Initialization" t;
  let acc = ref SSet.empty in
  let descr_in = Daemon.descr_of_in_channel ic in
  let fsnotify_callback events =
    acc := List.fold_left events ~f:(process_fsnotify_event env) ~init:!acc
  in
  let message_in_callback () =
    let () = Marshal_tools.from_fd_with_preamble infd in
    let count = SSet.cardinal !acc in
    if count > 0 then Hh_logger.log "Sending %d file updates\n%!" count;
    Marshal_tools.to_fd_with_preamble outfd (Updates !acc) |> ignore;
    acc := SSet.empty
  in
  while true do
    let read_fdl = [(descr_in, message_in_callback)] in
    let timeout = -1.0 in
    Fsnotify.select env.fsnotify ~read_fdl ~timeout fsnotify_callback
  done

let entry_point = Daemon.register_entry_point "dfind" run_daemon
