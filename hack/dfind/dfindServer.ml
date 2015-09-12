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
open Utils

(*****************************************************************************)
(* Processing an fsnotify event *)
(*****************************************************************************)

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
    then SSet.union dirty (SMap.find_unsafe path env.dirs)
    else begin
      let dir_content =
        try SMap.find_unsafe wpath env.dirs
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

let run_daemon roots (ic, oc) =
  let roots = List.map Path.to_string roots in
  let env = DfindEnv.make roots in
  List.iter (DfindAddFile.path env) roots;
  let acc = ref SSet.empty in
  let descr_in = Daemon.descr_of_in_channel ic in
  while true do
    let fsnotify_callback events =
      acc := List.fold_left (process_fsnotify_event env) !acc events
    in
    let message_in_callback () =
      (* XXX can we just select() on the writability of the oc? *)
      let () = Daemon.from_channel ic in
      Daemon.to_channel oc !acc;
      acc := SSet.empty
    in
    let read_fdl = [(descr_in, message_in_callback)] in
    let timeout = -1.0 in
    Fsnotify.select env.fsnotify ~read_fdl ~timeout fsnotify_callback
  done
