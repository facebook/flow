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
(* Fork and work *)
(*****************************************************************************)

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
