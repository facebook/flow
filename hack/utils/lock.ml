(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

let lock_fds = ref SMap.empty

(**
 * Basic lock operations.
 *
 * We use these for two reasons:
 * 1. making sure we are only running one instance of hh_server per person on a given dev box
 * 2. giving a way to hh_client to check if a server is running.
 *)

let lock_name root file =
    let tmp_dir = Tmp.get_dir () in
    let root_part = Path.slash_escaped_string_of_path root in
    Printf.sprintf "%s/%s.%s" tmp_dir root_part file

(**
 * Grab or check if a file lock is available.
 *
 * Returns true if the lock is/was available, false otherwise.
 *)
let _operations root op file : bool =
  try
    let lock_file = lock_name root file in
    let fd = match SMap.get lock_file !lock_fds with
      | None ->
          Sys_utils.with_umask 0o111 begin fun () ->
            let fd = Unix.descr_of_out_channel (open_out lock_file) in
            lock_fds := SMap.add lock_file fd !lock_fds;
            fd
          end
      | Some fd -> fd
    in
    let _ = Unix.lockf fd op 1 in
    true
  with _ ->
    false

(**
 * Grabs the file lock and returns true if it the lock was grabbed
 *)
let grab root file : bool =
  _operations root Unix.F_TLOCK file

(**
 * Releases a file lock.
 *)
let release root file : bool =
  _operations root Unix.F_ULOCK file

(**
 * Gets the server instance-unique integral fd for a given lock file.
 *)
let fd_of root file : int =
  let lock_file = lock_name root file in
  match SMap.get lock_file !lock_fds with
    | None -> -1
    | Some fd -> Obj.magic fd

(**
 * Check if the file lock is available without grabbing it.
 * Returns true if the lock is free.
 *)
let check root file : bool =
  _operations root Unix.F_TEST file
