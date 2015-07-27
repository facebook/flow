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

let name root file =
  let tmp_dir = Tmp.get_dir () in
  let root_part = Path.slash_escaped_string_of_path root in
  Printf.sprintf "%s/%s.%s" tmp_dir root_part file

let register_lock lock_file =
  Sys_utils.with_umask 0o111 begin fun () ->
    let fd = Unix.descr_of_out_channel (open_out lock_file) in
    let st = Unix.fstat fd in
    lock_fds := SMap.add lock_file (fd, st) !lock_fds;
    fd
  end

(**
 * Grab or check if a file lock is available.
 *
 * Returns true if the lock is/was available, false otherwise.
 *)
let _operations lock_file op : bool =
  try
    let fd = match SMap.get lock_file !lock_fds with
      | None -> register_lock lock_file
      | Some (fd, st) ->
          let identical_file =
            try
              (* Note: I'm carefully avoiding opening another fd to the
               * lock_file when doing this check, because closing any file
               * descriptor to a given file will release the locks on *all*
               * file descriptors that point to that file. Fortunately, stat()
               * gets us our information without opening a fd *)
              let current_st = Unix.stat lock_file in
              Unix.(st.st_dev = current_st.st_dev &&
                st.st_ino = current_st.st_ino)
            with _ ->
              false
          in
          if not identical_file then
            (* Looks like someone (tmpwatch?) deleted the lock file; don't
             * create another one, because our socket is probably gone too.
             * We are dead in the water. *)
            raise Exit
          else
            fd
    in
    let _ = Unix.lockf fd op 1 in
    true
  with _ ->
    false

(**
 * Grabs the file lock and returns true if it the lock was grabbed
 *)
let grab lock_file : bool = _operations lock_file Unix.F_TLOCK

(**
 * Releases a file lock.
 *)
let release lock_file : bool = _operations lock_file Unix.F_ULOCK

(**
 * Gets the server instance-unique integral fd for a given lock file.
 *)
let fd_of lock_file : int =
  match SMap.get lock_file !lock_fds with
    | None -> -1
    | Some fd -> Obj.magic fd

(**
 * Check if the file lock is available without grabbing it.
 * Returns true if the lock is free.
 *)
let check lock_file : bool = _operations lock_file Unix.F_TEST
