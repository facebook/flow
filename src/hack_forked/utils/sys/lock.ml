(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let lock_fds = ref SMap.empty

(**
 * Basic lock operations.
 *
 * We use these for two reasons:
 * 1. making sure we are only running one instance of hh_server per person on a given dev box
 * 2. giving a way to hh_client to check if a server is running.
 *)

let register_lock lock_file =
  let _ = Sys_utils.mkdir_no_fail (Filename.dirname lock_file) in
  Sys_utils.with_umask 0o111 (fun () ->
      let fd = Unix.descr_of_out_channel (open_out lock_file) in
      let st = Unix.fstat fd in
      lock_fds := SMap.add lock_file (fd, st) !lock_fds;
      fd)

(**
 * Grab or check if a file lock is available.
 *
 * Returns true if the lock is/was available, false otherwise.
 *)
let _operations lock_file op : bool =
  try
    let fd =
      match SMap.find_opt lock_file !lock_fds with
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
            Unix.(st.st_dev = current_st.st_dev && st.st_ino = current_st.st_ino)
          with
          | _ -> false
        in
        if not (Sys.win32 || identical_file) then
          (* Looks like someone (tmpwatch?) deleted the lock file; don't
           * create another one, because our socket is probably gone too.
           * We are dead in the water. *)
          raise Exit
        else
          fd
    in
    let _ =
      try Unix.lockf fd op 1 with
      | _ when Sys.win32 && (op = Unix.F_TLOCK || op = Unix.F_TEST) ->
        (* On Windows, F_TLOCK and F_TEST fail if we have the lock ourself *)
        (* However, we then are the only one to be able to write there. *)
        ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);

        (* If we don't have the lock, the following 'write' will
           throw an exception. *)
        let wb = Unix.write fd (Bytes.make 1 ' ') 0 1 in
        (* When not throwing an exception, the current
           implementation of `Unix.write` always return `1`. But let's
           be protective against semantic changes, and better fails
           than wrongly assume that we own a lock. *)
        assert (wb = 1)
    in
    true
  with
  | _ -> false

(**
 * Grabs the file lock and returns true if it the lock was grabbed
 *)
let grab lock_file : bool = _operations lock_file Unix.F_TLOCK

(**
 * Releases a file lock.
 *)
let release lock_file : bool = _operations lock_file Unix.F_ULOCK

let blocking_grab_then_release lock_file =
  ignore (_operations lock_file Unix.F_LOCK);
  ignore (release lock_file)

(**
 * Gets the server instance-unique integral fd for a given lock file.
 *)
let fd_of lock_file : int =
  match SMap.find_opt lock_file !lock_fds with
  | None -> -1
  | Some fd -> Obj.magic fd

(**
 * Check if the file lock is available without grabbing it.
 * Returns true if the lock is free.
 *)
let check lock_file : bool = _operations lock_file Unix.F_TEST
