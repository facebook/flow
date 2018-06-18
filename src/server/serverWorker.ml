(**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* As for [Daemon.register_entry_point], this should stay
   at toplevel, in order to be executed before
   [Daemon.check_entry_point]. *)
let entry =
  WorkerController.register_entry_point ~restore:(fun (logger_level, log_filename, profile_id) ->
    Hh_logger.Level.set_min_level logger_level;
    Flow_server_profile.init_from_id profile_id;

    match log_filename with
    | None -> ()
    | Some file ->
      let log_fd = Unix.openfile file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o666 in
      Hh_logger.set_log file (Unix.out_channel_of_descr log_fd)
  )

(* Saves the default GC settings, which are restored by the workers. Workers can
 * have more relaxed GC configs as they are short-lived processes, and this
 * prevents the workers from inheriting GC settings the master needs. *)
let gc_control = Gc.get ()

let make ~n heap_handle =
  MultiWorkerLwt.make
    ?call_wrapper:None
    ~saved_state: (
      Hh_logger.Level.min_level (),
      Hh_logger.get_log_name (),
      Flow_server_profile.get_id ()
    )
    ~entry
    ~nbr_procs: n
    ~gc_control
    ~heap_handle
