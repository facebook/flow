(**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* As for [Daemon.register_entry_point], this should stay
   at toplevel, in order to be executed before
   [Daemon.check_entry_point]. *)
let entry = Worker.register_entry_point ~restore:(fun (logger_level, profile_id) ->
  Hh_logger.Level.set_min_level logger_level;
  Flow_server_profile.init_from_id profile_id
)

(* Saves the default GC settings, which are restored by the workers. Workers can
 * have more relaxed GC configs as they are short-lived processes, and this
 * prevents the workers from inheriting GC settings the master needs. *)
let gc_control = Gc.get ()

let make ~n heap_handle =
  Worker.make
    ?call_wrapper:None
    ~saved_state: (
      Hh_logger.Level.min_level (),
      Flow_server_profile.get_id ()
    )
    ~entry
    ~nbr_procs: n
    ~gc_control
    ~heap_handle
