(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type init_settings = {
  scuba_table_name: string;
  (* File descriptors for the logger daemon's stdout and stderr. *)
  log_out: Unix.file_descr;
  log_err: Unix.file_descr;
}

type init_mode =
  (* Sends everything to /dev/null. *)
  | Event_logger_fake
  | Event_logger_real of init_settings

let init ?exit_on_parent_exit:_ ?log_pid:_ ?init_id:_ _ _ = ()

let disable_logging _ = ()

let log _ = ()

let logger_pid () = None

let set_init_type _ = ()

let log_if_initialized _ = ()

let master_exception _ ~stack:_ = ()

let worker_exception _ = ()

let sharedmem_gc_ran _ _ _ _ = ()

let sharedmem_init_done _ = ()

let sharedmem_failed_anonymous_memfd_init _ = ()

let sharedmem_failed_to_use_shm_dir ~shm_dir:_ ~reason:_ = ()

let sharedmem_less_than_minimum_available ~shm_dir:_ ~shm_min_avail:_ ~avail:_
    =
  ()

let sharedmem_access_sample ~heap_name:_ ~key:_ ~write_time:_ = ()

let find_done ~time_taken:_ ~name:_ = ()

let log_gc_stats () = ()

let flush _ = ()

let watchman_error _ = ()

let watchman_warning _ = ()

let watchman_died_caught _ = ()

let watchman_uncaught_failure _ = ()

let watchman_connection_reestablished _ = ()

let watchman_connection_reestablishment_failed _ = ()

let watchman_timeout _ = ()

let dfind_ready _ _ = ()
