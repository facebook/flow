(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

let init_fake () = ()

let disable_logging _ = ()

let log _ = ()

let logger_pid () = None

let set_init_type _ = ()

let should_log () = false

let worker_exception _ = ()

let sharedmem_gc_ran _ _ _ _ = ()

let sharedmem_init_done _ = ()

let sharedmem_failed_memfd_init _ = ()

let flush _ = ()

let dfind_ready _ _ = ()
