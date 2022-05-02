(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

let init ?log_pid:_ ?init_id:_ _ _ = ()

let disable_logging () = ()

let log _ = ()

let logger_pid () = None

let should_log () = false

let flush _ = ()
