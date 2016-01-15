(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type options = {
  opt_all : bool;
  opt_check_mode: bool;
  opt_debug : bool;
  opt_error_flags: Errors_js.flags;
  opt_json : bool;
  opt_libs: Path.t list;
  opt_log_file: Path.t;
  opt_max_workers: int;
  opt_module: string;
  opt_module_name_mappers: (Str.regexp * string) list;
  opt_munge_underscores: bool;
  opt_no_flowlib: bool;
  opt_profile : bool;
  opt_quiet : bool;
  opt_root : Path.t;
  opt_server_mode: bool;
  opt_should_detach : bool;
  opt_should_wait : bool;
  opt_strip_root : bool;
  opt_temp_dir: string;
  opt_traces : int;
  opt_verbose : int option; (* num of spaces to indent; None for not verbose *)
  opt_weak : bool;
}

let all opts = opts.opt_all
let error_flags opts = opts.opt_error_flags
let is_check_mode opts = opts.opt_check_mode
let is_debug_mode opts = opts.opt_debug
let is_server_mode opts = opts.opt_server_mode
let log_file opts = opts.opt_log_file
let max_workers opts = opts.opt_max_workers
let root opts = opts.opt_root
let should_detach opts = opts.opt_should_detach
let should_munge_underscores opts = opts.opt_munge_underscores
let should_strip_root opts = opts.opt_strip_root
let should_wait opts = opts.opt_should_wait
let temp_dir opts = opts.opt_temp_dir
let verbose opts = opts.opt_verbose
let weak_by_default opts = opts.opt_weak
