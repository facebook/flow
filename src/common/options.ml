(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type options = {
  opt_check_mode: bool;
  opt_error_flags: Errors_js.flags;
  opt_root : Path.t;
  opt_should_detach : bool;
  opt_debug : bool;
  opt_verbose : bool;
  opt_verbose_indent: bool;
  opt_all : bool;
  opt_weak : bool;
  opt_traces : int;
  opt_strict : bool;
  opt_json : bool;
  opt_quiet : bool;
  opt_profile : bool;
  opt_strip_root : bool;
  opt_module: string;
  opt_libs: Path.t list;
  opt_log_file: Path.t;
  opt_no_flowlib: bool;
  opt_module_name_mappers: (Str.regexp * string) list;
}

let error_flags opts = opts.opt_error_flags
let is_check_mode opts = opts.opt_check_mode
let log_file opts = opts.opt_log_file
let root opts = opts.opt_root
let should_detach opts = opts.opt_should_detach
let should_indent_verbose opts = opts.opt_verbose_indent
