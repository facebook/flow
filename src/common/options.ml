(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type error_flags = {
  color: Tty.color_mode;
  one_line: bool;
  show_all_errors: bool;
  old_output_format: bool;
}

type esproposal_feature_mode =
  | ESPROPOSAL_ENABLE
  | ESPROPOSAL_IGNORE
  | ESPROPOSAL_WARN

type options = {
  opt_all : bool;
  opt_check_mode: bool;
  opt_debug : bool;
  opt_default_lib_dir: Path.t option;
  opt_enable_const_params: bool;
  opt_enable_unsafe_getters_and_setters: bool;
  opt_error_flags: error_flags;
  opt_esproposal_class_static_fields: esproposal_feature_mode;
  opt_esproposal_class_instance_fields: esproposal_feature_mode;
  opt_esproposal_decorators: esproposal_feature_mode;
  opt_esproposal_export_star_as: esproposal_feature_mode;
  opt_ignore_fbt: bool;
  opt_ignore_non_literal_requires: bool;
  opt_ignores: (string * Str.regexp) list;
  opt_includes: Path_matcher.t;
  opt_json : bool;
  opt_libs: Path.t list;
  opt_log_file: Path.t;
  opt_max_workers: int;
  opt_module: string;
  opt_module_file_exts: SSet.t;
  opt_module_name_mappers: (Str.regexp * string) list;
  opt_modules_are_use_strict: bool;
  opt_munge_underscores: bool;
  opt_node_resolver_dirnames: string list;
  opt_profile : bool;
  opt_quiet : bool;
  opt_root : Path.t;
  opt_server_mode: bool;
  opt_should_detach : bool;
  opt_should_wait : bool;
  opt_strip_root : bool;
  opt_suppress_comments : Str.regexp list;
  opt_suppress_types : SSet.t;
  opt_temp_dir: string;
  opt_traces : int;
  opt_verbose : int option; (* num of spaces to indent; None for not verbose *)
  opt_weak : bool;
}

let default_error_flags = {
  color = Tty.Color_Auto;
  one_line = false;
  show_all_errors = false;
  old_output_format = false;
}

let all opts = opts.opt_all
let default_lib_dir opts = opts.opt_default_lib_dir
let enable_const_params opts = opts.opt_enable_const_params
let enable_unsafe_getters_and_setters opts =
  opts.opt_enable_unsafe_getters_and_setters
let error_flags opts = opts.opt_error_flags
let esproposal_class_static_fields opts =
  opts.opt_esproposal_class_static_fields
let esproposal_class_instance_fields opts =
  opts.opt_esproposal_class_instance_fields
let esproposal_decorators opts = opts.opt_esproposal_decorators
let esproposal_export_star_as opts = opts.opt_esproposal_export_star_as
let ignores opts = opts.opt_ignores
let includes opts = opts.opt_includes
let is_check_mode opts = opts.opt_check_mode
let is_debug_mode opts = opts.opt_debug
let is_server_mode opts = opts.opt_server_mode
let lib_paths opts = opts.opt_libs
let log_file opts = opts.opt_log_file
let max_trace_depth opts = opts.opt_traces
let max_workers opts = opts.opt_max_workers
let module_file_exts opts = opts.opt_module_file_exts
let module_name_mappers opts = opts.opt_module_name_mappers
let modules_are_use_strict opts = opts.opt_modules_are_use_strict
let node_resolver_dirnames opts = opts.opt_node_resolver_dirnames
let root opts = opts.opt_root
let should_detach opts = opts.opt_should_detach
let should_ignore_fbt opts = opts.opt_ignore_fbt
let should_ignore_non_literal_requires opts =
  opts.opt_ignore_non_literal_requires
let should_munge_underscores opts = opts.opt_munge_underscores
let should_profile opts = opts.opt_profile && not opts.opt_quiet
let should_strip_root opts = opts.opt_strip_root
let should_wait opts = opts.opt_should_wait
let suppress_comments opts = opts.opt_suppress_comments
let suppress_types opts = opts.opt_suppress_types
let temp_dir opts = opts.opt_temp_dir
let verbose opts = opts.opt_verbose
let weak_by_default opts = opts.opt_weak
