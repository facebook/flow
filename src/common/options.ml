(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type esproposal_feature_mode =
  | ESPROPOSAL_ENABLE
  | ESPROPOSAL_IGNORE
  | ESPROPOSAL_WARN

type module_system =
  | Node
  | Haste

type lazy_mode =
| LAZY_MODE_FILESYSTEM
| LAZY_MODE_IDE

type jsx_mode =
  (**
   * Specifies a function that should be invoked instead of React.createElement
   * when interpreting JSX syntax. Otherwise, the usual rules of JSX are
   * followed: children are varargs after a props argument.
   *)
  | JSXPragma of (string * Loc.t Ast.Expression.t)
  (**
   * Alternate mode for interpreting JSX syntax. The element name is treated
   * as a function to be directly invoked, e.g. <Foo /> -> Foo({}).
   * Children are part of props instead of a separate argument.
   *)
  | CSX

type file_watcher =
| NoFileWatcher
| DFind

type t = {
  opt_all : bool;
  opt_debug : bool;
  opt_enable_const_params: bool;
  opt_enforce_strict_call_arity: bool;
  opt_esproposal_class_static_fields: esproposal_feature_mode;
  opt_esproposal_class_instance_fields: esproposal_feature_mode;
  opt_esproposal_decorators: esproposal_feature_mode;
  opt_esproposal_export_star_as: esproposal_feature_mode;
  opt_facebook_fbt: string option;
  opt_file_options: Files.options;
  opt_haste_name_reducers: (Str.regexp * string) list;
  opt_haste_paths_blacklist: string list;
  opt_haste_paths_whitelist: string list;
  opt_haste_use_name_reducers: bool;
  opt_ignore_non_literal_requires: bool;
  opt_include_warnings: bool;
  opt_max_workers: int;
  opt_merge_timeout: float option;
  opt_module: module_system;
  opt_module_name_mappers: (Str.regexp * string) list;
  opt_modules_are_use_strict: bool;
  opt_munge_underscores: bool;
  opt_profile : bool;
  opt_lazy_mode: lazy_mode option;
  opt_quiet : bool;
  opt_root : Path.t;
  opt_strip_root : bool;
  opt_suppress_comments : Str.regexp list;
  opt_suppress_types : SSet.t;
  opt_temp_dir: string;
  opt_traces : int;
  opt_file_watcher: file_watcher;
  opt_verbose : Verbose.t option;
  opt_weak : bool;
  opt_max_header_tokens: int;
  opt_lint_severities: Severity.severity LintSettings.t;
  opt_strict_mode: StrictModeSettings.t;
}

let all opts = opts.opt_all
let enable_const_params opts = opts.opt_enable_const_params
let enforce_strict_call_arity opts = opts.opt_enforce_strict_call_arity
let esproposal_class_static_fields opts =
  opts.opt_esproposal_class_static_fields
let esproposal_class_instance_fields opts =
  opts.opt_esproposal_class_instance_fields
let esproposal_decorators opts = opts.opt_esproposal_decorators
let esproposal_export_star_as opts = opts.opt_esproposal_export_star_as
let haste_name_reducers opts = opts.opt_haste_name_reducers
let haste_paths_blacklist opts = opts.opt_haste_paths_blacklist
let haste_paths_whitelist opts = opts.opt_haste_paths_whitelist
let haste_use_name_reducers opts = opts.opt_haste_use_name_reducers
let file_options opts = opts.opt_file_options
let is_debug_mode opts = opts.opt_debug
let is_lazy_mode opts = opts.opt_lazy_mode <> None
let lazy_mode opts = opts.opt_lazy_mode
let is_quiet opts = opts.opt_quiet
let max_header_tokens opts = opts.opt_max_header_tokens
let max_trace_depth opts = opts.opt_traces
let max_workers opts = opts.opt_max_workers
let merge_timeout opts = opts.opt_merge_timeout
let module_name_mappers opts = opts.opt_module_name_mappers
let module_system opts = opts.opt_module
let modules_are_use_strict opts = opts.opt_modules_are_use_strict
let root opts = opts.opt_root
let facebook_fbt opts = opts.opt_facebook_fbt
let should_ignore_non_literal_requires opts =
  opts.opt_ignore_non_literal_requires
let should_include_warnings opts = opts.opt_include_warnings
let should_munge_underscores opts = opts.opt_munge_underscores
let should_profile opts = opts.opt_profile && not opts.opt_quiet
let should_strip_root opts = opts.opt_strip_root
let suppress_comments opts = opts.opt_suppress_comments
let suppress_types opts = opts.opt_suppress_types
let temp_dir opts = opts.opt_temp_dir
let use_file_watcher opts = opts.opt_file_watcher <> NoFileWatcher
let verbose opts = opts.opt_verbose
let weak_by_default opts = opts.opt_weak

let lint_severities opts = opts.opt_lint_severities
let strict_mode opts = opts.opt_strict_mode
