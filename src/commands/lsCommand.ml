(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(***********************************************************************)
(* flow ls (list files) command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec = {
  CommandSpec.
  name = "ls";
  doc = "Lists files visible to Flow";
  usage = Printf.sprintf
    "Usage: %s ls [OPTION]... [FILE]...\n\n\
      Lists files visible to Flow\n"
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> strip_root_flag
    |> ignore_flag
    |> include_flag
    |> anon "root" (optional string) ~doc:"Root directory"
  )
}

let rec print_all_rec ~normalize_filename next =
  match next () with
  | [] -> ()
  | result ->

    List.iter (fun filename ->
      Printf.printf "%s\n%!" (normalize_filename filename)
    ) result;
    print_all_rec ~normalize_filename next

let main strip_root ignore_flag include_flag root () =
  let root = guess_root root in
  let flowconfig = FlowConfig.get (Server_files_js.config_file root) in

  let opt_temp_dir =
    FlowConfig.(flowconfig.options.Opts.temp_dir)
    |> Path.make
    |> Path.to_string
  in

  let opt_shm_dir =
    FlowConfig.(flowconfig.options.Opts.shm_dir)
    |> Path.make
    |> Path.to_string
  in

  let opt_log_file = Server_files_js.log_file
    ~tmp_dir:opt_temp_dir
    root
    flowconfig.FlowConfig.options
  in

  let opt_strip_root = strip_root ||
    FlowConfig.(flowconfig.options.Opts.strip_root)
  in

  let opt_module = FlowConfig.(match flowconfig.options.Opts.moduleSystem with
  | Opts.Node -> "node"
  | Opts.Haste -> "haste") in

  let opt_ignores = ignores_of_arg
    root
    flowconfig.FlowConfig.ignores
    (list_of_string_arg ignore_flag) in

  let opt_includes =
    let includes = List.rev_append
      flowconfig.FlowConfig.includes
      (list_of_string_arg include_flag) in
    includes_of_arg root includes in

  let options = { Options.
    opt_check_mode = false;
    opt_server_mode = false;
    opt_error_flags = Options.default_error_flags;
    opt_log_file = opt_log_file;
    opt_root = root;
    opt_should_detach = false;
    opt_should_wait = false;
    opt_debug = false;
    opt_verbose = None;
    opt_all = false;
    opt_weak = false;
    opt_traces = 0;
    opt_json = false;
    opt_quiet = false;
    opt_module_file_exts = FlowConfig.(
      flowconfig.options.Opts.module_file_exts
    );
    opt_module_name_mappers = FlowConfig.(
      flowconfig.options.Opts.module_name_mappers
    );
    opt_modules_are_use_strict = FlowConfig.(
      flowconfig.options.Opts.modules_are_use_strict
    );
    opt_node_resolver_dirnames = FlowConfig.(
      flowconfig.options.Opts.node_resolver_dirnames
    );
    opt_profile = false;
    opt_strip_root;
    opt_module;
    opt_libs = [];
    opt_default_lib_dir = None;
    opt_munge_underscores = false;
    opt_temp_dir;
    opt_shm_dir;
    opt_max_workers = 1;
    opt_ignores;
    opt_includes;
    opt_suppress_comments = [];
    opt_suppress_types = SSet.empty;
    opt_enable_const_params = false;
    opt_enable_unsafe_getters_and_setters = false;
    opt_enforce_strict_type_args = false;
    opt_esproposal_class_static_fields = Options.ESPROPOSAL_WARN;
    opt_esproposal_class_instance_fields = Options.ESPROPOSAL_WARN;
    opt_esproposal_decorators = Options.ESPROPOSAL_WARN;
    opt_esproposal_export_star_as = Options.ESPROPOSAL_WARN;
    opt_ignore_fbt = false;
    opt_ignore_non_literal_requires = false;
  } in

  let _, libs = Files_js.init options in

  let root_str = spf "%s%s" (Path.to_string root) Filename.dir_sep in
  let normalize_filename filename =
    if not opt_strip_root then filename
    else Files_js.relative_path root_str filename
  in

  print_all_rec ~normalize_filename (Files_js.make_next_files ~options ~libs)

let command = CommandSpec.command spec main
