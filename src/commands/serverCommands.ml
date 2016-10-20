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
(* server commands *)
(***********************************************************************)

open CommandUtils

type mode = Check | Server | Detach

module type CONFIG = sig
  val mode : mode
end

module Main = ServerFunctors.ServerMain (Server.FlowProgram)

module OptionParser(Config : CONFIG) = struct
  let cmdname = match Config.mode with
  | Check -> "check"
  | Server -> "server"
  | Detach -> "start"

  let cmddoc = match Config.mode with
  | Check -> "Does a full Flow check and prints the results"
  | Server -> "Runs a Flow server in the foreground"
  | Detach -> "Starts a Flow server"

  let common_args prev = CommandSpec.ArgSpec.(
    prev
    |> flag "--debug" no_arg
        ~doc:"Print debug info during typecheck"
    |> flag "--all" no_arg
        ~doc:"Typecheck all files, not just @flow"
    |> flag "--weak" no_arg
        ~doc:"Typecheck with weak inference, assuming dynamic types by default"
    |> flag "--traces" (optional int)
        ~doc:"Outline an error path up to a specified level"
    |> flag "--lib" (optional string)
        ~doc:"Specify one or more library paths, comma separated"
    |> flag "--no-flowlib" no_arg
        ~doc:"Do not include embedded declarations"
    |> flag "--munge-underscore-members" no_arg
        ~doc:"Treat any class member name with a leading underscore as private"
    |> flag "--max-workers" (optional int)
        ~doc:"Maximum number of workers to create (capped by number of cores)"
    |> ignore_version_flag
    |> flowconfig_flags
    |> verbose_flags
    |> strip_root_flag
    |> temp_dir_flag
    |> shm_dirs_flag
    |> shm_min_avail_flag
    |> shm_dep_table_pow_flag
    |> shm_hash_table_pow_flag
    |> shm_log_level_flag
    |> from_flag
    |> quiet_flag
    |> anon "root" (optional string) ~doc:"Root directory"
  )

  let args = match Config.mode with
  | Check -> CommandSpec.ArgSpec.(
      empty
      |> error_flags
      |> flag "--json" no_arg
          ~doc:"Output errors in JSON format"
      |> flag "--profile" no_arg
          ~doc:"Output profiling information"
      |> dummy None  (* log-file *)
      |> dummy false (* wait *)
      |> common_args
    )
  | Server -> CommandSpec.ArgSpec.(
      empty
      |> dummy Options.default_error_flags (* error_flags *)
      |> dummy false (* json *)
      |> dummy false (* profile *)
      |> dummy None  (* log-file *)
      |> dummy false (* wait *)
      |> common_args
    )
  | Detach -> CommandSpec.ArgSpec.(
      empty
      |> dummy Options.default_error_flags (* error_flags *)
      |> flag "--json" no_arg
          ~doc:"Respond in JSON format"
      |> dummy false (* profile *)
      |> flag "--log-file" string
          ~doc:"Path to log file (default: /tmp/flow/<escaped root path>.log)"
      |> flag "--wait" no_arg
          ~doc:"Wait for the server to finish initializing"
      |> common_args
    )

  let spec = {
    CommandSpec.
    name = cmdname;
    doc = cmddoc;
    args;
    usage = Printf.sprintf
      "Usage: %s %s [OPTION]... [ROOT]\n\
        %s\n\n\
        Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
        ROOT is assumed to be the current directory if unspecified.\n\
        A server will be started if none is running over ROOT.\n"
        exe_name cmdname cmddoc;
  }

  let default_lib_dir tmp_dir =
    let root = Path.make (Tmp.temp_dir tmp_dir "flowlib") in
    if Flowlib.extract_flowlib root
    then root
    else begin
      let msg = "Could not locate flowlib files" in
      FlowExitStatus.(exit ~msg Could_not_find_flowconfig)
    end

  let libs ~root flowconfig extras =
    let flowtyped_path = Files.get_flowtyped_path root in
    let has_explicit_flowtyped_lib = ref false in
    let config_libs =
      List.fold_right (fun lib abs_libs ->
        let abs_lib = Files.make_path_absolute root lib in
        (**
         * "flow-typed" is always included in the libs list for convenience,
         * but there's no guarantee that it exists on the filesystem.
         *)
        if abs_lib = flowtyped_path then has_explicit_flowtyped_lib := true;
        abs_lib::abs_libs
      ) flowconfig.FlowConfig.libs []
    in
    let config_libs =
      if !has_explicit_flowtyped_lib = false
         && (Sys.file_exists (Path.to_string flowtyped_path))
      then flowtyped_path::config_libs
      else config_libs
    in
    match extras with
    | None -> config_libs
    | Some libs ->
      let libs = libs
      |> Str.split (Str.regexp ",")
      |> List.map Path.make in
      config_libs @ libs

  let assert_version version_constraint =
    if not (Semver.satisfies version_constraint FlowConfig.version)
    then
      let msg = Utils_js.spf
        "Wrong version of Flow. The config specifies version %s but this is version %s"
        version_constraint
        FlowConfig.version
      in
      FlowExitStatus.(exit ~msg Invalid_flowconfig)

  let main
      error_flags
      json
      profile
      log_file
      wait
      debug
      all
      weak
      traces
      lib
      no_flowlib
      munge_underscore_members
      max_workers
      ignore_version
      flowconfig_flags
      verbose
      strip_root
      temp_dir
      shm_dirs
      shm_min_avail
      shm_dep_table_pow
      shm_hash_table_pow
      shm_log_level
      from
      quiet
      root
      () =

    FlowEventLogger.set_from from;
    let root = CommandUtils.guess_root root in
    let flowconfig = FlowConfig.get (Server_files_js.config_file root) in

    begin match ignore_version, FlowConfig.(flowconfig.options.Opts.version) with
    | false, Some version -> assert_version version
    | _ -> ()
    end;

    let opt_module = FlowConfig.(match flowconfig.options.Opts.moduleSystem with
    | Opts.Node -> "node"
    | Opts.Haste -> "haste") in
    let opt_ignores = ignores_of_arg
      root
      flowconfig.FlowConfig.ignores
      flowconfig_flags.ignores in
    let opt_includes =
      let includes = List.rev_append
        flowconfig.FlowConfig.includes
        flowconfig_flags.includes in
      includes_of_arg root includes in
    let opt_traces = match traces with
      | Some level -> level
      | None -> FlowConfig.(flowconfig.options.Opts.traces) in
    let opt_strip_root = strip_root ||
      FlowConfig.(flowconfig.options.Opts.strip_root) in
    let opt_munge_underscores = munge_underscore_members ||
      FlowConfig.(flowconfig.options.Opts.munge_underscores) in
    let opt_temp_dir = match temp_dir with
    | Some x -> x
    | None -> FlowConfig.(flowconfig.options.Opts.temp_dir)
    in
    let opt_shm_dirs = Option.value_map
      shm_dirs
      ~default:FlowConfig.(flowconfig.options.Opts.shm_dirs)
      ~f:(Str.split (Str.regexp ",")) in
    let opt_shm_min_avail = Option.value
      shm_min_avail
      ~default:FlowConfig.(flowconfig.options.Opts.shm_min_avail) in
    let opt_temp_dir = Path.to_string (Path.make opt_temp_dir) in
    let opt_shm_dirs =
      List.map Path.(fun dir -> dir |> make |> to_string) opt_shm_dirs in
    let opt_shm_dep_table_pow = Option.value
      shm_dep_table_pow
      ~default:FlowConfig.(flowconfig.options.Opts.shm_dep_table_pow) in
    let opt_shm_hash_table_pow = Option.value
      shm_hash_table_pow
      ~default:FlowConfig.(flowconfig.options.Opts.shm_hash_table_pow) in
    let opt_shm_log_level = Option.value
      shm_log_level
      ~default:FlowConfig.(flowconfig.options.Opts.shm_log_level) in
    let opt_default_lib_dir =
      if no_flowlib then None else Some (default_lib_dir opt_temp_dir) in
    let opt_log_file = match log_file with
      | Some s ->
          let dirname = Path.make (Filename.dirname s) in
          let basename = Filename.basename s in
          Path.concat dirname basename
      | None ->
          Server_files_js.log_file
            ~tmp_dir:opt_temp_dir
            root
            flowconfig.FlowConfig.options
    in
    let opt_max_workers = match max_workers with
    | Some x -> x
    | None -> FlowConfig.(flowconfig.options.Opts.max_workers)
    in
    let all = all || FlowConfig.(flowconfig.options.Opts.all) in
    let opt_max_workers = min opt_max_workers Sys_utils.nbr_procs in

    let options = { Options.
      opt_check_mode = Config.(mode = Check);
      opt_server_mode = Config.(mode = Server);
      opt_error_flags = error_flags;
      opt_log_file = opt_log_file;
      opt_root = root;
      opt_should_detach = Config.(mode = Detach);
      opt_should_wait = wait;
      opt_debug = debug;
      opt_verbose = verbose;
      opt_all = all;
      opt_weak = weak;
      opt_traces;
      opt_json = json;
      opt_quiet = quiet || json;
      opt_module_file_exts = FlowConfig.(
        flowconfig.options.Opts.module_file_exts
      );
      opt_module_resource_exts = FlowConfig.(
        flowconfig.options.Opts.module_resource_exts
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
      opt_output_graphml = false;
      opt_profile = profile;
      opt_strip_root;
      opt_module;
      opt_libs = libs ~root flowconfig lib;
      opt_default_lib_dir;
      opt_munge_underscores = opt_munge_underscores;
      opt_temp_dir;
      opt_shm_dirs;
      opt_shm_min_avail;
      opt_shm_dep_table_pow;
      opt_shm_hash_table_pow;
      opt_shm_log_level;
      opt_shm_global_size = FlowConfig.(
        flowconfig.options.Opts.shm_global_size
      );
      opt_shm_heap_size = FlowConfig.(
        flowconfig.options.Opts.shm_heap_size
      );
      opt_max_workers;
      opt_ignores;
      opt_includes;
      opt_suppress_comments = FlowConfig.(
        flowconfig.options.Opts.suppress_comments
      );
      opt_suppress_types = FlowConfig.(
        flowconfig.options.Opts.suppress_types
      );
      opt_enable_const_params = FlowConfig.(
        flowconfig.options.Opts.enable_const_params
      );
      opt_enforce_strict_type_args = FlowConfig.(
        flowconfig.options.Opts.enforce_strict_type_args
      );
      opt_enable_unsafe_getters_and_setters = FlowConfig.(
        flowconfig.options.Opts.enable_unsafe_getters_and_setters
      );
      opt_esproposal_decorators = FlowConfig.(
        flowconfig.options.Opts.esproposal_decorators
      );
      opt_esproposal_export_star_as = FlowConfig.(
        flowconfig.options.Opts.esproposal_export_star_as
      );
      opt_facebook_fbt = FlowConfig.(
        flowconfig.options.Opts.facebook_fbt
      );
      opt_ignore_non_literal_requires = FlowConfig.(
        flowconfig.options.Opts.ignore_non_literal_requires
      );
      opt_esproposal_class_static_fields = FlowConfig.(
        flowconfig.options.Opts.esproposal_class_static_fields
      );
      opt_esproposal_class_instance_fields = FlowConfig.(
        flowconfig.options.Opts.esproposal_class_instance_fields
      );
      opt_max_header_tokens = FlowConfig.(
        flowconfig.options.Opts.max_header_tokens
      )
    } in
    Main.start options

  let command = CommandSpec.command spec main
end

module CheckCommand = OptionParser (struct let mode = Check end)
module ServerCommand = OptionParser (struct let mode = Server end)
module StartCommand = OptionParser (struct let mode = Detach end)
