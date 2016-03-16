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
    |> flowconfig_flags
    |> verbose_flags
    |> strip_root_flag
    |> temp_dir_flag
    |> from_flag
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
      |> flag "--quiet" no_arg
          ~doc:"Suppress info messages to stdout (included in --json)"
      |> dummy None  (* log-file *)
      |> dummy false (* wait *)
      |> common_args
    )
  | Server -> CommandSpec.ArgSpec.(
      empty
      |> dummy Options.default_error_flags (* error_flags *)
      |> dummy false (* json *)
      |> dummy false (* profile *)
      |> dummy false (* quiet *)
      |> dummy None  (* log-file *)
      |> dummy false (* wait *)
      |> common_args
    )
  | Detach -> CommandSpec.ArgSpec.(
      empty
      |> dummy Options.default_error_flags (* error_flags *)
      |> dummy false (* json *)
      |> dummy false (* profile *)
      |> dummy false (* quiet *)
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

  let ignores_of_arg patterns extras =
    let patterns = List.rev_append extras patterns in
    List.map (fun s ->
      (* On Windows, we have to take care about '\'. *)
      let reg = Str.regexp (Str.global_replace (Str.regexp "/") "[/\\]" s) in
      (s, reg)
    ) patterns

  let includes_of_arg root paths =
    List.fold_left (fun acc path ->
      let path = Files_js.make_path_absolute root path in
      Path_matcher.add acc path
    ) Path_matcher.empty paths

  let default_lib_dir tmp_dir =
    let root = Path.make (Tmp.temp_dir tmp_dir "flowlib") in
    if Flowlib.extract_flowlib root
    then root
    else begin
      let msg = "Could not locate flowlib files" in
      FlowExitStatus.(exit ~msg Could_not_find_flowconfig)
    end

  let libs ~root flowconfig extras =
    let config_libs =
      List.map (Files_js.make_path_absolute root) flowconfig.FlowConfig.libs in
    match extras with
    | None -> config_libs
    | Some libs ->
      let libs = libs
      |> Str.split (Str.regexp ",")
      |> List.map Path.make in
      config_libs @ libs

  let result = ref None
  let main
      error_flags
      json
      profile
      quiet
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
      flowconfig_flags
      verbose
      strip_root
      temp_dir
      from
      root
      () =
    FlowEventLogger.set_from from;
    let root = CommandUtils.guess_root root in
    let flowconfig = FlowConfig.get root in
    let opt_module = FlowConfig.(match flowconfig.options.Opts.moduleSystem with
    | Opts.Node -> "node"
    | Opts.Haste -> "haste") in
    let opt_ignores = ignores_of_arg
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
    let opt_temp_dir = Path.to_string (Path.make opt_temp_dir) in
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

    result := Some {
      Options.opt_check_mode = Config.(mode = Check);
      Options.opt_server_mode = Config.(mode = Server);
      Options.opt_error_flags = error_flags;
      Options.opt_log_file = opt_log_file;
      Options.opt_root = root;
      Options.opt_should_detach = Config.(mode = Detach);
      Options.opt_should_wait = wait;
      Options.opt_debug = debug;
      Options.opt_verbose = verbose;
      Options.opt_all = all;
      Options.opt_weak = weak;
      Options.opt_traces;
      Options.opt_json = json;
      Options.opt_quiet = quiet || json;
      Options.opt_module_file_exts = FlowConfig.(
        flowconfig.options.Opts.module_file_exts
      );
      Options.opt_module_name_mappers = FlowConfig.(
        flowconfig.options.Opts.module_name_mappers
      );
      Options.opt_node_resolver_dirnames = FlowConfig.(
        flowconfig.options.Opts.node_resolver_dirnames
      );
      Options.opt_profile = profile;
      Options.opt_strip_root;
      Options.opt_module;
      Options.opt_libs = libs ~root flowconfig lib;
      Options.opt_default_lib_dir;
      Options.opt_munge_underscores = opt_munge_underscores;
      Options.opt_temp_dir;
      Options.opt_max_workers;
      Options.opt_ignores;
      Options.opt_includes;
      Options.opt_suppress_comments = FlowConfig.(
        flowconfig.options.Opts.suppress_comments
      );
      Options.opt_suppress_types = FlowConfig.(
        flowconfig.options.Opts.suppress_types
      );
      Options.opt_enable_const_params = FlowConfig.(
        flowconfig.options.Opts.enable_const_params
      );
      Options.opt_enable_unsafe_getters_and_setters = FlowConfig.(
        flowconfig.options.Opts.enable_unsafe_getters_and_setters
      );
      Options.opt_esproposal_decorators = FlowConfig.(
        flowconfig.options.Opts.esproposal_decorators
      );
      Options.opt_esproposal_export_star_as = FlowConfig.(
        flowconfig.options.Opts.esproposal_export_star_as
      );
      Options.opt_ignore_fbt = FlowConfig.(
        flowconfig.options.Opts.facebook_ignore_fbt
      );
      Options.opt_ignore_non_literal_requires = FlowConfig.(
        flowconfig.options.Opts.ignore_non_literal_requires
      );
      Options.opt_esproposal_class_static_fields = FlowConfig.(
        flowconfig.options.Opts.esproposal_class_static_fields
      );
      Options.opt_esproposal_class_instance_fields = FlowConfig.(
        flowconfig.options.Opts.esproposal_class_instance_fields
      );
    };
    ()

  let rec parse () =
    match !result with
    | Some result -> result
    | None ->
        let argv = Array.to_list Sys.argv in
        CommandSpec.main spec main argv;
        parse ()
end

module Main (OptionParser : Server.OPTION_PARSER) =
  ServerFunctors.ServerMain (Server.FlowProgram (OptionParser))

module Check = struct
  module OptionParser = OptionParser (struct let mode = Check end)
  module Main = Main (OptionParser)
  let spec = OptionParser.spec
  (* ignores argv because OptionParser grabs it directly *)
  let command = CommandSpec.raw_command spec (fun _ -> Main.start ())
end

module Server = struct
  module OptionParser = OptionParser (struct let mode = Server end)
  module Main = Main (OptionParser)
  let spec = OptionParser.spec
  (* ignores argv because OptionParser grabs it directly *)
  let command = CommandSpec.raw_command spec (fun _ -> Main.start ())
end

module Start = struct
  module OptionParser = OptionParser (struct let mode = Detach end)
  module Main = Main (OptionParser)
  let spec = OptionParser.spec
  (* ignores argv because OptionParser grabs it directly *)
  let command = CommandSpec.raw_command spec (fun _ -> Main.start ())
end
