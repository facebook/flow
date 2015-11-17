(**
 * Copyright (c) 2014, Facebook, Inc.
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

type mode = Check | Normal | Detach

module type CONFIG = sig
  val mode : mode
end

module OptionParser(Config : CONFIG) = struct
  let cmdname = match Config.mode with
  | Check -> "check"
  | Normal -> "server"
  | Detach -> "start"

  let cmddoc = match Config.mode with
  | Check -> "Does a full Flow check and prints the results"
  | Normal -> "Runs a Flow server in the foreground"
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
    |> verbose_flags
    |> strip_root_flag
    |> temp_dir_flag
    |> shm_dir_flag
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
  | Normal -> CommandSpec.ArgSpec.(
      empty
      |> dummy Errors_js.default_flags (* error_flags *)
      |> dummy false (* json *)
      |> dummy false (* profile *)
      |> dummy false (* quiet *)
      |> dummy None  (* log-file *)
      |> dummy false (* wait *)
      |> common_args
    )
  | Detach -> CommandSpec.ArgSpec.(
      empty
      |> dummy Errors_js.default_flags (* error_flags *)
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

  let result = ref None
  let main error_flags json profile quiet log_file wait debug
           all weak traces lib no_flowlib munge_underscore_members max_workers
           verbose strip_root temp_dir shm_dir from
           root () =
    FlowEventLogger.set_from from;
    let root = CommandUtils.guess_root root in
    let flowconfig = FlowConfig.get root in
    let opt_module = FlowConfig.(match flowconfig.options.Opts.moduleSystem with
    | Opts.Node -> "node"
    | Opts.Haste -> "haste") in
    let opt_libs = FlowConfig.(match lib with
    | None -> flowconfig.libs
    | Some libs ->
        let libs = libs
        |> Str.split (Str.regexp ",")
        |> List.map Path.make in
        flowconfig.libs @ libs
    ) in
    let opt_traces = match traces with
      | Some level -> level
      | None -> FlowConfig.(flowconfig.options.Opts.traces) in
    let opt_strip_root = strip_root ||
      FlowConfig.(flowconfig.options.Opts.strip_root) in
    let opt_munge_underscores = munge_underscore_members ||
      FlowConfig.(flowconfig.options.Opts.munge_underscores) in
    let opt_temp_dir = match temp_dir with
    | Some x -> x
    | None -> Path.to_string (FlowConfig.(flowconfig.options.Opts.temp_dir))
    in
    let opt_shm_dir = match shm_dir with
    | Some x -> x
    | None -> Path.to_string (FlowConfig.(flowconfig.options.Opts.shm_dir))
    in
    let opt_log_file = match log_file with
      | Some s ->
          let dirname = Path.make (Filename.dirname s) in
          let basename = Filename.basename s in
          Path.concat dirname basename
      | None -> FlowConfig.(
          log_file ~tmp_dir:opt_temp_dir root flowconfig.options
        )
    in
    let opt_max_workers = match max_workers with
    | Some x -> x
    | None -> FlowConfig.(flowconfig.options.Opts.max_workers)
    in
    let opt_max_workers = min opt_max_workers Sys_utils.nbr_procs in

    result := Some {
      Options.opt_check_mode = Config.(mode = Check);
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
      Options.opt_module_name_mappers = FlowConfig.(
        flowconfig.options.Opts.module_name_mappers
      );
      Options.opt_profile = profile;
      Options.opt_strip_root;
      Options.opt_module;
      Options.opt_libs;
      Options.opt_no_flowlib = no_flowlib;
      Options.opt_munge_underscores = opt_munge_underscores;
      Options.opt_temp_dir;
      Options.opt_shm_dir;
      Options.opt_max_workers;
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
  let command = CommandSpec.raw_command spec (fun argv -> Main.start ())
end

module Server = struct
  module OptionParser = OptionParser (struct let mode = Normal end)
  module Main = Main (OptionParser)
  let spec = OptionParser.spec
  let command = CommandSpec.raw_command spec (fun argv -> Main.start ())
end

module Start = struct
  module OptionParser = OptionParser (struct let mode = Detach end)
  module Main = Main (OptionParser)
  let spec = OptionParser.spec
  let command = CommandSpec.raw_command spec (fun argv -> Main.start ())
end
