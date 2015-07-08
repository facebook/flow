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
    |> flag "--verbose" no_arg
        ~doc:"Print verbose info during typecheck"
    |> flag "--verbose-indent" no_arg
        ~doc:"Indent verbose info during typecheck (implies --verbose)"
    |> flag "--all" no_arg
        ~doc:"Typecheck all files, not just @flow"
    |> flag "--weak" no_arg
        ~doc:"Typecheck with weak inference, assuming dynamic types by default"
    |> flag "--traces" (optional int)
        ~doc:"Outline an error path up to a specified level"
    |> flag "--strip-root" no_arg
        ~doc:"Print paths without the root"
    |> flag "--lib" (optional string)
        ~doc:"Specify one or more library paths, comma separated"
    |> flag "--no-flowlib" no_arg
        ~doc:"Do not include embedded declarations"
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
      |> common_args
    )
  | Normal -> CommandSpec.ArgSpec.(
      empty
      |> dummy Errors_js.default_flags (* error_flags *)
      |> dummy false (* json *)
      |> dummy false (* profile *)
      |> dummy false (* quiet *)
      |> dummy None  (* log-file *)
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
  let main error_flags json profile quiet log_file debug verbose verbose_indent
           all weak traces strip_root lib no_flowlib root () =
    let root = CommandUtils.guess_root root in
    let flowconfig = FlowConfig.get root in
    let opt_module = FlowConfig.(match flowconfig.options.moduleSystem with
    | Node -> "node"
    | Haste -> "haste") in
    let opt_libs = FlowConfig.(match lib with
    | None -> flowconfig.libs
    | Some libs ->
        let libs = libs
        |> Str.split (Str.regexp ",")
        |> List.map Path.make in
        libs @ flowconfig.libs
    ) in
    let opt_traces = match traces with
      | Some level -> level
      | None -> FlowConfig.(flowconfig.options.traces) in
    let opt_strip_root = strip_root ||
      FlowConfig.(flowconfig.options.strip_root) in
    let opt_log_file = match log_file with
      | Some s ->
          let dirname = Path.make (Filename.dirname s) in
          let basename = Filename.basename s in
          Path.concat dirname basename
      | None ->
          FlowConfig.(flowconfig.options.log_file)
    in

    result := Some {
      Options.opt_check_mode = Config.(mode = Check);
      Options.opt_error_flags = error_flags;
      Options.opt_log_file = opt_log_file;
      Options.opt_root = root;
      Options.opt_should_detach = Config.(mode = Detach);
      Options.opt_debug = debug;
      Options.opt_verbose = verbose || verbose_indent;
      Options.opt_verbose_indent = verbose_indent;
      Options.opt_all = all;
      Options.opt_weak = weak;
      Options.opt_traces;
      Options.opt_strict = true;
      Options.opt_json = json;
      Options.opt_quiet = quiet || json;
      Options.opt_module_name_mappers = FlowConfig.(
        flowconfig.options.module_name_mappers
      );
      Options.opt_profile = profile;
      Options.opt_strip_root;
      Options.opt_module;
      Options.opt_libs;
      Options.opt_no_flowlib = no_flowlib;
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
