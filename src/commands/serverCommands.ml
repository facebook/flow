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
    |> flag "--all" no_arg
        ~doc:"Typecheck all files, not just @flow"
    |> flag "--weak" no_arg
        ~doc:"Typecheck with weak inference, assuming dynamic types by default"
    |> flag "--traces" no_arg
        ~doc:"Outline an error path"
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
      |> flag "--json" no_arg
          ~doc:"Output errors in JSON format"
      |> flag "--show-all-errors" no_arg
          ~doc:"Print all errors (the default is to truncate after 50 errors)"
      |> flag "--profile" no_arg
          ~doc:"Output profiling information"
      |> flag "--quiet" no_arg
          ~doc:"Suppress info messages to stdout (included in --json)"
      |> common_args
    )
  | Normal
  | Detach -> CommandSpec.ArgSpec.(
      empty
      |> dummy false
      |> dummy false
      |> dummy false
      |> dummy false
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
        CommandUtils.exe_name cmdname cmddoc;
  }

  let result = ref None
  let main json show_all_errors profile quiet debug verbose all weak
           traces strip_root lib no_flowlib root () =
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
        |> List.map Path.mk_path in
        libs @ flowconfig.libs
    ) in
    let opt_traces = traces || FlowConfig.(flowconfig.options.traces) in

    (* hack opts and flow opts: latter extends the former *)
    result := Some ({
      ServerArgs.check_mode    = Config.(mode = Check);
      ServerArgs.json_mode     = json;
      ServerArgs.root          = root;
      ServerArgs.should_detach = Config.(mode = Detach);
      ServerArgs.convert       = None;
      ServerArgs.no_load       = false;
      ServerArgs.save_filename = None;
    },
    {
      Types_js.opt_debug = debug;
      Types_js.opt_verbose = verbose;
      Types_js.opt_all = all;
      Types_js.opt_weak = weak;
      Types_js.opt_traces;
      Types_js.opt_newtraces = false;
      Types_js.opt_strict = true;
      Types_js.opt_console = false;
      Types_js.opt_json = json;
      Types_js.opt_show_all_errors = show_all_errors;
      Types_js.opt_quiet = quiet || json;
      Types_js.opt_profile = profile;
      Types_js.opt_strip_root = strip_root;
      Types_js.opt_module;
      Types_js.opt_libs;
      Types_js.opt_no_flowlib = no_flowlib;
    });
    ()

  let rec do_parse () =
    match !result with
    | Some result -> result
    | None ->
        let argv = Array.to_list Sys.argv in
        CommandSpec.main spec main argv;
        do_parse ()

  let parse () = fst (do_parse ())
  let get_flow_options () = snd (do_parse ())
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
