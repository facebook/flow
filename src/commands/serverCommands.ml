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

module OptionParser(Config : CONFIG) : Server.OPTION_PARSER = struct
  let result = ref None
  let do_parse () =
    match !result with
    | Some result -> result
    | None ->
      let debug = ref false in
      let all = ref false in
      let weak = ref false in
      let traces = ref false in
      let newtraces = ref false in
      let console = ref false in
      let json = ref false in
      let show_all_errors = ref false in
      let quiet = ref false in
      let profile = ref false in
      let strip_root = ref false in
      let lib = ref None in
      let no_flowlib = ref false in
      let variant_opts = match Config.mode with
      | Check -> [
        "--json", CommandUtils.arg_set_unit json,
          " Output errors in JSON format";
        "--show-all-errors", CommandUtils.arg_set_unit show_all_errors,
          " Print all errors (the default is to truncate after 50 errors)";
        "--profile", CommandUtils.arg_set_unit profile,
          " Output profiling information";
        "--quiet", CommandUtils.arg_set_unit quiet,
          " Suppress info messages to stdout (included in --json)"; ]
      | _ -> [] in
      let options = CommandUtils.sort_opts (List.append variant_opts [
        "--debug", CommandUtils.arg_set_unit debug,
          " Print debug info during typecheck";
        "--all", CommandUtils.arg_set_unit all,
          " Typecheck all files, not just @flow";
        "--weak", CommandUtils.arg_set_unit weak,
          " Typecheck with weak inference, assuming dynamic types by default";
        "--traces", CommandUtils.arg_set_unit traces,
          " Outline an error path";
        "--strip-root", CommandUtils.arg_set_unit strip_root,
          " Print paths without the root";
        "--lib", CommandUtils.arg_set_string lib,
          " Specify one or more library paths, comma separated";
        "--no-flowlib", CommandUtils.arg_set_unit no_flowlib,
          " Do not include embedded declarations";
      ]) in
      let cmdname = match Config.mode with
        | Check -> "check" | Normal -> "server" | Detach -> "start"
      in
      let usage = (Printf.sprintf "Usage: %s %s [OPTION]... [ROOT]\n"
        Sys.argv.(0) cmdname)
      ^
      (match Config.mode with
      | Check -> "Does a full Flow check and prints the results\n\n"
      | Normal -> "Runs a Flow server in the foreground\n\n"
      | Detach -> "Starts a Flow server\n\n")
      ^
      "Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
      ROOT is assumed to be the current directory if unspecified.\n\
      A server will be started if none is running over ROOT.\n"
      in
      let args = ClientArgs.parse_without_command options usage cmdname in
      let root = match args with
      | [] -> CommandUtils.guess_root None
      | [x] -> CommandUtils.guess_root (Some x)
      | _ -> Arg.usage options usage; exit 2 in
      let flowconfig = FlowConfig.get root in
      let opt_module = FlowConfig.(match flowconfig.options.moduleSystem with
      | Node -> "node"
      | Haste -> "haste") in
      let opt_libs = FlowConfig.(match !lib with
      | None -> flowconfig.libs
      | Some libs ->
          let libs = libs
          |> Str.split (Str.regexp ",")
          |> List.map Path.mk_path in
          libs @ flowconfig.libs
      ) in

      (* hack opts and flow opts: latter extends the former *)
      let ret = ({
        ServerArgs.check_mode    = Config.(mode = Check);
        ServerArgs.json_mode     = !json;
        ServerArgs.root          = root;
        ServerArgs.should_detach = Config.(mode = Detach);
        ServerArgs.convert       = None;
        ServerArgs.load_save_opt = None;
        ServerArgs.gc_control    = Gc.get ();
        ServerArgs.assume_php    = false;
      },
      {
        Types_js.opt_debug = !debug;
        Types_js.opt_all = !all;
        Types_js.opt_weak = !weak;
        Types_js.opt_traces = !traces;
        Types_js.opt_newtraces = !newtraces;
        Types_js.opt_strict = true;
        Types_js.opt_console = !console;
        Types_js.opt_json = !json;
        Types_js.opt_show_all_errors = !show_all_errors;
        Types_js.opt_quiet = !quiet || !json;
        Types_js.opt_profile = !profile;
        Types_js.opt_strip_root = !strip_root;
        Types_js.opt_module;
        Types_js.opt_libs;
        Types_js.opt_no_flowlib = !no_flowlib;
      }) in
      result := Some ret;
      ret

  let parse () = fst (do_parse ())
  let get_flow_options () = snd (do_parse ())
end

module Main (Config : CONFIG) =
  ServerFunctors.ServerMain (Server.FlowProgram (OptionParser (Config)))

module Check = struct
  module Main = Main (struct let mode = Check end)
  let run = Main.start
end

module Server = struct
  module Main = Main (struct let mode = Normal end)
  let run = Main.start
end

module Start = struct
  module Main = Main (struct let mode = Detach end)
  let run = Main.start
end
