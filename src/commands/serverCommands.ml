(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
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
      let quiet = ref false in
      let strip_root = ref false in
      let module_ = ref "haste" in
      let lib = ref None in
      let variant_opts = match Config.mode with
      | Check -> [
        "--json", CommandUtils.arg_set_unit json,
          " Output errors in JSON format";
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
        "--module", CommandUtils.arg_set_enum
            ["node";"haste"] module_,
          " Specify a module system";
        "--lib", CommandUtils.arg_set_string lib,
          " Specify a library path";
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
      (* hack opts and flow opts: latter extends the former *)
      let ret = ({
        ServerArgs.check_mode    = Config.(mode = Check);
        ServerArgs.json_mode     = !json;
        ServerArgs.root          = root;
        ServerArgs.should_detach = Config.(mode = Detach);
        ServerArgs.convert       = None;
        ServerArgs.load_save_opt = None;
        ServerArgs.version       = false;
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
        Types_js.opt_quiet = !quiet || !json;
        Types_js.opt_strip_root = !strip_root;
        Types_js.opt_module = !module_;
        Types_js.opt_lib = !lib;
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
