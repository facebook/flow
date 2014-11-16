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
(* flow single (run analysis single-threaded) command *)
(***********************************************************************)

type env = {
  filename : string;
  options : Types_js.options;
}

let parse_args () =
  let all = ref false in
  let weak = ref false in
  let console = ref false in
  let debug = ref false in
  let json = ref false in
  let quiet = ref false in
  let profile = ref false in
  let newtraces = ref false in
  let traces = ref false in
  let module_ = ref "haste" in
  let lib = ref None in
  let options = CommandUtils.sort_opts [
    "--all", CommandUtils.arg_set_unit all,
      " Typecheck all files, not just @flow";
    "--weak", CommandUtils.arg_set_unit weak,
      " Typecheck with weak inference, assuming dynamic types by default";
    "--debug", CommandUtils.arg_set_unit debug,
      " Print verbose debug info during typecheck";
    "--json", CommandUtils.arg_set_unit json,
      " Output errors in JSON format";
    "--profile", CommandUtils.arg_set_unit profile,
      " Output profiling information";
    "--quiet", CommandUtils.arg_set_unit quiet,
      " Suppress info messages to stdout (included in --json)";
    "--module", CommandUtils.arg_set_enum
        ["node";"haste"] module_,
      " Specify a module system";
    "--lib", CommandUtils.arg_set_string lib,
      " Specify a library path";
  ] in
  let usage = Printf.sprintf "Usage: %s single ROOT" Sys.argv.(0) in
  let args = ClientArgs.parse_without_command options usage "single" in
  match args with
  | [filename] ->
    let options = {
      Types_js.opt_all = !all;
      Types_js.opt_weak = !weak;
      Types_js.opt_console = !console;
      Types_js.opt_debug = !debug;
      Types_js.opt_newtraces = !newtraces;
      Types_js.opt_strict = true;
      Types_js.opt_traces = !traces;
      Types_js.opt_json = !json;
      Types_js.opt_quiet = !quiet || !json;
      Types_js.opt_profile = !profile;
      Types_js.opt_strip_root = false;
      Types_js.opt_module = !module_;
      Types_js.opt_lib = !lib;
    } in
    { filename; options; }
  | _ ->
      Arg.usage options usage;
      exit 2

let die str =
  let oc = stderr in
  output_string oc str;
  close_out oc;
  exit 2

let main { filename; options; } =
  if ! Sys.interactive
  then ()
  else
    SharedMem.init();
    Errors.try_
      (fun () -> Types_js.single_main [filename] options)
      (fun l -> die (Errors.to_string (Errors.to_absolute l)))

let run () = main (parse_args ())
