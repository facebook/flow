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
  let show_all_errors = ref false in
  let quiet = ref false in
  let profile = ref false in
  let newtraces = ref false in
  let traces = ref false in
  let module_ = ref "node" in
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
    "--show-all-errors", CommandUtils.arg_set_unit show_all_errors,
      " Print all errors (the default is to truncate after 50 errors)";
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
  let opt_libs = match !lib with
  | None -> []
  | Some lib -> [Path.mk_path lib] in
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
      Types_js.opt_show_all_errors = !show_all_errors;
      Types_js.opt_quiet = !quiet || !json;
      Types_js.opt_profile = !profile;
      Types_js.opt_strip_root = false;
      Types_js.opt_module = !module_;
      Types_js.opt_libs;
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
