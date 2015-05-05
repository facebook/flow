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

let spec = {
  CommandSpec.
  name = "single";
  doc = "Does a single-threaded check (testing)";
  usage = Printf.sprintf "Usage: %s single ROOT\n" CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> flag "--all" no_arg
        ~doc:"Typecheck all files, not just @flow"
    |> flag "--weak" no_arg
        ~doc:"Typecheck with weak inference, assuming dynamic types by default"
    |> flag "--debug" no_arg
        ~doc:"Print debug info during typecheck"
    |> flag "--verbose" no_arg
        ~doc:"Print verbose info during typecheck"
    |> flag "--json" no_arg
        ~doc:"Output errors in JSON format"
    |> flag "--show-all-errors" no_arg
        ~doc:"Print all errors (the default is to truncate after 50 errors)"
    |> flag "--profile" no_arg
        ~doc:"Output profiling information"
    |> flag "--quiet" no_arg
        ~doc:"Suppress info messages to stdout (included in --json)"
    |> flag "--module" (optional string)
        ~doc:"Specify a module system"
    |> flag "--lib" (optional string)
        ~doc:"Specify a library path"
    |> flag "--no-flowlib" no_arg
        ~doc:"Do not include embedded declarations"
    |> anon "root" (required string)
        ~doc:"Root"
  )
}

let die str =
  let oc = stderr in
  output_string oc str;
  close_out oc;
  exit 2

let main all weak debug verbose json show_all_errors profile quiet module_
         lib no_flowlib root () =
  let opt_libs = match lib with
  | None -> []
  | Some lib -> [Path.mk_path lib]
  in

  let module_ = match module_ with
  | Some "node" -> "node"
  | Some "haste" -> "haste"
  | Some _ -> failwith "Invalid --module. Expected node or haste"
  | None -> "node"
  in

  let options = {
    Types_js.opt_all = all;
    Types_js.opt_weak = weak;
    Types_js.opt_console = false;
    Types_js.opt_debug = debug;
    Types_js.opt_verbose = verbose;
    Types_js.opt_strict = true;
    Types_js.opt_traces = 0;
    Types_js.opt_json = json;
    Types_js.opt_show_all_errors = show_all_errors;
    Types_js.opt_quiet = quiet || json;
    Types_js.opt_profile = profile;
    Types_js.opt_strip_root = false;
    Types_js.opt_module = module_;
    Types_js.opt_libs;
    Types_js.opt_no_flowlib = no_flowlib;
  } in

  if ! Sys.interactive
  then ()
  else
    SharedMem.init();
    Errors.try_
      (fun () -> Types_js.single_main [root] options)
      (fun l -> die (Errors.to_string (Errors.to_absolute l)))

let command = CommandSpec.command spec main
