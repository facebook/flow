(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow config command *)
(***********************************************************************)

open CommandUtils

let find_subcommand =
  let spec = {
    CommandSpec.
    name = "config find";
    doc = "Return path to .flowconfig";
    usage = Printf.sprintf
      "Usage: %s config find [ROOT]\n\
        Return the path to the .flowconfig file\n\n\
        e.g. %s config find /path/to/root"
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args = CommandSpec.ArgSpec.(
      empty
      |> flowconfig_name_flag
      |> json_flags
      |> anon "root" (optional string)
    )
  } in
  let main flowconfig_name json pretty root () =
    let root = guess_root flowconfig_name root |> Path.to_string in
    FlowEventLogger.set_root (Some root);
    if json || pretty then
      let open Hh_json in
      let json = JSON_Object [
        "root", JSON_String root;
      ] in
      print_json_endline ~pretty json
    else
      print_endline root
  in
  CommandSpec.command spec main


let command =
  let spec = {
    CommandSpec.
    name = "config";
    doc = "Read or write the .flowconfig file";
    usage = Printf.sprintf
      "Usage: %s config SUBCOMMAND [ROOT]\n\
        Read or write the .flowconfig file\n\n\
        SUBCOMMANDS:\n\
          find: Return the path to the .flowconfig\n"
        CommandUtils.exe_name;
    args = CommandSpec.ArgSpec.(
      empty
      |> CommandUtils.from_flag
      |> anon "subcommand" (required (command [
           "find", find_subcommand
         ]))
    )
  }
  in
  let main from (cmd, argv) () =
    FlowEventLogger.set_from from;
    CommandUtils.run_command cmd argv
  in
  CommandSpec.command spec main
