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
(* flow find-module (get filename of module) command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "find-module";
  doc = "Resolves a module reference to a file";
  usage = Printf.sprintf
    "Usage: %s find-module [OPTION]... [FILE]...\n\n\
      Resolves a module reference to a file\n\n\
      Example usage:\n\
      \t%s find-module moduleref filename\n"
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> json_flags
    |> strip_root_flag
    |> anon "module" (required string)
        ~doc:"Module reference to resolve"
    |> anon "file" (required string)
        ~doc:"File name containing module reference"
  )
}

let main option_values root json pretty strip_root moduleref filename () =
  let root = guess_root (
    match root with Some root -> Some root | None -> Some filename
  ) in

  let ic, oc = connect option_values root in

  ServerProt.cmd_to_channel oc (ServerProt.FIND_MODULE (moduleref, filename));
  let response: Loc.filename option = Timeout.input_value ic in
  let result = match response with
    | Some Loc.LibFile file
    | Some Loc.SourceFile file
    | Some Loc.JsonFile file
    | Some Loc.ResourceFile file ->
        if strip_root then Files.relative_path (Path.to_string root) file
        else file
    | Some Loc.Builtins -> "(global)"
    | None -> "(unknown)" in
  if json || pretty
  then (
    let open Hh_json in
    let json = JSON_Object (["file", JSON_String result]) in
    print_endline (json_to_string ~pretty json)
  ) else
    Printf.printf "%s\n%!" result

let command = CommandSpec.command spec main
