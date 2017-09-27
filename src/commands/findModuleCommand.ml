(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
    |> from_flag
    |> anon "module" (required string)
        ~doc:"Module reference to resolve"
    |> anon "file" (required string)
        ~doc:"File name containing module reference"
  )
}

let main option_values root json pretty strip_root from moduleref filename () =
  FlowEventLogger.set_from from;
  let root = guess_root (
    match root with Some root -> Some root | None -> Some filename
  ) in

  let ic, oc = connect option_values root in

  send_command oc (ServerProt.FIND_MODULE (moduleref, filename));
  let response: File_key.t option = Timeout.input_value ic in
  let result = match response with
    | Some File_key.LibFile file
    | Some File_key.SourceFile file
    | Some File_key.JsonFile file
    | Some File_key.ResourceFile file ->
        if strip_root then Files.relative_path (Path.to_string root) file
        else file
    | Some File_key.Builtins -> "(global)"
    | None -> "(unknown)" in
  if json || pretty
  then (
    let open Hh_json in
    let json = JSON_Object (["file", JSON_String result]) in
    print_endline (json_to_string ~pretty json)
  ) else
    Printf.printf "%s\n%!" result

let command = CommandSpec.command spec main
