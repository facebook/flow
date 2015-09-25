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
(* flow find-module (get filename of module) command *)
(***********************************************************************)

open CommandUtils

module Json = Hh_json

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
    |> json_flags
    |> flag "--strip-root" no_arg
        ~doc:"Print paths without the root"
    |> anon "module" (required string)
        ~doc:"Module reference to resolve"
    |> anon "file" (required string)
        ~doc:"File name containing module reference"
  )
}

let main option_values json strip_root moduleref filename () =
  let root = guess_root (Some filename) in

  let ic, oc = connect_with_autostart option_values root in

  ServerProt.cmd_to_channel oc (ServerProt.FIND_MODULE (moduleref, filename));
  let response: Loc.filename option = Marshal.from_channel ic in
  let result = match response with
    | Some Loc.LibFile file
    | Some Loc.SourceFile file ->
        if strip_root then Files_js.relative_path (Path.to_string root) file
        else file
    | Some Loc.Builtins -> "(global)"
    | None -> "(unknown)" in
  if json
  then (
    let json = Json.json_to_string (
      Json.JAssoc (["file", Json.JString result])
    ) in
    output_string stdout (json^"\n");
  ) else
    Printf.printf "%s\n%!" result

let command = CommandSpec.command spec main
