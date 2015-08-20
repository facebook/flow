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
(* flow port (transform docblock-style annotations) command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "port";
  doc = "Shows ported type annotations for given files";
  usage = Printf.sprintf
    "Usage: %s port [OPTION]... [FILE]...\n\n\
      Ports types in one or more files\n\n\
      Example usage:\n\
      \t%s port file1 file2\n"
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> anon "files" (required (list_of string))
        ~doc:"File(s) to port"
  )
}

let main option_values files () =
  let root = guess_root (Some (List.hd files)) in
  let ic, oc = connect_with_autostart option_values root in
  let files = List.map expand_path files in
  ServerProt.cmd_to_channel oc (ServerProt.PORT files);
  let patch_map = Marshal.from_channel ic in
  Utils.SMap.iter (fun file patches ->
    Printf.printf "%s\n%s" file patches
  ) patch_map;
  flush stdout

let command = CommandSpec.command spec main
