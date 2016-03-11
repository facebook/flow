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
(* flow port (transform docblock-style annotations) command *)
(***********************************************************************)

open CommandUtils
open Utils_js

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
    |> root_flag
    |> anon "files" (required (list_of string))
        ~doc:"File(s) to port"
  )
}

let main option_values root files () =
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> Some (List.hd files)
  ) in
  let ic, oc = connect option_values root in
  let files = List.map expand_path files in
  ServerProt.cmd_to_channel oc (ServerProt.PORT files);
  let patch_map: ((string, exn) ok_or_err) SMap.t = Timeout.input_value ic in
  SMap.iter (fun file patches_or_err ->
    match patches_or_err with
    | OK patches ->
      Printf.printf "%s\n%s" file patches
    | Err exn ->
      Printf.eprintf
        "Could not port docblock-style annotations for %s\n%s"
        file
        ((Printexc.to_string exn) ^ "\n" ^ (Printexc.get_backtrace ()));
  ) patch_map;
  flush stderr;
  flush stdout

let command = CommandSpec.command spec main
