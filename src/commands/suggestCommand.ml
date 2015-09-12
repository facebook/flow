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
(* flow suggest (infer types for file) command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "suggest";
  doc = "Shows type annotation suggestions for given files";
  usage = Printf.sprintf
    "Usage: %s suggest [OPTION]... [FILE]...\n\n\
      Suggests types in one or more files\n\n\
      Example usage:\n\
      \t%s suggest file1 file2\n"
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> anon "files" (required (list_of string)) ~doc:"Files"
  )
}

(* move to utils? *)
let split_char c s =
  try
    let i = String.index s c in
    (Str.string_before s i, Str.string_after s i)
  with _ -> (s, "")

let main option_values files () =
  let (files, regions) = files |> List.map (split_char ':') |> List.split in
  let root = match files with
  | file::_ -> guess_root (Some file)
  | _ -> failwith "Expected at least one file" in

  let ic, oc = connect_with_autostart option_values root in
  let files = List.map expand_path files in
  let files = List.map2 (^) files regions in
  ServerProt.cmd_to_channel oc (ServerProt.SUGGEST files);
  let suggestion_map = Marshal.from_channel ic in
  Utils.SMap.iter (fun file suggestions ->
    Printf.printf "%s\n%s" file suggestions
  ) suggestion_map;
  flush stdout

let command = CommandSpec.command spec main
