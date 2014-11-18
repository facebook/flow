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

type env = {
  files : string list;
  option_values : CommandUtils.command_params;
}

let parse_args () =
  let option_values, options = CommandUtils.create_command_options false in
  let usage =  Printf.sprintf
    "Usage: %s suggest [OPTION]... [FILE]...\n\n\
    Suggests types in one or more files\n\n\
    Example usage:\n\
    \t%s suggest file1 file2"
    Sys.argv.(0)
    Sys.argv.(0) in
  let files = ClientArgs.parse_without_command options usage "suggest" in
  match files with
  | [] ->
      Printf.fprintf stderr "You must provide at least one file\n%!";
      Arg.usage options usage;
      exit 2
  | _ -> ();
  { files; option_values; }

(* move to utils? *)
let split_char c s =
  try
    let i = String.index s c in
    (Str.string_before s i, Str.string_after s i)
  with _ -> (s, "")

let main { files; option_values; } =
  let (files, regions) = files |> List.map (split_char ':') |> List.split in
  let root = match files with
  | file::_ -> CommandUtils.guess_root (Some file)
  | _ -> failwith "Expected at least one file" in

  let ic, oc = CommandUtils.connect_with_autostart option_values root in
  let files = List.map ClientCheck.expand_path files in
  let files = List.map2 (^) files regions in
  ServerProt.cmd_to_channel oc (ServerProt.SUGGEST files);
  let suggestion_map = Marshal.from_channel ic in
  Utils.SMap.iter (fun file suggestions ->
    Printf.printf "%s\n%s" file suggestions
  ) suggestion_map;
  flush stdout

let run () = main (parse_args ())
