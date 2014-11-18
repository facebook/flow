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

type env = {
  files : string list;
  option_values : CommandUtils.command_params;
}

let parse_args () =
  let option_values, options = CommandUtils.create_command_options false in
  let usage =  Printf.sprintf
    "Usage: %s port [OPTION]... [FILE]...\n\n\
    Ports types in one or more files\n\n\
    Example usage:\n\
    \t%s port file1 file2"
    Sys.argv.(0)
    Sys.argv.(0) in
  let files = ClientArgs.parse_without_command options usage "port" in
  match files with
  | [] ->
      Printf.fprintf stderr "You must provide at least one file\n%!";
      Arg.usage options usage;
      exit 2
  | _ -> ();
  { files; option_values; }

let main { files; option_values; } =
  let root = match files with
  | file::_ -> CommandUtils.guess_root (Some file)
  | _ -> failwith "Expected at least one file" in

  let ic, oc = CommandUtils.connect_with_autostart option_values root in
  let files = List.map ClientCheck.expand_path files in
  ServerProt.cmd_to_channel oc (ServerProt.PORT files);
  let patch_map = Marshal.from_channel ic in
  Utils.SMap.iter (fun file patches ->
    Printf.printf "%s\n%s" file patches
  ) patch_map;
  flush stdout

let run () = main (parse_args ())
