(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
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
