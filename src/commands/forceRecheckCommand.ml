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
(* flow force-recheck *)
(***********************************************************************)

let spec = {
  CommandSpec.
  name = "force-recheck";
  doc = "Forces the server to recheck a given list of files";
  usage = Printf.sprintf
    "Usage: %s force-recheck [OPTION]... FILES\n\
      Forces the Flow server to recheck a given list of files.\n"
    CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> CommandUtils.server_flags
    |> CommandUtils.root_flag
    |> anon "files" (required (list_of string))
        ~doc:"Specify files to recheck"
  )
}

type args = {
  root: Path.t;
  files: string list;
}

let force_recheck (args:args) server_flags =
  let ic, oc = CommandUtils.connect server_flags args.root in
  let files = List.map CommandUtils.get_path_of_file args.files in
  ServerProt.cmd_to_channel oc
    (ServerProt.FORCE_RECHECK files);
  let () = Timeout.input_value ic in
  FlowExitStatus.(exit Ok)

let main server_flags root files () =
  let root = CommandUtils.guess_root (
    match root, files with
    | Some root, _ -> Some root
    | None, file::_ -> (Some file)
    | None, [] -> None
  ) in
  let args = { root; files } in
  force_recheck args server_flags

let command = CommandSpec.command spec main
