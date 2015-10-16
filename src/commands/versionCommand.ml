(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(***********************************************************************)
(* flow ast command *)
(***********************************************************************)

let spec = {
  CommandSpec.
  name = "version";
  doc = "Print version information";
  usage = Printf.sprintf
    "Usage: %s version [OPTION]... [ROOT]\n\n\
      e.g. %s version\n\
      or   %s version --json\n\
      or   %s version /path/to/root\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> CommandUtils.json_flags
    |> CommandUtils.from_flag
    |> anon "root" (optional string) ~doc:"Root directory"
  )
}

let main json from root () =
  FlowEventLogger.set_from from;
  if json
  then begin
    let open Hh_json in
    let json = JAssoc [
      "semver",JString FlowConfig.version;
      "binary", JString (Sys_utils.executable_path ());
      "build_id", JString Build_id.build_id_ohai;
    ] in
    print_endline (json_to_string json)
  end else begin
    CommandUtils.print_version ()
  end;
  FlowExitStatus.(exit Ok)

let command = CommandSpec.command spec main
