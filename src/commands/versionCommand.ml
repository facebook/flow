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

let main json pretty from _root () =
  FlowEventLogger.set_from from;
  if json || pretty
  then begin
    let open Hh_json in
    let json = JSON_Object [
      "semver", JSON_String FlowConfig.version;
      "binary", JSON_String (Sys_utils.executable_path ());
      "build_id", JSON_String Build_id.build_id_ohai;
    ] in
    print_endline (json_to_string ~pretty json)
  end else begin
    CommandUtils.print_version ()
  end;
  FlowExitStatus.(exit No_error)

let command = CommandSpec.command spec main
