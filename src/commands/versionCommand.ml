(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow ast command *)
(***********************************************************************)

let spec =
  {
    CommandSpec.name = "version";
    doc = "Print version information";
    usage =
      Printf.sprintf
        "Usage: %s version [OPTION]... [ROOT]\n\ne.g. %s version\nor   %s version --json\nor   %s version /path/to/root\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> CommandUtils.json_flags
        |> CommandUtils.from_flag
        |> flag "--semver" no_arg ~doc:"Return only the version number"
        |> anon "root" (optional string));
  }

let print_semver json pretty =
  if json || pretty then
    Hh_json.(
      let json = JSON_Object [("semver", JSON_String Flow_version.version)] in
      print_json_endline ~pretty json)
  else
    print_endline Flow_version.version

let main json pretty semver _root () =
  if semver then
    print_semver json pretty
  else if json || pretty then
    Hh_json.(
      let json =
        JSON_Object
          [
            ("semver", JSON_String Flow_version.version);
            ("binary", JSON_String (Sys_utils.executable_path ()));
            ("build_id", JSON_String Build_id.build_revision);
            ("flow_build_id", JSON_String (Flow_build_id.get_build_id ()));
          ]
      in
      print_json_endline ~pretty json)
  else
    CommandUtils.print_version ();
  Exit.(exit No_error)

let command = CommandSpec.command spec main
