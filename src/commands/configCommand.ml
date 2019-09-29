(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow config command *)
(***********************************************************************)

open CommandUtils

let find_subcommand =
  let spec =
    {
      CommandSpec.name = "config find";
      doc = "Return path to .flowconfig";
      usage =
        Printf.sprintf
          "Usage: %s config find [ROOT]\nReturn the path to the .flowconfig file\n\ne.g. %s config find /path/to/root"
          CommandUtils.exe_name
          CommandUtils.exe_name;
      args =
        CommandSpec.ArgSpec.(
          empty |> flowconfig_name_flag |> json_flags |> anon "root" (optional string));
    }
  in
  let main flowconfig_name json pretty root () =
    let root = guess_root flowconfig_name root |> Path.to_string in
    FlowEventLogger.set_root (Some root);
    if json || pretty then
      Hh_json.(
        let json = JSON_Object [("root", JSON_String root)] in
        print_json_endline ~pretty json)
    else
      print_endline root
  in
  CommandSpec.command spec main

let check_subcommand =
  let spec =
    {
      CommandSpec.name = "config check";
      doc = "Validates the .flowconfig file";
      usage =
        Printf.sprintf
          "Usage: %s config check [FILE]\nValidates the .flowconfig file\n\ne.g. %s config check /path/to/.flowconfig"
          CommandUtils.exe_name
          CommandUtils.exe_name;
      args =
        CommandSpec.ArgSpec.(
          empty
          |> flowconfig_name_flag
          |> json_flags
          |> root_flag
          |> ignore_version_flag
          |> anon "file" (optional string));
    }
  in
  (* If a flowconfig was passed in, confirm it exists; otherwise, search for it using the
     --root and --flowconfig-name flags. *)
  let find_flowconfig flowconfig_name root = function
    | Some file ->
      ( if not (Sys.file_exists file) then
        let msg = Utils_js.spf "Could not find file %s" file in
        FlowExitStatus.(exit ~msg Could_not_find_flowconfig) );
      let root = Path.make (Filename.dirname file) in
      (file, root |> Path.to_string)
    | None ->
      let root = guess_root flowconfig_name root in
      let file = Server_files_js.config_file flowconfig_name root in
      (file, root |> Path.to_string)
  in
  let json_of_issue kind (line, msg) =
    Hh_json.(
      JSON_Object
        [
          ("line", JSON_Number (string_of_int line));
          ("message", JSON_String msg);
          ( "level",
            JSON_String
              (match kind with
              | `Error -> "error"
              | `Warning -> "warning") );
        ])
  in
  let exit_with_json ~pretty json =
    Hh_json.(
      FlowExitStatus.(
        let code = Invalid_flowconfig in
        let json = JSON_Object (("errors", json) :: FlowExitStatus.json_props_of_t code) in
        Hh_json.print_json_endline ~pretty json;
        FlowExitStatus.unset_json_mode ();
        FlowExitStatus.(exit code)))
  in
  let main flowconfig_name json pretty root ignore_version file () =
    let (file, root) = find_flowconfig flowconfig_name root file in
    FlowEventLogger.set_root (Some root);
    match FlowConfig.get ~allow_cache:false file with
    | Ok (config, []) ->
      if not ignore_version then
        assert_version config
      else
        ()
    | Ok (config, warnings) ->
      if ignore_version then
        if json || pretty then
          Hh_json.JSON_Object [] |> Hh_json.json_to_string ~pretty |> print_endline
        else
          ()
      else (
        assert_version config;
        if json || pretty then
          Hh_json.(
            let json = JSON_Array (List.map (json_of_issue `Warning) warnings) in
            exit_with_json ~pretty json)
        else
          flowconfig_multi_error warnings
      )
    | Error err ->
      if json || pretty then
        Hh_json.(
          let json = JSON_Array [json_of_issue `Error err] in
          exit_with_json ~pretty json)
      else
        flowconfig_multi_error [err]
  in
  CommandSpec.command spec main

let command =
  let spec =
    {
      CommandSpec.name = "config";
      doc = "Read or write the .flowconfig file";
      usage =
        Printf.sprintf
          "Usage: %s config SUBCOMMAND [ROOT]\nRead or write the .flowconfig file\n\nSUBCOMMANDS:\nfind: Return the path to the .flowconfig\n"
          CommandUtils.exe_name;
      args =
        CommandSpec.ArgSpec.(
          empty
          |> CommandUtils.from_flag
          |> anon
               "subcommand"
               (required (command [("check", check_subcommand); ("find", find_subcommand)])));
    }
  in
  let main (cmd, argv) () = CommandUtils.run_command cmd argv in
  CommandSpec.command spec main
