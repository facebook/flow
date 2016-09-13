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
(* flow get importers command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "get-importers";
  doc = "Gets a list of all importers for one or more given modules";
  usage = Printf.sprintf
    "Usage: %s get-importers [OPTION]... [FILE]...\n\n\
      Gets a list of all importers for one or more given modules\n\n\
      Example usage:\n\
      \t%s get-importers FirstModule SecondModule\n"
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> json_flags
    |> strip_root_flag
    |> anon "modules" (required (list_of string))
        ~doc:"Module name(s) to find"
  )
}

let main option_values root json pretty strip_root modules () =
  let root = guess_root root in

  let ic, oc = connect option_values root in

  ServerProt.cmd_to_channel oc (ServerProt.GET_IMPORTERS modules);
  let importers_map = Timeout.input_value ic in
  let importers_map = SMap.fold (fun module_name importers map ->
    let importer_list = List.map (function
      | Modulename.String s -> s
      | Modulename.Filename f ->
        let f = Loc.string_of_filename f in
        if strip_root then Files.relative_path (Path.to_string root) f
        else f
    ) (Module_js.NameSet.elements importers) in
    SMap.add module_name importer_list map
  ) importers_map SMap.empty in
  let json = json || pretty in
  if json
  then (
    let open Hh_json in
    let json_list =
      SMap.fold (fun module_name importer_list json_list ->
        let importer_list = List.map (fun entry ->
          JSON_String entry
        ) importer_list in
        (module_name, JSON_Array importer_list) :: json_list
      ) importers_map [] in
    let json_output = JSON_Object json_list in
    print_endline (json_to_string ~pretty json_output)
  ) else (
    List.iter (fun module_name ->
      if (SMap.mem module_name importers_map)
      then begin
        let importer_list =
          SMap.find_unsafe module_name importers_map in
        Printf.printf "Modules importing module '%s':\n" module_name;
        List.iter (fun entry ->
          Printf.printf "\t%s\n" entry
        ) importer_list
      end else
        Printf.printf "Module '%s' could not be found!\n" module_name
    ) modules;
    flush stdout
  )

let command = CommandSpec.command spec main
