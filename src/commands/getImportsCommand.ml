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
(* flow get imports command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "get-imports";
  doc = "Get names of all modules imported by one or more given modules";
  usage = Printf.sprintf
    "Usage: %s get-requirements [OPTION]... [FILE]...\n\n\
      Get names of all modules imported by one or more given modules\n\n\
      Example usage:\n\
      \t%s get-imports FirstModule SecondModule\n"
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

let extract_location req req_locs = SMap.find_unsafe req req_locs

let main option_values root json pretty strip_root modules () =
  let root = guess_root root in

  let ic, oc = connect option_values root in

  ServerProt.cmd_to_channel oc (ServerProt.GET_IMPORTS modules);
  let requirements_map, non_flow = Timeout.input_value ic in
  let requirements_map = SMap.fold
    begin fun module_name (requires, req_locs) map ->
      let requirements = Module_js.NameSet.fold (fun req assoc ->
        let loc = extract_location (Modulename.to_string req) req_locs in
        let req = match req with
          | Modulename.String s -> s
          | Modulename.Filename f ->
            let f = Loc.string_of_filename f in
            if strip_root then Files.relative_path (Path.to_string root) f
            else f
        in
        let loc = relativize strip_root root loc in
        (req, loc)::assoc
      ) requires [] in
      SMap.add module_name requirements map
    end
    requirements_map SMap.empty in
  if json || pretty
  then (
    let open Hh_json in
    let json_non_flow = SSet.fold (fun module_name acc ->
      let json = JSON_Object [
        "not_flow", JSON_Bool true;
        "requirements", JSON_Array []
      ] in
      (module_name, json) :: acc
    ) non_flow [] in
    let json_imports = SMap.fold (fun module_name assoc acc ->
      let requirements = List.map (fun (req, loc) ->
        JSON_Object (
          ("import", JSON_String req) ::
          ("loc", Reason.json_of_loc loc) ::
          (Errors.deprecated_json_props_of_loc loc)
        )
      ) assoc in
      let json = JSON_Object [
        "not_flow", JSON_Bool false;
        "requirements", JSON_Array requirements
      ] in
      (module_name, json) :: acc
    ) requirements_map [] in
    let json = JSON_Object (List.append json_non_flow json_imports) in
    print_endline (json_to_string ~pretty json)
  ) else (
    let print_imports module_name =
      if (SMap.mem module_name requirements_map)
      then begin
        let requirements = SMap.find_unsafe module_name requirements_map in
        Printf.printf "Imports for module '%s':\n" module_name;
        List.iter (fun (req, loc) ->
          Printf.printf "\t%s@%s\n" req (range_string_of_loc loc)
        ) requirements
      end else if (SSet.mem module_name non_flow)
      then
        Printf.printf "Cannot obtain imports for module '%s' because is not\
                       \ marked for processing by flow!\n" module_name
      else
        Printf.printf "Module '%s' could not be found!\n" module_name
    in
    List.iter print_imports modules;
    flush stdout
  )

let command = CommandSpec.command spec main
