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

let extract_location req req_locs = Utils.SMap.find_unsafe req req_locs

let main option_values root json strip_root modules () =
  let root = guess_root root in

  let ic, oc = connect option_values root in

  ServerProt.cmd_to_channel oc (ServerProt.GET_IMPORTS modules);
  let requirements_map, non_flow = Timeout.input_value ic in
  let requirements_map = Utils.SMap.fold
    begin fun module_name (requires, req_locs) map ->
      let requirements = Module_js.NameSet.fold (fun req assoc ->
        let loc = extract_location (Modulename.to_string req) req_locs in
        let req = match req with
          | Modulename.String s -> s
          | Modulename.Filename f ->
            let f = Loc.string_of_filename f in
            if strip_root then Files_js.relative_path (Path.to_string root) f
            else f
        in
        let loc = relativize strip_root root loc in
        (req, loc)::assoc
      ) requires [] in
      Utils.SMap.add module_name requirements map
    end
    requirements_map Utils.SMap.empty in
  if json
  then (
    let json_non_flow =
      Utils.SSet.fold (fun module_name json_list ->
          (module_name, Hh_json.JSON_Object [
                          "not_flow", Hh_json.JSON_Bool true;
                          "requirements", Hh_json.JSON_Array []
                          ]) :: json_list
        ) non_flow [] in
    let json_imports =
      Utils.SMap.fold (fun module_name assoc json_list ->
          let requirements =
            List.map (fun (req, loc) ->
              Hh_json.JSON_Object (
                ("import", Hh_json.JSON_String req) ::
                  (Errors_js.json_of_loc loc)
              )
            ) assoc in
          (module_name, Hh_json.JSON_Object [
                          "not_flow", Hh_json.JSON_Bool false;
                          "requirements", Hh_json.JSON_Array requirements
                          ]) :: json_list
        ) requirements_map [] in
    let json_output = Hh_json.JSON_Object (List.append json_non_flow json_imports) in
    output_string stdout ((Hh_json.json_to_string json_output)^"\n");
    flush stdout
  ) else (
    let print_imports module_name =
      if (Utils.SMap.mem module_name requirements_map)
      then begin
        let requirements = Utils.SMap.find_unsafe module_name requirements_map in
        Printf.printf "Imports for module '%s':\n" module_name;
        List.iter (fun (req, loc) ->
          Printf.printf "\t%s@%s\n" req (range_string_of_loc loc)
        ) requirements
      end else if (Utils.SSet.mem module_name non_flow)
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
