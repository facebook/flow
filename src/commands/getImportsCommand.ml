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

type env = {
  modules : string list;
  option_values : command_params;
}

let parse_args () =
  let option_values, options = create_command_options true in
  let options = sort_opts options in
  let usage =  Printf.sprintf
    "Usage: %s get-requirements [OPTION]... [FILE]...\n\n\
    Get names of all modules imported by one or more given modules\n\n\
    Example usage:\n\
    \t%s get-imports FirstModule SecondModule"
    Sys.argv.(0)
    Sys.argv.(0) in
  let modules = ClientArgs.parse_without_command options usage "get-imports" in
  match modules with
  | [] ->
      Printf.fprintf stderr "You must provide at least one module name\n%!";
      Arg.usage options usage;
      exit 2
  | _ -> ();
  { modules; option_values; }

module Json = Hh_json

let extract_position req req_locs =
  let loc = Utils.SMap.find_unsafe req req_locs in
  Reason_js.pos_of_loc loc

let main { modules; option_values; } =
  let root = guess_root (Some (Sys.getcwd ())) in

  let ic, oc = connect_with_autostart option_values root in

  ServerProt.cmd_to_channel oc (ServerProt.GET_IMPORTS modules);
  let requirements_map, non_flow = Marshal.from_channel ic in
  if !(option_values.json)
  then (
    let json_non_flow =
      Utils.SSet.fold (fun module_name json_list ->
          (module_name, Json.JAssoc [
                          "not_flow", Json.JBool true;
                          "requirements", Json.JList []
                          ]) :: json_list
        ) non_flow [] in
    let json_imports =
      Utils.SMap.fold (fun module_name (requires, req_locs) json_list ->
          let requirements =
            Utils.SSet.fold (fun req json_list ->
                let pos = extract_position req req_locs in
                Json.JAssoc (
                  ("import", Json.JString req) ::
                  (Errors_js.pos_to_json pos)
                ) :: json_list
              ) requires [] in
          (module_name, Json.JAssoc [
                          "not_flow", Json.JBool false;
                          "requirements", Json.JList requirements
                          ]) :: json_list
        ) requirements_map [] in
    let json_output = Json.JAssoc (List.append json_non_flow json_imports) in
    output_string stdout (Json.json_to_string json_output);
    flush stdout
  ) else (
    let print_imports module_name =
      if (Utils.SMap.mem module_name requirements_map)
      then begin
        let (requirements, req_locs) =
          Utils.SMap.find_unsafe module_name requirements_map in
        Printf.printf "Imports for module '%s':\n" module_name;
        Utils.SSet.iter (fun req ->
          let pos = extract_position req req_locs in
          let file = Relative_path.to_absolute Pos.(pos.pos_file) in
          let l0, c0, l1, c1 = Errors_js.pos_range pos in
          Printf.printf "\t%s@%s:%d:%d,%d:%d\n" req file l0 c0 l1 c1
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

let run () = main (parse_args ())
