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
(* flow get importers command *)
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
    "Usage: %s get-importers [OPTION]... [FILE]...\n\n\
    Gets a list of all importers for one or more given modules\n\n\
    Example usage:\n\
    \t%s get-importers FirstModule SecondModule"
    Sys.argv.(0)
    Sys.argv.(0) in
  let modules =
    ClientArgs.parse_without_command options usage "get-importers" in
  match modules with
  | [] ->
      Printf.fprintf stderr "You must provide at least one module name\n%!";
      Arg.usage options usage;
      exit 2
  | _ -> ();
  { modules; option_values; }

module Json = Hh_json

let main { modules; option_values; } =
  let root = guess_root (Some (Sys.getcwd ())) in

  let ic, oc = connect_with_autostart option_values root in

  ServerProt.cmd_to_channel oc (ServerProt.GET_IMPORTERS modules);
  let importers_map = Marshal.from_channel ic in
  if !(option_values.json)
  then (
    let json_list =
      Utils.SMap.fold (fun module_name importers json_list ->
        let importer_list = List.map (fun entry ->
          Json.JString entry
        ) (Utils.SSet.elements importers) in
        (module_name, Json.JList importer_list) :: json_list
      ) importers_map [] in
    let json_output = Json.JAssoc json_list in
    output_string stdout (Json.json_to_string json_output);
    flush stdout
  ) else (
    List.iter (fun module_name ->
      if (Utils.SMap.mem module_name importers_map)
      then begin
        let import_files =
          Utils.SMap.find_unsafe module_name importers_map in
        Printf.printf "Modules importing module '%s':\n" module_name;
        Utils.SSet.iter (fun file ->
          Printf.printf "\t%s\n" file
        ) import_files
      end else
        Printf.printf "Module '%s' could not be found!\n" module_name
    ) modules;
    flush stdout
  )

let run () = main (parse_args ())
