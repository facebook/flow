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
(* flow find-module (get filename of module) command *)
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
    "Usage: %s find-module [OPTION]... [FILE]...\n\n\
    Shows filenames for one or more modules\n\n\
    Example usage:\n\
    \t%s find-module FirstModule SecondModule"
    Sys.argv.(0)
    Sys.argv.(0) in
  let modules = ClientArgs.parse_without_command options usage "find-module" in
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

  ServerProt.cmd_to_channel oc (ServerProt.FIND_MODULES modules);
  let response = Marshal.from_channel ic in
  if !(option_values.json)
  then (
    let json_entries =
      Utils.SMap.fold (fun key path json_list ->
          (key, Json.JString path) :: json_list
        ) response [] in
    let json_output = Json.JAssoc json_entries in
    output_string stdout (Json.json_to_string json_output);
    flush stdout
  ) else (
    List.iter (fun module_name ->
        match Utils.SMap.get module_name response with
        | Some filename ->
            Printf.printf "Module '%s' is located at '%s'\n"
              module_name filename
        | None ->
            Printf.printf "Module '%s' does not exist\n" module_name
      ) modules;
    flush stdout
  )

let run () = main (parse_args ())
