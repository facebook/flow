(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow get imports command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "get-imports";
    doc = "Get names of all modules imported by one or more given modules";
    usage =
      Printf.sprintf
        "Usage: %s get-requirements [OPTION]... [FILE]...\n\nGet names of all modules imported by one or more given modules\n\nExample usage:\n\t%s get-imports FirstModule SecondModule\n"
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_and_json_flags
        |> root_flag
        |> strip_root_flag
        |> from_flag
        |> wait_for_recheck_flag
        |> anon "modules" (required (list_of string))
      );
  }

let main base_flags option_values json pretty root strip_root wait_for_recheck modules () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root = guess_root flowconfig_name root in
  let request = ServerProt.Request.GET_IMPORTS { module_names = modules; wait_for_recheck } in
  let (requirements_map, non_flow) =
    match connect_and_make_request flowconfig_name option_values root request with
    | ServerProt.Response.GET_IMPORTS response -> response
    | response -> failwith_bad_response ~request ~response
  in
  let requirements_map =
    SMap.fold
      begin
        fun module_name reqlocs map ->
        let requirements =
          Modulename.Map.fold
            (fun req loc assoc ->
              let req =
                match req with
                | Modulename.String s -> s
                | Modulename.Filename f ->
                  let f = File_key.to_string f in
                  if strip_root then
                    Files.relative_path (Path.to_string root) f
                  else
                    f
              in
              (req, loc) :: assoc)
            reqlocs
            []
        in
        SMap.add module_name requirements map
      end
      requirements_map
      SMap.empty
  in
  let strip_root =
    if strip_root then
      Some root
    else
      None
  in
  if json || pretty then
    Hh_json.(
      let json_non_flow =
        SSet.fold
          (fun module_name acc ->
            let json =
              JSON_Object [("not_flow", JSON_Bool true); ("requirements", JSON_Array [])]
            in
            (module_name, json) :: acc)
          non_flow
          []
      in
      let json_imports =
        SMap.fold
          (fun module_name assoc acc ->
            let requirements =
              List.fold_left
                (fun acc (req, locs) ->
                  Nel.fold_left
                    (fun acc loc ->
                      JSON_Object
                        (("import", JSON_String req)
                         ::
                         ("loc", json_of_loc_with_offset ~strip_root loc)
                         :: Errors.deprecated_json_props_of_loc ~strip_root loc
                        )
                      :: acc)
                    acc
                    locs)
                []
                assoc
            in
            let json =
              JSON_Object [("not_flow", JSON_Bool false); ("requirements", JSON_Array requirements)]
            in
            (module_name, json) :: acc)
          requirements_map
          []
      in
      let json = JSON_Object (List.append json_non_flow json_imports) in
      print_json_endline ~pretty json
    )
  else
    let print_imports module_name =
      if SMap.mem module_name requirements_map then (
        let requirements = SMap.find module_name requirements_map in
        Printf.printf "Imports for module '%s':\n" module_name;
        List.iter
          (fun (req, locs) ->
            Nel.iter
              (fun loc ->
                let loc_str = range_string_of_loc ~strip_root loc in
                Printf.printf "\t%s@%s\n" req loc_str)
              locs)
          requirements
      ) else if SSet.mem module_name non_flow then
        Printf.printf
          "Cannot obtain imports for module '%s' because is not\ marked for processing by flow!\n"
          module_name
      else
        Printf.printf "Module '%s' could not be found!\n" module_name
    in
    List.iter print_imports modules;
    flush stdout

let command = CommandSpec.command spec main
