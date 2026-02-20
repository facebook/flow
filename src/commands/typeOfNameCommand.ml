(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow type-of-name command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec =
  {
    CommandSpec.name = "type-of-name-experimental";
    doc = "";
    usage =
      Printf.sprintf
        "Usage: %s type-of-name-experimental [OPTION]... FILE NAME\n\ne.g. %s type-of-name foo.js myVariable\n"
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_flags
        |> root_flag
        |> strip_root_flag
        |> verbose_flags
        |> from_flag
        |> path_flag
        |> wait_for_recheck_flag
        |> flag
             "--expand-component-props"
             truthy
             ~doc:"[DEPRECATED: now default] Expand rest props of components"
        |> flag
             "--unexpand-component-props"
             truthy
             ~doc:"Show spread types without expanding (e.g., ...UserCardProps)"
        |> flag "--hide-references" truthy ~doc:"Hide definition information of names within types"
        |> flag "--exact-match-only" truthy ~doc:"Only show results that match the name exactly"
        |> anon "args" (required (list_of string))
      );
  }

let handle_response ~strip_root ~hide_references ~query_name response =
  let {
    ServerProt.Response.InferTypeOfName.loc;
    actual_name;
    type_;
    refs;
    documentation;
    prop_docs;
    source;
  } =
    response
  in

  let match_exactness =
    if actual_name = query_name then
      spf "exact match '%s'" actual_name
    else
      spf "approximate match '%s' (instead of '%s')" actual_name query_name
  in
  let range =
    let open Export_index in
    match source with
    | Global -> " defined as a library definition (no need to import)"
    | Builtin s -> spf "defined in module `%s`" (Flow_import_specifier.show_userland s)
    | File_key file ->
      if loc = Loc.none then
        spf " defined at %s" (File_key.to_string file)
      else
        spf " defined at %s" (Reason.range_string_of_loc ~strip_root loc)
  in
  let str_of_loc loc =
    match loc.Loc.source with
    | Some _file -> Some (Reason.string_of_loc ~strip_root loc)
    | None -> None
  in
  let type_str =
    if Base.String.contains type_ '\n' then
      spf "\n```\n%s\n```" type_
    else
      spf " `%s` " type_
  in
  let refs =
    if hide_references then
      ""
    else
      match refs with
      | None -> ""
      | Some refs ->
        let refs =
          Base.List.concat_map refs ~f:(fun (name, loc) ->
              match str_of_loc loc with
              | Some s -> [Utils_js.spf "'%s' is defined at %s" name s]
              | None -> []
          )
        in
        (match refs with
        | [] -> ""
        | _ -> "\nwhere\n" ^ String.concat "\n" refs ^ "\n")
  in
  let doc =
    match documentation with
    | Some doc -> spf "\nand documentation:\n%s\n" doc
    | None -> ""
  in
  let prop_docs_str =
    match prop_docs with
    | None -> ""
    | Some docs ->
      let lines =
        Base.List.map docs ~f:(fun { ServerProt.Response.InferTypeOfName.prop_name; description } ->
            spf "  %s - %s" prop_name description
        )
      in
      (match lines with
      | [] -> ""
      | _ -> "\nProp documentation:\n" ^ String.concat "\n" lines ^ "\n")
  in
  print_endline
    (spf "Found %s%s with type%s%s%s%s" match_exactness range type_str refs doc prop_docs_str)

let handle_error err =
  prerr_endline err;
  Exit.(exit Type_error)

let main
    base_flags
    option_values
    root
    strip_root
    verbose
    path
    wait_for_recheck
    _expand_component_props_deprecated
    unexpanded
    hide_references
    exact_match_only
    args
    () =
  let (file, name) =
    match args with
    | [file; name] -> (file, name)
    | _ ->
      CommandSpec.usage spec;
      Exit.exit ~msg:"Expected exactly two arguments: FILE and NAME" Exit.Commandline_usage_error
  in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let file_input = get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name) path (Some file) in
  let root = find_a_root ~base_flags ~input:file_input root in
  let strip_root =
    if strip_root then
      Some root
    else
      None
  in
  if verbose <> None then prerr_endline "NOTE: --verbose writes to the server log file";
  (* Default is now expanded (true), --unexpanded sets to false *)
  let expand_component_props = not unexpanded in
  let options =
    {
      ServerProt.Type_of_name_options.input = file_input;
      name;
      verbose;
      wait_for_recheck;
      expand_component_props;
      exact_match_only;
      strip_root;
    }
  in
  let request = ServerProt.Request.TYPE_OF_NAME options in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.TYPE_OF_NAME (Error err) -> handle_error err
  | ServerProt.Response.TYPE_OF_NAME (Ok resp) ->
    handle_response ~strip_root ~hide_references ~query_name:name resp
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
