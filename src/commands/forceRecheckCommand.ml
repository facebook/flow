(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow force-recheck *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "force-recheck";
    doc = "Forces the server to recheck a given list of files";
    usage =
      Printf.sprintf
        "Usage: %s force-recheck [OPTION]... [FILES]\nForces the Flow server to recheck a given list of files.\n\nFILES may be omitted if and only if --input-file is used.\n"
        exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_and_json_flags
        |> root_flag
        |> from_flag
        |> profile_flag
        |> flag
             "--focus"
             no_arg
             ~doc:"If the server is running in lazy mode, force it to focus on these files"
        |> flag
             "--input-file"
             string
             ~doc:
               ( "File containing list of files to recheck, one per line. If -, list of files is "
               ^ "read from the standard input." )
        |> anon "files" (list_of string));
  }

type json =
  | JSON
  | Pretty

type args = {
  root: Path.t;
  files: string list;
  focus: bool;
  profile: bool;
  json: json option;
}

let force_recheck flowconfig_name (args : args) connect_flags =
  let files = Core_list.map ~f:get_path_of_file args.files in
  let request =
    ServerProt.Request.FORCE_RECHECK { files; focus = args.focus; profile = args.profile }
  in
  let profiling =
    match connect_and_make_request flowconfig_name connect_flags args.root request with
    | ServerProt.Response.FORCE_RECHECK profiling -> profiling
    | response -> failwith_bad_response ~request ~response
  in
  (* Print profiling info *)
  begin
    if args.json = None then
      Option.iter ~f:Profiling_js.print_summary profiling
    else
      let properties = Option.value_map ~default:[] ~f:Profiling_js.to_json_properties profiling in
      Hh_json.(print_json_endline ~pretty:(args.json = Some Pretty) (JSON_Object properties))
  end;

  FlowExitStatus.(exit No_error)

let rec find_parent_that_exists path =
  if Sys.file_exists path then
    path
  else
    let newpath = Filename.dirname path in
    (* dirname called repeatedly should eventually return ".", which should
     * always exist. But no harm in being overly cautious. Let's detect
     * infinite recursion *)
    if newpath = path then
      path
    else
      find_parent_that_exists newpath

let main base_flags connect_flags json pretty root profile focus input_file files () =
  begin
    match (input_file, files) with
    | (None, (None | Some [])) ->
      CommandSpec.usage spec;
      let msg = "FILES may be omitted if and only if --input-file is used" in
      FlowExitStatus.(exit ~msg Commandline_usage_error)
    | _ -> ()
  end;

  let files = get_filenames_from_input ~allow_imaginary:true input_file files in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root =
    guess_root
      flowconfig_name
      (match (root, files) with
      | (Some root, _) -> Some root
      | (None, file :: _) -> Some (find_parent_that_exists file)
      | (None, []) -> None)
  in
  let json =
    if pretty then
      Some Pretty
    else if json then
      Some JSON
    else
      None
  in
  let args = { root; files; focus; profile; json } in
  force_recheck flowconfig_name args connect_flags

let command = CommandSpec.command spec main
