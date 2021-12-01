(*
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
        |> connect_flags
        |> root_flag
        |> from_flag
        |> flag
             "--focus"
             no_arg
             ~doc:"If the server is running in lazy mode, force it to focus on these files"
        |> flag
             "--input-file"
             string
             ~doc:
               ("File containing list of files to recheck, one per line. If -, list of files is "
               ^ "read from the standard input."
               )
        |> anon "files" (list_of string)
      );
  }

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

let main base_flags connect_flags root focus input_file files () =
  begin
    match (input_file, files) with
    | (None, (None | Some [])) ->
      CommandSpec.usage spec;
      let msg = "FILES may be omitted if and only if --input-file is used" in
      Exit.(exit ~msg Commandline_usage_error)
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

  let files = Base.List.map ~f:get_path_of_file files in
  let request = ServerProt.Request.FORCE_RECHECK { files; focus } in
  let () =
    match connect_and_make_request flowconfig_name connect_flags root request with
    | ServerProt.Response.FORCE_RECHECK -> ()
    | response -> failwith_bad_response ~request ~response
  in
  Exit.(exit No_error)

let command = CommandSpec.command spec main
