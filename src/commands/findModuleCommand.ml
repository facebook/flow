(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow find-module (get filename of module) command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "find-module";
    doc = "Resolves a module reference to a file";
    usage =
      Printf.sprintf
        "Usage: %s find-module [OPTION]... [FILE]...\n\nResolves a module reference to a file\n\nExample usage:\n\t%s find-module moduleref filename\n"
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
        |> flag
             "--debug-show-considered-candidates"
             truthy
             ~doc:"Print all the candidates considered during module resolution to stderr."
        |> anon "module" (required string)
        |> anon "file" (required string)
      );
  }

let main
    base_flags
    option_values
    json
    pretty
    root
    strip_root
    wait_for_recheck
    show_considered_candidates
    moduleref
    filename
    () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root =
    guess_root
      flowconfig_name
      (match root with
      | Some root -> Some root
      | None -> Some filename)
  in
  let request = ServerProt.Request.FIND_MODULE { moduleref; filename; wait_for_recheck } in
  let (resolution_result, failed_candidates) =
    match connect_and_make_request flowconfig_name option_values root request with
    | ServerProt.Response.FIND_MODULE
        ( ( Some (File_key.LibFile file)
          | Some (File_key.SourceFile file)
          | Some (File_key.JsonFile file)
          | Some (File_key.ResourceFile file) ),
          failed_candidates
        ) ->
      if strip_root then
        (Files.relative_path (File_path.to_string root) file, failed_candidates)
      else
        (file, failed_candidates)
    | ServerProt.Response.FIND_MODULE (None, failed_candidates) -> ("(unknown)", failed_candidates)
    | response -> failwith_bad_response ~request ~response
  in
  if show_considered_candidates then (
    Printf.eprintf "The following candidates are considered during module resolution:\n";
    List.iter (Printf.eprintf " - %s\n") failed_candidates
  );
  if json || pretty then
    Hh_json.(
      let json = JSON_Object [("file", JSON_String resolution_result)] in
      print_json_endline ~pretty json
    )
  else
    Printf.printf "%s\n%!" resolution_result

let command = CommandSpec.command spec main
