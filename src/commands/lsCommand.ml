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
(* flow ls (list files) command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec = {
  CommandSpec.
  name = "ls";
  doc = "Lists files visible to Flow";
  usage = Printf.sprintf
    "Usage: %s ls [OPTION]... [FILE]...\n\n\
      Lists files visible to Flow\n"
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> strip_root_flag
    |> ignore_flag
    |> include_flag
    |> root_flag
    |> json_flags
    |> flag "--all" no_arg
      ~doc:"Even list ignored files and lib files"
    |> flag "--explain" no_arg
      ~doc:"Output what kind of file each file is and why Flow cares about it"
    |> anon "files or dirs" (list_of string)
      ~doc:"Lists only these files or files in these directories"
  )
}

type file_result =
  | ImplicitlyIncluded
  | ExplicitlyIncluded
  | ImplicitlyIgnored
  | ExplicitlyIgnored
  | ImplicitLib
  | ExplicitLib

let string_of_file_result = function
| ImplicitlyIncluded -> "ImplicitlyIncluded"
| ExplicitlyIncluded -> "ExplicitlyIncluded"
| ImplicitlyIgnored -> "ImplicitlyIgnored"
| ExplicitlyIgnored -> "ExplicitlyIgnored"
| ImplicitLib -> "ImplicitLib"
| ExplicitLib -> "ExplicitLib"

let string_of_file_result_with_padding = function
| ImplicitlyIncluded -> "ImplicitlyIncluded"
| ExplicitlyIncluded -> "ExplicitlyIncluded"
| ImplicitlyIgnored  -> "ImplicitlyIgnored "
| ExplicitlyIgnored  -> "ExplicitlyIgnored "
| ImplicitLib        -> "ImplicitLib       "
| ExplicitLib        -> "ExplicitLib       "

let is_included ~root ~options ~libs raw_file =
  let file = raw_file |> Path.make |> Path.to_string in
  let root_str = Path.to_string root in
  let result =
    if SSet.mem file libs
    then begin
      (* This is a lib file *)
      let flowtyped_path = Files.get_flowtyped_path root in
      if String_utils.string_starts_with file (Path.to_string flowtyped_path)
      then ImplicitLib
      else ExplicitLib
    end else if Files.is_ignored options file
    then ExplicitlyIgnored
    else if Files.is_included options file
    then ExplicitlyIncluded
    else if String_utils.string_starts_with file root_str
    then ImplicitlyIncluded
    else ImplicitlyIgnored
  in (raw_file, result)

let json_of_files_with_explanations files =
  let open Hh_json in
  let properties = List.map
    (fun (file,res) -> file, JSON_Object [
      "explanation", JSON_String (string_of_file_result res);
    ])
    files in
  JSON_Object properties

let rec iter_get_next ~f get_next =
  match get_next () with
  | [] -> ()
  | result ->
      List.iter f result;
      iter_get_next ~f get_next

let make_options ~root ~ignore_flag ~include_flag =
  let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
  let flowconfig_flags = { CommandUtils.
    ignores = CommandUtils.list_of_string_arg ignore_flag;
    includes = CommandUtils.list_of_string_arg include_flag;
    libs = [];
  } in
  let temp_dir = FlowConfig.temp_dir flowconfig in
  CommandUtils.file_options ~root ~no_flowlib:true ~temp_dir ~lib:None flowconfig_flags flowconfig

(* Directories will return a closure that returns every file under that
   directory. Individual files will return a closure that returns just that file
 *)
let get_ls_files ~root ~all ~options ~libs = function
| None ->
    Files.make_next_files ~root ~all ~subdir:None ~options ~libs
| Some dir when try Sys.is_directory dir with _ -> false ->
    let subdir = Some (Path.make dir) in
    Files.make_next_files ~root ~all ~subdir ~options ~libs
| Some file ->
    if all || (Sys.file_exists file && Files.wanted ~options libs file)
    then begin
      let file = file |> Path.make |> Path.to_string in
      let rec cb = ref begin fun () ->
        cb := begin fun () -> [] end;
        [file]
      end in
      fun () -> !cb ()
    end else fun () -> []

(* We have a list of get_next() functions. This combines them into a single
   get_next function *)
let concat_get_next get_nexts =
  let get_nexts = ref get_nexts in

  let rec concat () =
    match !get_nexts with
    | [] -> []
    | get_next::rest ->
        (match get_next () with
        | [] ->
            get_nexts := rest;
            concat ()
        | ret -> ret)

  in concat

let main
  strip_root ignore_flag include_flag root_flag json pretty all reason
  root_or_files () =

  let root = guess_root (
    match root_flag with
    | Some root -> Some root
    | None -> (match root_or_files with
      | Some (first_file::_) -> Some first_file
      | _ -> None)
  ) in

  let options = make_options ~root ~ignore_flag ~include_flag in
  let _, libs = Files.init options in
  (* `flow ls` and `flow ls dir` will list out all the flow files *)
  let next_files = (match root_or_files with
  | None
  | Some [] ->
      get_ls_files ~root ~all ~options ~libs None
  | Some files_or_dirs ->
      files_or_dirs
      |> List.map (fun f -> get_ls_files ~root ~all ~options ~libs (Some f))
      |> concat_get_next) in

  let root_str = spf "%s%s" (Path.to_string root) Filename.dir_sep in
  let normalize_filename filename =
    if not strip_root then filename
    else Files.relative_path root_str filename
  in

  if json || pretty
  then Hh_json.(begin
    let files = Files.get_all next_files |> SSet.elements in
    let json =
      if reason
      then
        files
        |> List.map normalize_filename
        |> List.map (is_included ~root ~options ~libs)
        |> json_of_files_with_explanations
      else JSON_Array (
        List.map (fun f -> JSON_String (normalize_filename f)) files
      ) in
    json_to_string ~pretty json |> print_endline
  end) else begin
    let f = if reason
    then begin fun filename ->
      let f, r = is_included ~root ~options ~libs filename in
      Printf.printf
        "%s    %s\n%!"
        (string_of_file_result_with_padding r)
        (normalize_filename f)
    end else begin fun filename ->
      Printf.printf "%s\n%!" (normalize_filename filename)
    end in

    iter_get_next ~f next_files
  end

let command = CommandSpec.command spec main
