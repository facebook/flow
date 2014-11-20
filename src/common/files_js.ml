(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(************** file filter utils ***************)

open Utils

let flow_extensions = [
    ".js"  ;      (* Standard JavaScript files *)
    ".jsx" ;      (* JavaScript files with JSX *)
  ]

let is_directory path = try Sys.is_directory path with Sys_error _ -> false

let is_dot_file path =
  let filename = Filename.basename path in
  String.length filename > 0 && filename.[0] = '.'

let is_flow_file path =
  not (is_dot_file path) &&
  List.exists (Filename.check_suffix path) flow_extensions &&
  not (is_directory path)

let read_dir dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun file -> Filename.check_suffix file ".js")
  |> List.map (fun file -> Filename.concat dir file)

let lib_files = ref []

let flowlib_root = ref None

let get_flowlib_root () =
  match !flowlib_root with
  | Some root -> root
  | None ->
      let root = match Flowlib.get_flowlib_root () with
      | None ->
          print_endline "Could not locate hhi files"; exit 1
      | Some root -> Path.string_of_path root in
      flowlib_root := Some root;
      root

let init = function
  | None ->
      lib_files := read_dir (get_flowlib_root () )
  | Some path ->
      lib_files := (read_dir (get_flowlib_root ())) @
        let path = Path.(path |> mk_path |> string_of_path) in
        if Sys.is_directory path
        then (read_dir path)
        else [path]

let is_lib_file p =
  List.mem p ((get_flowlib_root ()) :: !lib_files)

let lib_module = ""

let dir_sep = Str.regexp_string Filename.dir_sep
let current_dir_name = Str.regexp_string Filename.current_dir_name
let parent_dir_name = Str.regexp_string Filename.parent_dir_name

let match_regexp s r =
  Str.string_match r s 0

let wanted config =
  let list = List.map snd config.FlowConfig.excludes in
  fun file -> not (List.exists (match_regexp file) list)

let make_next_files root =
  let config = FlowConfig.get root in
  let filter = wanted config in
  let others = config.FlowConfig.includes in
  Find.make_next_files_with_find
    (fun p -> is_flow_file p && filter p) ~others root

let rec normalize_path dir file =
  normalize_path_ dir (Str.split_delim dir_sep file)

and normalize_path_ dir names =
  match names with
  | ""::names ->
      List.fold_left Filename.concat "" names
  | dot::names when dot = Filename.current_dir_name ->
      normalize_path_ dir names
  | dots::names when dots = Filename.parent_dir_name ->
      normalize_path_ (Filename.dirname dir) names
  | _ ->
      List.fold_left Filename.concat dir names
