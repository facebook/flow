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
open Modes_js

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

(* This is initialized early, before we work all the workers, so that each
 * worker isn't forced to find all the lib files themselves *)
let lib_files = ref None

let get_lib_files () = match !lib_files with
| None -> SSet.empty
| Some files -> files

let flowlib_root = ref None

let get_flowlib_root () =
  match !flowlib_root with
  | Some root -> root
  | None ->
      let root = match Flowlib.get_flowlib_root () with
      | None ->
          print_endline "Could not locate hhi files"; exit 1
      | Some root -> Path.to_string root in
      flowlib_root := Some root;
      root

let init libs =
  match !lib_files with
  | Some libs -> ()
  | None -> (
    let libs = if Modes_js.modes.no_flowlib
      then libs
      else (Path.make (get_flowlib_root ()))::libs
    in
    let libs = if libs = []
      then SSet.empty
      else (Find.find_with_name libs "*.js")
        |> List.fold_left (fun set x -> SSet.add x set) SSet.empty
    in
    lib_files := Some libs
  )

let is_lib_file p =
  SSet.mem p (get_lib_files ())

let is_lib_file_or_flowlib_root p =
  p = get_flowlib_root () || is_lib_file p

let lib_module = ""

let dir_sep = Str.regexp_string Filename.dir_sep
let current_dir_name = Str.regexp_string Filename.current_dir_name
let parent_dir_name = Str.regexp_string Filename.parent_dir_name

let match_regexp s r =
  Str.string_match r s 0

let wanted config =
  let list = List.map snd config.FlowConfig.excludes in
  fun file ->
    not (List.exists (match_regexp file) list) &&
      not (SSet.mem file (get_lib_files ()))

let make_next_files root =
  let config = FlowConfig.get root in
  let filter = wanted config in
  let others = config.FlowConfig.include_stems in
  let sroot = Path.to_string root in
  Find.make_next_files_with_find (fun p ->
    (str_starts_with p sroot || FlowConfig.is_included config p)
    && is_flow_file p
    && filter p
  ) ~others root

let rec normalize_path dir file =
  normalize_path_ dir (Str.split_delim dir_sep file)

and normalize_path_ dir names =
  match names with
  | dot::names when dot = Filename.current_dir_name ->
      (* ./<names> => dir/names *)
      normalize_path_ dir names
  | dots::names when dots = Filename.parent_dir_name ->
      (* ../<names> => parent(dir)/<names> *)
      normalize_path_ (Filename.dirname dir) names
  | ""::names when names <> [] ->
      (* /<names> => /<names> *)
      construct_path Filename.dir_sep names
  | _ ->
      (* <names> => dir/<names> *)
      construct_path dir names

and construct_path = List.fold_left Filename.concat

(* return a list of all file paths ending in 'package.json'
   under root or includes *)
let package_json root =
  let config = FlowConfig.get root in
  let sroot = Path.to_string root in
  let want = wanted config in
  let filt = fun p -> want p &&
    (str_starts_with p sroot || FlowConfig.is_included config p) in
  let paths = root :: config.FlowConfig.include_stems in
  List.filter filt (Find.find_with_name paths "package.json")
