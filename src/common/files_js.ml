(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

(************** file filter utils ***************)

open Utils

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
  Find.make_next_files_js ~filter ~others root

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
