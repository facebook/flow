(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module PathMap : Flow_map.S with type key = File_path.t = Flow_map.Make (struct
  type t = File_path.t

  let compare p1 p2 = String.compare (File_path.to_string p1) (File_path.to_string p2)
end)

type t = {
  (* stems extracted from paths.
     NOTE: stored in reverse-insertion order! *)
  stems: File_path.t list;
  (* map from stems to list of (original path, regexified path) *)
  stem_map: (string * Str.regexp) list PathMap.t;
}

let empty = { stems = []; stem_map = PathMap.empty }

let stems matcher = matcher.stems

(* given a path, return the max prefix not containing a wildcard
   or a terminal filename.
*)
let path_stem =
  let wc = Str.regexp "^[^*?]*[*?]" in
  fun path ->
    (* strip filename *)
    let path =
      if File_path.file_exists path && not (File_path.is_directory path) then
        File_path.parent path
      else
        path
    in
    let path_str = File_path.to_string path in
    (* strip back to non-wc prefix *)
    let stem =
      if Str.string_match wc path_str 0 then
        Filename.dirname (Str.matched_string path_str)
      else
        path_str
    in
    File_path.make stem

(* translate a path with wildcards into a regex *)
let path_patt =
  let star = Str.regexp_string "*" in
  let star2 = Str.regexp_string "**" in
  let qmark = Str.regexp_string "?" in
  fun path ->
    let str = File_path.to_string path |> Sys_utils.normalize_filename_dir_sep in
    (* because we accept both * and **, convert in 2 steps *)
    let results = Str.full_split star2 str in
    let results =
      Base.List.map
        ~f:(fun r ->
          match r with
          | Str.Text s ->
            (* note: unix directory separators specifiers only. Windows directory
             * separators will already have been normalized to unix directory
             * separators *)
            let s = Str.global_replace star "[^/]*" s in
            Str.global_replace qmark "." s
          | Str.Delim _ -> ".*")
        results
    in
    let str = String.concat "" results in
    Str.regexp str

let dir_sep =
  if Sys.win32 then
    Str.regexp "[/\\\\]"
  else
    Str.regexp_string Filename.dir_sep

(* helper - eliminate noncanonical entries where possible.
   no other normalization is done *)
let fixup_path p =
  let s = File_path.to_string p in
  let is_normalized =
    match Sys_utils.realpath s with
    | Some s' -> s' = s
    | None -> false
  in
  if is_normalized then
    p
  else
    let (root, entries) =
      match Str.split_delim dir_sep s with
      | "" :: entries when not Sys.win32 ->
        (* "/foo" -> ("/", ["foo"]) *)
        (Filename.dir_sep, entries)
      | root :: entries when Sys.win32 && String.length root = 2 && root.[1] = ':' ->
        (* "C:\\foo" -> ("C:\\", ["foo"]) *)
        (root ^ Filename.dir_sep, entries)
      | entries ->
        (* relative *)
        ("", entries)
    in
    let rec loop revbase entries =
      match entries with
      | h :: t when h = Filename.current_dir_name -> loop revbase t
      | h :: t when h = Filename.parent_dir_name ->
        (match revbase with
        | _ :: rt -> loop rt t
        | _ -> loop (h :: revbase) t)
      | h :: t -> loop (h :: revbase) t
      | [] -> List.rev revbase
    in
    let entries = loop [] entries in
    let s = List.fold_left Filename.concat root entries in
    File_path.make s

(* adds `path` to the matcher, calculating the appropriate stem and pattern *)
let add { stems; stem_map } path =
  let path = fixup_path path in
  let stem = path_stem path in
  let patt = path_patt path in
  let pstr = File_path.to_string path in
  let (stems, stem_map) =
    match PathMap.find_opt stem stem_map with
    | None ->
      let stem_map = PathMap.add stem [(pstr, patt)] stem_map in
      (stem :: stems, stem_map)
    | Some entries ->
      let stem_map = PathMap.add stem ((pstr, patt) :: entries) stem_map in
      (stems, stem_map)
  in
  { stems; stem_map }

(* filters a list of prefixes into only the prefixes with which f starts *)
let find_prefixes f =
  List.filter (fun prefix -> String.starts_with ~prefix:(File_path.to_string prefix) f)

(* find a match for f in a list of patterns, or none *)
let rec match_patt f = function
  | [] -> None
  | (path, patt) :: _ when Str.string_match patt f 0 -> Some path
  | (_, _) :: t -> match_patt f t

let matches path_matcher f =
  let matching_stems = find_prefixes f path_matcher.stems in
  let normalized_f = Sys_utils.normalize_filename_dir_sep f in
  List.exists
    (fun stem ->
      let patts = PathMap.find stem path_matcher.stem_map in
      match_patt normalized_f patts != None)
    matching_stems
