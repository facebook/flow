(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

type t = {
  (* list of paths to match against. may contain wildcards.
     NOTE: stored in reverse-insertion order! *)
  paths: Path.t list;
  (* stems extracted from paths.
     NOTE: stored in reverse-insertion order! *)
  stems: Path.t list;
  (* map from stems to list of (original path, regexified path) *)
  stem_map: ((string * Str.regexp) list) PathMap.t;
}

let empty = { paths = []; stems = []; stem_map = PathMap.empty; }
let paths matcher = matcher.paths
let stems matcher = matcher.stems
let stem_map matcher = matcher.stem_map

(* given a path, return the max prefix not containing a wildcard
   or a terminal filename.
 *)
let path_stem =
  let wc = Str.regexp "^[^*?]*[*?]" in
  (fun path ->
    (* strip filename *)
    let path = if Path.file_exists path && not (Path.is_directory path)
      then Path.parent path else path in
    let path_str = Path.to_string path in
    (* strip back to non-wc prefix *)
    let stem = if Str.string_match wc path_str 0
      then Filename.dirname (Str.matched_string path_str)
    else path_str in
    Path.make stem)

(* translate a path with wildcards into a regex *)
let path_patt =
  let star = Str.regexp_string "*" in
  let star2 = Str.regexp_string "**" in
  let qmark = Str.regexp_string "?" in
  fun path ->
    let str = Path.to_string path 
      |> Sys_utils.normalize_filename_dir_sep in
    (* because we accept both * and **, convert in 2 steps *)
    let results = Str.full_split star2 str in
    let results = List.map (fun r -> match r with
      | Str.Text s ->
          (* note: unix directory seperators specifiers only. Windows directory
           * seperators will already have been normalized to unix directory
           * seperators *)
          let s = Str.global_replace star "[^/]*" s in
          Str.global_replace qmark "." s
      | Str.Delim _ -> ".*") results in
    let str = String.concat "" results in
    Str.regexp str

(* helper - eliminate noncanonical entries where possible.
   no other normalization is done *)
let dir_sep = Str.regexp_string Filename.dir_sep
let fixup_path p =
  let s = Path.to_string p in
  let is_normalized = match Sys_utils.realpath s with
      | Some s' -> s' = s
      | None -> false in
  if is_normalized then p else
  let abs = not (Filename.is_relative s) in
  let entries = Str.split_delim dir_sep s in
  let rec loop revbase entries =
    match entries with
    | h :: t when h = Filename.current_dir_name ->
        loop revbase t
    | h :: t when h = Filename.parent_dir_name -> (
        match revbase with
        | _ :: rt -> loop rt t
        | _ -> loop (h :: revbase) t
      )
    | h :: t -> loop (h :: revbase) t
    | [] -> List.rev revbase
  in
  let entries = loop [] entries in
  let s = List.fold_left Filename.concat "" entries in
  let s = if abs then Filename.dir_sep ^ s else s in
  Path.make s

(* adds `path` to the matcher, calculating the appropriate stem and pattern *)
let add { paths; stems; stem_map; } path =
  let path = fixup_path path in
  let stem = path_stem path in
  let patt = path_patt path in
  let pstr = Path.to_string path in
  let stems, stem_map =
    match PathMap.get stem stem_map with
    | None ->
        let stem_map = PathMap.add stem [pstr, patt] stem_map in
        (stem :: stems), stem_map
    | Some entries ->
        let stem_map = PathMap.add stem ((pstr, patt) :: entries) stem_map in
        stems, stem_map
  in
  { paths = path::paths; stems; stem_map; }

(* filters a list of prefixes into only the prefixes with which f starts *)
let find_prefixes f = List.filter (fun prefix ->
  String_utils.string_starts_with f (Path.to_string prefix)
)

(* find a match for f in a list of patterns, or none *)
let rec match_patt f = function
  | [] -> None
  | (path, patt) :: _ when Str.string_match patt f 0 -> Some path
  | (_, _) :: t -> match_patt f t

let matches path_matcher f =
  let matching_stems = find_prefixes f path_matcher.stems in
  let normalized_f = Sys_utils.normalize_filename_dir_sep f in
  List.exists (fun stem ->
    let patts = PathMap.find_unsafe stem path_matcher.stem_map in
    match_patt normalized_f patts != None
  ) matching_stems
