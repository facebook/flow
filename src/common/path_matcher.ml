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
    let str = Path.to_string path in
    (* because we accept both * and **, convert in 2 steps *)
    let results = Str.full_split star2 str in
    let results = List.map (fun r -> match r with
      | Str.Text s ->
          (* note: unix path specifiers only *)
          let s = Str.global_replace star "[^/]*" s in
          Str.global_replace qmark "." s
      | Str.Delim _ -> ".*") results in
    let str = String.concat "" results in
    Str.regexp str

(* adds `path` to the matcher, calculating the appropriate stem and pattern *)
let add { paths; stems; stem_map; } path =
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

(* find a prefix for f in a list of paths, or none *)
let rec find_prefix f = function
  | [] -> None
  | h :: _ when Utils.str_starts_with f (Path.to_string h) -> Some h
  | _ :: t -> find_prefix f t

(* find a match for f in a list of patterns, or none *)
let rec match_patt f = function
  | [] -> None
  | (path, patt) :: _ when Str.string_match patt f 0 -> Some path
  | (_, _) :: t -> match_patt f t

let matches path_matcher f =
  match find_prefix f path_matcher.stems with
  | None -> false
  | Some stem ->
      let patts = PathMap.find_unsafe stem path_matcher.stem_map in
      match_patt f patts != None
