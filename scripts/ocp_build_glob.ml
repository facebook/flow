(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

#use "utils.ml"

module SSet = Set.Make (struct
  type t = string
  let compare (x: t) (y: t) = String.compare x y
  let to_string x = x
end)

let glob_whitespace = "[ \n]*"
let glob_str = "\"[^\"]*\""
let glob_list = Printf.sprintf
  "\\[\\(%s*%s%s*\\)*\\]"
  glob_whitespace
  glob_str
  glob_whitespace

let glob = Printf.sprintf
  "begin fb-glob %s%s\\(%s\\)%s\\(excludes%s=%s\\(%s\\)%s\\)?%send"
  glob_whitespace
  glob_whitespace
  glob_str
  glob_whitespace
  glob_whitespace
  glob_whitespace
  glob_list
  glob_whitespace
  glob_whitespace

let glob_str_regex = Str.regexp glob_str
let glob_regex = Str.regexp glob

(* Remove the quotes *)
let process_str str =
  let str = String.trim str in
  String.sub str 1 (String.length str - 2)

let process_list str =
  let excludes_ref = ref SSet.empty in
  let start = ref 0 in
  try
    while !start < String.length str do
      start := 1 + Str.search_forward glob_str_regex str !start;
      let exclude = Str.matched_string str in
      excludes_ref := SSet.add (process_str exclude) !excludes_ref
    done;
    !excludes_ref
  with Not_found -> !excludes_ref

let extensions = [".ml"; ".mll"; ".mly"; ".c"]
let filter excludes filename =
  List.exists (fun ext -> Filename.check_suffix filename ext) extensions &&
    not (SSet.mem filename excludes)

let process_glob str =
  assert (Str.string_match glob_regex str 0);
  let dir =
    Str.matched_group 1 str
    |> process_str in
  let excludes = try
    Str.matched_group 3 str
      |> process_list;
  with Not_found -> SSet.empty in

  Sys.readdir (String.trim dir)
    |> Array.to_list
    |> List.filter (filter excludes)
    |> List.map (Printf.sprintf "%s/%s" dir)
    |> List.map (Printf.sprintf "    \"%s\"")
    |> String.concat "\n"
    |> Printf.sprintf "[\n%s\n  ]"

let process_split_result = function
| Str.Text text -> text
| Str.Delim glob -> process_glob glob

let () =
  let in_file = Sys.argv.(1) in
  let out_file = Sys.argv.(2) in

  let before = string_of_file in_file in

  let after = Str.full_split glob_regex before
    |> List.map process_split_result
    |> String.concat "" in

  with_out_channel out_file (fun oc -> output_string oc after)
