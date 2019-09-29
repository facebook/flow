(*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Core_kernel

(* This allows one to fake having multiple files in one file. This
 * is used only in unit test files.
 * Indeed, there are some features that require mutliple files to be tested.
 * For example, newtype has a different meaning depending on the file.
 *)
let rec make_files = function
  | [] -> []
  | Str.Delim header :: Str.Text content :: rl ->
    let pattern = Str.regexp "////" in
    let header = Str.global_replace pattern "" header in
    let pattern = Str.regexp "[ ]*" in
    let filename = Str.global_replace pattern "" header in
    (filename, content) :: make_files rl
  | _ -> assert false

(* We have some hacky "syntax extensions" to have one file contain multiple
 * files, which can be located at arbitrary paths. This is useful e.g. for
 * testing lint rules, some of which activate only on certain paths. It's also
 * useful for testing abstract types, since the abstraction is enforced at the
 * file boundary.
 * Takes the path to a single file, returns a map of filenames to file contents.
 *)
let file_to_files file =
  let abs_fn = Relative_path.to_absolute file in
  let content = Sys_utils.cat abs_fn in
  let delim = Str.regexp "////.*\n" in
  if Str.string_match delim content 0 then
    let contentl = Str.full_split delim content in
    let files = make_files contentl in
    List.fold_left
      ~f:
        begin
          fun acc (sub_fn, content) ->
          let file =
            Relative_path.create Relative_path.Dummy (abs_fn ^ "--" ^ sub_fn)
          in
          Relative_path.Map.add acc ~key:file ~data:content
        end
      ~init:Relative_path.Map.empty
      files
  else if String.is_prefix content ~prefix:"// @directory " then (
    let contentl = Str.split (Str.regexp "\n") content in
    let first_line = List.hd_exn contentl in
    let regexp =
      Str.regexp "^// @directory *\\([^ ]*\\) *\\(@file *\\([^ ]*\\)*\\)?"
    in
    let has_match = Str.string_match regexp first_line 0 in
    assert has_match;
    let dir = Str.matched_group 1 first_line in
    let file_name =
      (try Str.matched_group 3 first_line with Caml.Not_found -> abs_fn)
    in
    let file = Relative_path.create Relative_path.Dummy (dir ^ file_name) in
    let content = String.concat ~sep:"\n" (List.tl_exn contentl) in
    Relative_path.Map.singleton file content
  ) else
    Relative_path.Map.singleton file content
