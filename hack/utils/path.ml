(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

include Sys

type t = string

let dummy_path : t = ""

let cat = Sys_utils.cat
let compare = Pervasives.compare
let dirname = Filename.dirname
let expanduser = Sys_utils.expanduser
let null_path = if Sys.win32 then "nul" else "/dev/null"
let temp_dir_name =
  if Sys.win32 then Filename.get_temp_dir_name () else "/tmp"

(**
 * Resolves a path (using realpath)
 *
 * The advantage of using a path instead of strings is that you
 * don't need to care about symlinks or trailing slashes: each
 * path gets normalized by calling realpath.
 *
 * A few things to keep in mind:
 * - paths are always absolute. So the empty string "" becomes
 *   the current directory (in absolute)
 *)
let make path =
  match Sys_utils.realpath (expanduser path) with
  | Some path -> path
  | None -> path (* assert false? *)

let to_string path = path

let concat path more =
  make (Filename.concat path more)

let parent path =
  if is_directory path
  then make (concat path Filename.parent_dir_name)
  else make (Filename.dirname path)

let output = output_string

let slash_escaped_string_of_path path =
  let buf = Buffer.create (String.length path) in
  String.iter (fun ch ->
    match ch with
    | '\\' -> Buffer.add_string buf "zB"
    | ':' -> Buffer.add_string buf "zC"
    | '/' -> Buffer.add_string buf "zS"
    | '\x00' -> Buffer.add_string buf "z0"
    | 'z' -> Buffer.add_string buf "zZ"
    | _ -> Buffer.add_char buf ch
  ) path;
  Buffer.contents buf

let path_of_slash_escaped_string str =
  let length = String.length str in
  let buf = Buffer.create length in
  let rec consume i =
    if i >= length then ()
    else
      let replacement =
        if i < length - 1 && str.[i] = 'z'
        then match str.[i+1] with
          | 'B' -> Some '\\'
          | 'C' -> Some ':'
          | 'S' -> Some '/'
          | '0' -> Some '\x00'
          | 'Z' -> Some 'z'
          | _ -> None
        else None in
      let c, next_i = match replacement with
      | Some r -> r, i+2
      | None -> str.[i], i+1 in
      Buffer.add_char buf c;
      consume next_i
  in consume 0;
  make (Buffer.contents buf)

exception IntBreakEarly of int
let is_parent_of =
  let dir_sep = Filename.dir_sep in
  let dir_sep_re = Str.regexp_string dir_sep in
  let split_on_dir_sep = Str.split_delim dir_sep_re in
  fun child parent ->
    if child.[0] <> dir_sep.[0] || parent.[0] <> dir_sep.[0] then (
      invalid_arg "Can only compare path ancestry of two absolute paths!"
    );

    if child = parent then 
      false 
    else if (String.length child) < (String.length parent) then 
      false
    else (
      let child_elems = split_on_dir_sep child in
      let child_elems_len = List.length child_elems in

      let parent_elems = split_on_dir_sep parent in
      let parent_elems_len = List.length parent_elems in

      if child_elems_len < parent_elems_len then 
        false
      else (
        let (common_ancestors, _) = 
          try 
            List.fold_left (fun (common_ancestors, child_elems) p ->
              let (c, cs) = match child_elems with
                | c::cs -> (c, cs)
                (* Should never happen because we compare child_elems and
                 * parent_elems cardinality above *)
                | [] -> failwith "Path.is_parent_of: This should never happen!"
              in

              if c = p
              then (1 + common_ancestors, cs)
              else raise (IntBreakEarly common_ancestors)
            ) (0, child_elems) parent_elems
          with IntBreakEarly common_ancestors -> (common_ancestors, [])
        in
        common_ancestors = parent_elems_len
      )
    )
