(**
 * Copyright (c) 2014, Facebook, Inc.
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
  make (Printf.sprintf "%s/%s" path more)

let parent path =
  if is_directory path
  then make (path ^ "/../")
  else make (Filename.dirname path)

let output = output_string

let slash_escaped_string_of_path path =
  let buf = Buffer.create (String.length path) in
  String.iter (fun ch ->
    match ch with
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
