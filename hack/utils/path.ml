(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


type path = {
  is_normalized: bool;
  path: string;
}

external realpath: string -> string option = "hh_realpath"

let dummy_path: path = { is_normalized = true; path = ""; }

(**
 * Like Python's os.path.expanduser, though probably doesn't cover some cases.
 * Roughly follow's bash's tilde expansion:
 * http://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html
 *
 * ~/foo -> /home/bob/foo if $HOME = "/home/bob"
 * ~joe/foo -> /home/joe/foo if joe's home is /home/joe
 *)
let expanduser (path : string) : string =
  Str.substitute_first
    (Str.regexp "^~\\([^/]*\\)")
    begin fun s ->
      match Str.matched_group 1 s with
        | "" ->
          begin try Unix.getenv "HOME"
          with Not_found -> (Unix.getpwuid (Unix.getuid())).Unix.pw_dir end
        | unixname ->
          try (Unix.getpwnam unixname).Unix.pw_dir
          with Not_found -> Str.matched_string s end
    path

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
let mk_path (path : string) : path =
  match realpath (expanduser path) with
  | Some path ->
    {
      is_normalized=true;
      path=path;
    }
  | None ->
    {
      is_normalized=false;
      path=path;
    }

let string_of_path (path : path) : string =
  path.path

let equal (path1 : path) (path2 : path) : bool =
  path1.is_normalized = path2.is_normalized &&
  path1.path = path2.path

let file_exists (path : path) : bool =
  let file = string_of_path path in
  Sys.file_exists file

let is_directory (path : path) : bool =
  let file = string_of_path path in
  Sys.is_directory file

let concat (path : path) (more : string) : path =
  let path = string_of_path path in
  let path = Printf.sprintf "%s/%s" path more in
  mk_path path

let remove (path : path) : unit =
  let file = string_of_path path in
  Sys.remove file

let parent (path : path) : path =
  let s = string_of_path path in
  if is_directory path
  then mk_path (s ^ "/../")
  else mk_path (Filename.dirname s)

let slash_escaped_string_of_path (path: path) : string =
  let path_str = string_of_path path in
  let buf = Buffer.create (String.length path_str) in
  String.iter (fun ch ->
    match ch with
    | '/' -> Buffer.add_string buf "zS"
    | '\x00' -> Buffer.add_string buf "z0"
    | 'z' -> Buffer.add_string buf "zZ"
    | _ -> Buffer.add_char buf ch
  ) path_str;
  Buffer.contents buf

let path_of_slash_escaped_string (str: string) : path =
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
  mk_path (Buffer.contents buf)
