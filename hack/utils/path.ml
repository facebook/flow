(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


type t = {
  is_normalized: bool;
  path: string;
}

let dummy_path: t = { is_normalized = true; path = ""; }

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
let make (path : string) : t =
  match Sys_utils.realpath (expanduser path) with
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

let to_string (path : t) : string =
  path.path

let equal (path1 : t) (path2 : t) : bool =
  path1.is_normalized = path2.is_normalized &&
  path1.path = path2.path

let file_exists (path : t) : bool =
  let file = to_string path in
  Sys.file_exists file

let is_directory (path : t) : bool =
  let file = to_string path in
  Sys.is_directory file

let is_normalized (path : t) : bool =
  path.is_normalized

let concat (path : t) (more : string) : t =
  let path = to_string path in
  let path = Printf.sprintf "%s/%s" path more in
  make path

let remove (path : t) : unit =
  let file = to_string path in
  Sys.remove file

let parent (path : t) : t =
  let s = to_string path in
  if is_directory path
  then make (s ^ "/../")
  else make (Filename.dirname s)

let slash_escaped_string_of_path (path: t) : string =
  let path_str = to_string path in
  let buf = Buffer.create (String.length path_str) in
  String.iter (fun ch ->
    match ch with
    | '/' -> Buffer.add_string buf "zS"
    | '\x00' -> Buffer.add_string buf "z0"
    | 'z' -> Buffer.add_string buf "zZ"
    | _ -> Buffer.add_char buf ch
  ) path_str;
  Buffer.contents buf

let path_of_slash_escaped_string (str: string) : t =
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
