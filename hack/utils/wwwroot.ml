(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(**
 * Checks if x is a www directory by looking for ".hhconfig".
 *)
let is_www_directory ?(config = ".hhconfig") (path : Path.t) : bool =
  let arcconfig = Path.concat path config in
  Path.file_exists arcconfig

let assert_www_directory ?(config = ".hhconfig") (path : Path.t) : unit =
  if not (Path.file_exists path && Path.is_directory path) then (
    Printf.eprintf "Error: %s is not a directory\n%!" (Path.to_string path);
    exit 1
  );
  if not (is_www_directory ~config path) then (
    Printf.fprintf
      stderr
      "Error: could not find a %s file in %s or any of its parent directories. Do you have a %s in your code's root directory?\n"
      config
      (Path.to_string path)
      config;
    flush stderr;
    exit 1
  )

let rec guess_root config start ~recursion_limit : Path.t option =
  if start = Path.parent start then
    None
  (* Reached file system root *)
  else if is_www_directory ~config start then
    Some start
  else if recursion_limit <= 0 then
    None
  else
    guess_root config (Path.parent start) (recursion_limit - 1)

let get ?(config = ".hhconfig") (path : string option) : Path.t =
  let start_str =
    match path with
    | None -> "."
    | Some s -> s
  in
  let start_path = Path.make start_str in
  let root =
    match guess_root config start_path ~recursion_limit:50 with
    | None -> start_path
    | Some r -> r
  in
  assert_www_directory ~config root;
  root
