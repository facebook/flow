(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = string

let dummy_path : t = ""

let file_exists = Sys.file_exists

let is_directory = Sys.is_directory

let cat = Sys_utils.cat

(** [is_ancestor ~prefix path] determines if [prefix] is an ancestor directory of [path] *)
let is_ancestor ~prefix path =
  let prefix =
    if String.ends_with prefix ~suffix:Filename.dir_sep then
      prefix
    else
      prefix ^ Filename.dir_sep
  in
  String.starts_with ~prefix path

let compare = String.compare

let dirname = Filename.dirname

let basename = Filename.basename

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
  match Sys_utils.realpath path with
  | Some path -> path
  | None -> (* assert false? *) path

(**
 * Creates a Path without running it through `realpath`. This is unsafe because
 * it doesn't normalize symlinks, trailing slashes, or relative paths. The path
 * you pass here must be absolute, and free of symlinks (including ../).
 *)
let make_unsafe path = path

let to_string path = path

let concat path more = make (Filename.concat path more)

let parent path =
  (* Sys.is_directory raises Sys_error on non-existent paths. We catch it and
     treat missing paths as non-directories, so that callers like Vcs.find_root
     don't crash when given paths to deleted files (e.g. files reported as
     changed by Mercurial that no longer exist on disk). *)
  let is_dir =
    try Sys.is_directory path with
    | Sys_error _ -> false
  in
  if is_dir then
    make (concat path Filename.parent_dir_name)
  else
    make (Filename.dirname path)

let output = output_string
