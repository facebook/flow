(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Disk_sig.Types
module Hashtbl_base = Hashtbl

module Hashtbl = struct
  include Hashtbl_base

  let find_opt t x =
    try Some (find t x) with
    | Not_found -> None

  let empty t = length t = 0
end

type file =
  | Actual_file_with_contents of string
  | Directory of (string, file) Hashtbl.t

module Helpers = struct
  exception Relative_parent_not_supported

  exception Is_not_actual_file

  exception Cannot_overwrite_existing_directory_with_actual_file
end

(** Directory for "/" *)
let root = Hashtbl.create 10

let mtimes = Hashtbl.create 10

(** We avoid using Unix.getcwd () in TestDisk because:
  *   1) Getting global state from this clean test environment is gross
  *   2) Because of 1, CWD should actually be tracked inside this virtual FS.
  *   3) Javascript doesn't support Unix.getcwd anyway, so we can't transpile
  * As such, we make a fake cwd *)
let cwd = ref "/fake/initial_cwd"

let getcwd () = !cwd

let chdir s = cwd := s

let rec mkdir_p path root =
  match Filename.dirname path with
  | "." -> mkdir_p (getcwd ()) root
  | "/" -> root
  | parent ->
    let parent = mkdir_p parent root in
    if Filename.basename path = "." then
      parent
    else if Filename.basename path = ".." then
      raise Helpers.Relative_parent_not_supported
    else (
      match Hashtbl.find_opt parent (Filename.basename path) with
      | None ->
        let dir = Hashtbl.create 10 in
        let () = Hashtbl.add parent (Filename.basename path) (Directory dir) in
        dir
      | Some (Directory table) -> table
      | Some (Actual_file_with_contents _) -> raise (NotADirectory path)
    )

and get_dir path root =
  match Filename.dirname path with
  | "." -> get_dir (getcwd ()) root
  | "/" -> root
  | parent ->
    let parent = get_dir parent root in
    if Filename.basename path = "." then
      parent
    else if Filename.basename path = ".." then
      raise Helpers.Relative_parent_not_supported
    else (
      match Hashtbl.find_opt parent (Filename.basename path) with
      | None -> raise (No_such_file_or_directory path)
      | Some (Directory table) -> table
      | Some (Actual_file_with_contents _) -> raise (NotADirectory path)
    )

(** Returns file at path (may be an actual file or a directory). *)
and get_file path root =
  let parent = get_dir (Filename.dirname path) root in
  let basename = Filename.basename path in
  if basename = "." then
    Directory parent
  else
    try Hashtbl.find parent basename with
    | Not_found -> raise (No_such_file_or_directory path)

(** Initialize creation of CWD. *)
let () = ignore (mkdir_p "." root)

let get x =
  match get_file x root with
  | Actual_file_with_contents contents -> contents
  | Directory _ -> raise Helpers.Is_not_actual_file

(** Set the contents "y" for file "x". Has an option to create all parent
 * directories automatically. *)
let set ?(create_parent_dirs = true) x y =
  let parent =
    if create_parent_dirs then
      mkdir_p (Filename.dirname x) root
    else
      get_dir (Filename.dirname x) root
  in
  match Hashtbl.find_opt parent (Filename.basename x) with
  | None ->
    let new_file = Actual_file_with_contents y in
    Hashtbl.add parent (Filename.basename x) new_file
  | Some (Actual_file_with_contents _) ->
    let new_file = Actual_file_with_contents y in
    Hashtbl.replace parent (Filename.basename x) new_file
  | Some (Directory _) -> raise Helpers.Cannot_overwrite_existing_directory_with_actual_file

let is_directory x =
  try
    match get_file x root with
    | Directory _ -> true
    | Actual_file_with_contents _ -> false
  with
  | No_such_file_or_directory _ -> false

let cat = get

let file_exists x =
  try
    match get_file x root with
    | Actual_file_with_contents _
    | Directory _ ->
      true
  with
  | No_such_file_or_directory _ -> false

let write_file ~file ~contents = set ~create_parent_dirs:false file contents

let mkdir path _perm =
  let parent = get_dir (Filename.dirname path) root in
  ignore (mkdir_p (Filename.basename path) parent)

let mkdir_p path = ignore (mkdir_p path root)

let rm_dir_tree path =
  if path = "/" then
    Hashtbl.clear root
  else
    try
      let dir = get_dir (Filename.dirname path) root in
      Hashtbl.remove dir (Filename.basename path)
    with
    | No_such_file_or_directory _ ->
      (* File already doesn't exist; ignore. *)
      ()

let readdir x =
  match get_file x root with
  | Actual_file_with_contents _ -> raise (NotADirectory x)
  | Directory directory ->
    let names = Hashtbl.fold (fun k _v acc -> k :: acc) directory [] in
    Array.of_list names

let rename old target =
  if not (file_exists old) then
    raise (No_such_file_or_directory old)
  else if not (file_exists (Filename.dirname target)) then
    raise (No_such_file_or_directory (Filename.dirname target))
  else
    let old_parent = get_dir (Filename.dirname old) root in
    let old_file = get_file old root in
    (* What if the last character in target is a "/"? What to do? *)
    let target_parent = get_dir (Filename.dirname target) root in
    match (old_file, Hashtbl.find_opt target_parent (Filename.basename target)) with
    | (Directory _, Some (Directory target_files)) when not (Hashtbl.empty target_files) ->
      raise (Rename_target_dir_not_empty target)
    | (Directory _, Some (Directory _))
    | (_, None) ->
      (* Rename one directory to the other. *)
      Hashtbl.replace target_parent (Filename.basename target) old_file;
      Hashtbl.remove old_parent (Filename.basename old)
    | (_, _) -> failwith "Not sure what to do here"

let filemtime (file : string) : float =
  match Hashtbl.find_opt mtimes file with
  | None -> 0.0
  | Some mtime -> mtime
