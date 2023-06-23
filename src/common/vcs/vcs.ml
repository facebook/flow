(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Hg
  | Git

let find_root : ?recursion_limit:int -> File_path.t -> (t * File_path.t) option =
  let rec walk ~recursion_limit dir =
    if dir = File_path.parent dir then
      (* Reached fs root *)
      None
    else if File_path.file_exists (File_path.concat dir ".hg") then
      Some (Hg, dir)
    else if File_path.file_exists (File_path.concat dir ".git") then
      Some (Git, dir)
    else if recursion_limit <= 0 then
      None
    else
      walk ~recursion_limit:(recursion_limit - 1) (File_path.parent dir)
  in
  (fun ?(recursion_limit = 100) dir -> walk ~recursion_limit dir)

let find ?recursion_limit dir =
  match find_root ?recursion_limit dir with
  | Some (t, _) -> Some t
  | None -> None

let name = function
  | Git -> "Git"
  | Hg -> "Mercurial"
