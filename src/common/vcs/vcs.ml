(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Hg
  | Git

let find : ?recursion_limit:int -> Path.t -> t option =
  let rec walk ~recursion_limit dir =
    if dir = Path.parent dir then
      (* Reached fs root *)
      None
    else if Path.file_exists (Path.concat dir ".hg") then
      Some Hg
    else if Path.file_exists (Path.concat dir ".git") then
      Some Git
    else if recursion_limit <= 0 then
      None
    else
      walk ~recursion_limit:(recursion_limit - 1) (Path.parent dir)
  in
  (fun ?(recursion_limit = 100) dir -> walk ~recursion_limit dir)
