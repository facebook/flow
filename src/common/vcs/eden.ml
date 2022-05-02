(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let is_eden_posix dir =
  (* Eden on Mac and Linux exposes a .eden folder in every directory, containing
     a `root` file that symlinks to the eden root.

     There is also a .eden folder containing global state that is not an eden
     root, whose path is determined by the `edenDirectory` config in
     `/etc/eden/edenfs.rc`. It defaults to `~/.eden`, so a naive implementation
     would think everything in your home dir is in an eden mount! This folder
     does not have a `root` file. *)
  let open Path in
  file_exists (concat (concat dir ".eden") "root")

let rec is_eden_win32 ~recursion_limit dir =
  (* Eden on Windows does not expose a .eden folder in every directory -- only
     at the root. This folder contains a file named `config`.

     Inside an eden redirection, you're not in an eden mount but walking the
     parent dirs would cross back into the eden mount and return true
     incorrectly. Redirections on Windows are symlinks, so resolving the
     parent dir symlink (as Path.parent does) will jump out of the eden mount
     and into wherever redirections' data lives, eventually returning false.

     There is also a .eden folder containing global state that is not an eden
     root, whose path is determined by the `edenDirectory` config. It defaults
     to `C:\users\USER\.eden`, so a naive implementation would think everything
     in your home dir is in an eden mount! This folder has `config.json` and
     `config.toml`, but not an extensionless `config`. :shrug: *)
  let open Path in
  let parent_dir = parent dir in
  if dir = parent_dir then
    (* Reached fs root *)
    false
  else if file_exists (concat (concat dir ".eden") "config") then
    true
  else if file_exists (concat dir ".hg") || file_exists (concat dir ".git") then
    (* small optimization. if we hit a different VCS, definitely not eden. *)
    false
  else if recursion_limit <= 0 then
    false
  else
    is_eden_win32 ~recursion_limit:(recursion_limit - 1) parent_dir

let is_eden : Path.t -> bool =
  if Sys.win32 then
    is_eden_win32 ~recursion_limit:100
  else
    is_eden_posix
