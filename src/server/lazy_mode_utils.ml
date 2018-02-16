(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open Utils_js

let focus_and_check genv env filenames =
  let options = genv.options in
  let file_options = Options.file_options options in
  let focused = Nel.fold_left
    (fun acc fn ->
      FilenameSet.add (Files.filename_from_string ~options:file_options fn) acc)
    FilenameSet.empty
    filenames in
  let checked_files = CheckedSet.add ~focused env.checked_files in

  let new_focused_files = focused
    |> Fn.flip FilenameSet.diff (CheckedSet.focused env.checked_files)
    |> Fn.flip FilenameSet.diff (CheckedSet.dependents env.checked_files) in

  let env = { env with checked_files } in
  if not (FilenameSet.is_empty new_focused_files)
  then
    (* Rechecking will send errors to the clients *)
    let _profiling, env = Rechecker.recheck genv env new_focused_files in
    env, true
  else
    env, false
