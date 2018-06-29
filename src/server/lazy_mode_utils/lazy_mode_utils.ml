(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open Utils_js

let focus_and_check genv env filenames =
  let filenames = SSet.of_list (Nel.to_list filenames) in
  let focused = Rechecker.process_updates genv env filenames in
  let checked_files = CheckedSet.add ~focused env.checked_files in

  let new_focused_files = focused
    |> Fn.flip FilenameSet.diff (CheckedSet.focused env.checked_files)
    |> Fn.flip FilenameSet.diff (CheckedSet.dependents env.checked_files) in

  let env = { env with checked_files } in
  if not (FilenameSet.is_empty new_focused_files)
  then
    (* Rechecking will send errors to the clients *)
    let%lwt _summary, env = Rechecker.recheck genv env new_focused_files in
    Lwt.return (env, true)
  else
    Lwt.return (env, false)
