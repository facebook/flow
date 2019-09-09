(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open Utils_js

let focus_and_check genv env filenames =
  let filenames = SSet.of_list (Nel.to_list filenames) in
  let focused = Rechecker.process_updates ~options:genv.ServerEnv.options env filenames in
  let files_to_focus =
    focused
    |> Fn.flip FilenameSet.diff (CheckedSet.focused env.checked_files)
    |> Fn.flip FilenameSet.diff (CheckedSet.dependents env.checked_files)
  in
  let files_to_force = CheckedSet.add ~focused:files_to_focus CheckedSet.empty in
  match%lwt Rechecker.recheck_single ~files_to_force genv env with
  | Error env -> Lwt.return (env, false)
  | Ok (_summary, env) -> Lwt.return (env, true)
