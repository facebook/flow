(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type status' =
  | Initializing
  | Ready

type status = Options.file_watcher * status'

let string_of_file_watcher = function
  | Options.NoFileWatcher -> "Dummy"
  | Options.DFind -> "Dfind"
  | Options.Watchman -> "Watchman"

let string_of_status =
  let string_of_status = function
    | Initializing -> "still initializing"
    | Ready -> "ready"
  in
  fun (watcher, status) ->
    Printf.sprintf
      "%s file watcher is %s"
      (string_of_file_watcher watcher)
      (string_of_status status)
