(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type file_watcher =
  | NoFileWatcher
  | DFind
  | Watchman

type status' =
  | Initializing
  | Ready
  | Deferred of { reason: string }

type status = file_watcher * status'

let string_of_file_watcher = function
  | NoFileWatcher -> "Dummy"
  | DFind -> "Dfind"
  | Watchman -> "Watchman"

let string_of_status =
  let string_of_status = function
    | Initializing -> "still initializing"
    | Ready -> "ready"
    | Deferred { reason } -> Printf.sprintf "deferred (%s)" reason
  in
  fun (watcher, status) ->
    Printf.sprintf "%s file watcher is %s" (string_of_file_watcher watcher) (string_of_status status)
