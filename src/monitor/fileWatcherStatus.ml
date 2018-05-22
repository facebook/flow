(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type file_watcher =
| NoFileWatcher
| DFind

type status' =
| Initializing
| Ready
type status = file_watcher * status'

let string_of_status =
  let string_of_watcher = function
  | NoFileWatcher -> "Dummy file watcher"
  | DFind -> "Dfind file watcher"
  in

  let string_of_status = function
  | Initializing -> "still initializing"
  | Ready -> "ready"
  in

  fun (watcher, status) ->
    Printf.sprintf "%s is %s" (string_of_watcher watcher) (string_of_status status)
