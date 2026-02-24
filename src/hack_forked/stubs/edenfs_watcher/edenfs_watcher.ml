(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type clock = string [@@deriving show, eq]

type instance_handle_ffi

type instance = { handle: instance_handle_ffi }

type edenfs_watcher_error = Edenfs_watcher_types.edenfs_watcher_error [@@deriving show]

type changes = Edenfs_watcher_types.changes [@@deriving show]

let init (_settings : Edenfs_watcher_types.settings) = failwith "not implemented"

let get_changes_async (_instance : instance) :
    (changes list * clock * Hh_json.json option, edenfs_watcher_error) result =
  failwith "not implemented"

let get_notification_fd (_instance : instance) : (Unix.file_descr, edenfs_watcher_error) result =
  failwith "not implemented"

let is_available () = false

let watch_spec _options = failwith "not implemented"

let hooks_upon_clean_exit : (unit -> unit) list ref = ref []
