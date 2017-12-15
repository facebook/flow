(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val add_sig: (Context.t -> unit) Expensive.t
val find_sig: options:Options.t -> File_key.t -> Context.t

val find_leader: File_key.t -> File_key.t

val add_merge_on_diff: (Context.t -> File_key.t list -> Xx.hash -> bool) Expensive.t
val oldify_merge_batch: Utils_js.FilenameSet.t -> unit
val revive_merge_batch: Utils_js.FilenameSet.t -> unit
val remove_old_merge_batch: Utils_js.FilenameSet.t -> unit
