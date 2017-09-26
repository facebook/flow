(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val add_sig: (Context.t -> unit) Expensive.t
val find_sig: options:Options.t -> File_key.t -> Context.t

val find_leader: File_key.t -> File_key.t

val add_merge_on_diff: (Context.t -> File_key.t list -> SigHash.t -> bool) Expensive.t
val oldify_merge_batch: Utils_js.FilenameSet.t -> unit
val revive_merge_batch: Utils_js.FilenameSet.t -> unit
val remove_old_merge_batch: Utils_js.FilenameSet.t -> unit
