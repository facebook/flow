(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

class sig_context_cache : object
  method find: Loc.FilenameKey.t -> Context.t option
  method read: (options: Options.t -> Loc.FilenameKey.t -> Context.t * Context.t) Expensive.t
  method read_safe:
    (options: Options.t -> Loc.FilenameKey.t -> (Context.t * Context.t) option) Expensive.t
end

val add_sig: (Context.t -> unit) Expensive.t

val find_leader: Loc.FilenameKey.t -> Loc.FilenameKey.t

val add_merge_on_diff: (Context.t -> Loc.filename list -> SigHash.t -> bool) Expensive.t
val oldify_merge_batch: Utils_js.FilenameSet.t -> unit
val revive_merge_batch: Utils_js.FilenameSet.t -> unit
val remove_old_merge_batch: Utils_js.FilenameSet.t -> unit
