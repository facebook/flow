(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

class context_cache : object
  method find: Loc.FilenameKey.t -> Context.t option
  method read: Loc.FilenameKey.t -> Context.t
end

class sig_context_cache : object
  method find: Loc.FilenameKey.t -> Context.t option
  method read: Loc.FilenameKey.t -> Context.t * Context.t
end

val add: Context.t -> unit
val add_sig: Context.t -> unit
val remove_batch: Utils_js.FilenameSet.t -> unit
val remove_sig_batch: Utils_js.FilenameSet.t -> unit
