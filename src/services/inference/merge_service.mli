(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

val merge_strict_context:
  options: Options.options ->
  Context_cache.context_cache ->
  Context.t list ->
  unit
val merge_strict:
  options: Options.options ->
  workers: Worker.t list option ->
  save_errors: (filename list -> Errors_js.ErrorSet.t list -> unit) ->
  FilenameSet.t FilenameMap.t ->
  filename list list IMap.t ->
  unit

val remove_batch:
  FilenameSet.t ->
  unit
