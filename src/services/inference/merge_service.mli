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
  options: Options.t ->
  Context_cache.context_cache ->
  Context.t list ->
  unit
val merge_strict:
  options: Options.t ->
  workers: Worker.t list option ->
  FilenameSet.t FilenameMap.t ->
  filename list list IMap.t ->
  bool FilenameMap.t ->
  (filename * Errors.ErrorSet.t) list
