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
  Context.t list ->
  Context.t
val merge_contents_context:
  options: Options.t ->
  Context.t ->
  Loc.t SMap.t ->
  ensure_checked_dependencies: (Module_js.NameSet.t -> unit) ->
  unit
val merge_strict:
  intermediate_result_callback: ((filename * Errors.ErrorSet.t) list Lazy.t -> unit) ->
  options: Options.t ->
  workers: Worker.t list option ->
  FilenameSet.t FilenameMap.t ->
  (filename list) FilenameMap.t ->
  bool FilenameMap.t ->
  (filename * Errors.ErrorSet.t) list
