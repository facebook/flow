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

val dependent_files:
  Worker.t list option -> (* workers *)
  unchanged:FilenameSet.t ->
  new_or_changed:FilenameSet.t ->
  changed_modules:Modulename.Set.t ->
  (* (transitive_dependents, direct_dependents) of changed_modules *)
  FilenameSet.t * FilenameSet.t

val file_dependencies: (filename -> FilenameSet.t) Expensive.t

val calc_dependency_graph:
  Worker.t list option -> (* workers *)
  filename list -> (* files *)
  FilenameSet.t FilenameMap.t

val calc_all_dependencies:
  FilenameSet.t FilenameMap.t -> (* dependency graph *)
  FilenameSet.t -> (* files *)
  FilenameSet.t
