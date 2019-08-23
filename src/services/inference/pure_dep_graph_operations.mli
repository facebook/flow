(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Contains pure functions which perform calculations on the dependency graph *)

type dependency_graph = Utils_js.FilenameSet.t Utils_js.FilenameMap.t

val calc_direct_dependencies:
  Dependency_info.t ->
  Utils_js.FilenameSet.t ->
  Utils_js.FilenameSet.t

val calc_all_dependencies:
  Dependency_info.t ->
  Utils_js.FilenameSet.t ->
  Utils_js.FilenameSet.t

val calc_all_dependents:
  Dependency_info.t ->
  Utils_js.FilenameSet.t ->
  Utils_js.FilenameSet.t

val filter_dependency_graph:
  dependency_graph ->
  Utils_js.FilenameSet.t -> (* files *)
  dependency_graph
