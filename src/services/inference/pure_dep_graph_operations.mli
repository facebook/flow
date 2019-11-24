(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Contains pure functions which perform calculations on the dependency graph *)

val calc_direct_dependencies :
  Utils_js.FilenameGraph.t -> Utils_js.FilenameSet.t -> Utils_js.FilenameSet.t

val calc_all_dependencies :
  Utils_js.FilenameGraph.t -> Utils_js.FilenameSet.t -> Utils_js.FilenameSet.t

val calc_all_dependents :
  sig_dependency_graph:Utils_js.FilenameGraph.t ->
  implementation_dependency_graph:Utils_js.FilenameGraph.t ->
  Utils_js.FilenameSet.t ->
  (* sig dependents, all dependents *)
  Utils_js.FilenameSet.t * Utils_js.FilenameSet.t

val filter_dependency_graph :
  Utils_js.FilenameGraph.t ->
  (* files *)
  Utils_js.FilenameSet.t ->
  Utils_js.FilenameSet.t Utils_js.FilenameMap.t
