(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val of_classic_map : Utils_js.FilenameSet.t Utils_js.FilenameMap.t -> t

val of_types_first_map :
  (Utils_js.FilenameSet.t * Utils_js.FilenameSet.t) Utils_js.FilenameMap.t -> t

val update : t -> Partial_dependency_graph.t -> Utils_js.FilenameSet.t -> t

val implementation_dependency_graph : t -> Utils_js.FilenameGraph.t

val sig_dependency_graph : t -> Utils_js.FilenameGraph.t

val map_filenames : (File_key.t -> File_key.t) -> t -> t

val is_types_first : t -> bool

val debug_to_string : t -> string
