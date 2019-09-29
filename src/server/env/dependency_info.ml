(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type t =
  | Classic of FilenameSet.t FilenameMap.t
  | TypesFirst of (FilenameSet.t * FilenameSet.t) FilenameMap.t

let all_dependency_graph = function
  | Classic map -> map
  | TypesFirst map -> FilenameMap.map (fun (_sig_files, all_files) -> all_files) map

let dependency_graph = function
  | Classic map -> map
  | TypesFirst map -> FilenameMap.map (fun (sig_files, _all_files) -> sig_files) map
