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

let implementation_dependency_graph = function
  | Classic map -> map
  | TypesFirst map -> FilenameMap.map (fun (_sig_files, all_files) -> all_files) map

let sig_dependency_graph = function
  | Classic map -> map
  | TypesFirst map -> FilenameMap.map (fun (sig_files, _all_files) -> sig_files) map

let debug_to_string = function
  | Classic map ->
    spf "Classic:\n%s" (debug_string_of_filename_map debug_string_of_filename_set map)
  | TypesFirst map ->
    spf
      "TypesFirst:\n%s"
      (debug_string_of_filename_map
         (fun (sig_files, all_files) ->
           spf
             "Sig: %s, All: %s"
             (debug_string_of_filename_set sig_files)
             (debug_string_of_filename_set all_files))
         map)
