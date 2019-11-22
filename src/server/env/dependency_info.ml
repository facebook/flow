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

let of_classic_map map = Classic map

let of_types_first_map map = TypesFirst map

let update_map old_map updated_map files_to_remove =
  old_map |> FilenameSet.fold FilenameMap.remove files_to_remove |> FilenameMap.union updated_map

let update old_dep_info partial_dep_info files_to_remove =
  match (old_dep_info, partial_dep_info) with
  | (Classic old_map, Classic updated_map) ->
    Classic (update_map old_map updated_map files_to_remove)
  | (TypesFirst old_map, TypesFirst updated_map) ->
    TypesFirst (update_map old_map updated_map files_to_remove)
  | _ -> assert false

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
