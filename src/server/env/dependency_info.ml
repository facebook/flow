(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type t =
  | Classic of FilenameSet.t FilenameMap.t
  | TypesFirst of {
      sig_dependency_graph: FilenameSet.t FilenameMap.t;
      implementation_dependency_graph: FilenameSet.t FilenameMap.t;
    }

let of_classic_map map = Classic map

let of_types_first_map map =
  let sig_dependency_graph = FilenameMap.map (fun (sig_deps, _impl_deps) -> sig_deps) map in
  let implementation_dependency_graph =
    FilenameMap.map (fun (_sig_deps, impl_deps) -> impl_deps) map
  in
  TypesFirst { sig_dependency_graph; implementation_dependency_graph }

let update_map old_map updated_map files_to_remove =
  old_map |> FilenameSet.fold FilenameMap.remove files_to_remove |> FilenameMap.union updated_map

let update old_dep_info partial_dep_info files_to_remove =
  match (old_dep_info, partial_dep_info) with
  | (Classic old_map, Classic updated_map) ->
    Classic (update_map old_map updated_map files_to_remove)
  | ( TypesFirst
        { sig_dependency_graph = old_sig_map; implementation_dependency_graph = old_impl_map },
      TypesFirst
        {
          sig_dependency_graph = updated_sig_map;
          implementation_dependency_graph = updated_impl_map;
        } ) ->
    TypesFirst
      {
        sig_dependency_graph = update_map old_sig_map updated_sig_map files_to_remove;
        implementation_dependency_graph = update_map old_impl_map updated_impl_map files_to_remove;
      }
  | _ -> assert false

let implementation_dependency_graph = function
  | Classic map -> map
  | TypesFirst { implementation_dependency_graph; _ } -> implementation_dependency_graph

let sig_dependency_graph = function
  | Classic map -> map
  | TypesFirst { sig_dependency_graph; _ } -> sig_dependency_graph

let debug_to_string = function
  | Classic map ->
    spf "Classic:\n%s" (debug_string_of_filename_map debug_string_of_filename_set map)
  | TypesFirst { sig_dependency_graph; implementation_dependency_graph } ->
    spf
      "TypesFirst sig dependency graph:\n%s\nTypesFirst implementation dependency graph:\n%s"
      (debug_string_of_filename_map debug_string_of_filename_set sig_dependency_graph)
      (debug_string_of_filename_map debug_string_of_filename_set implementation_dependency_graph)
