(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type t =
  | Classic of FilenameGraph.t
  | TypesFirst of {
      sig_dependency_graph: FilenameGraph.t;
      implementation_dependency_graph: FilenameGraph.t;
    }

let extract_sig_map map = FilenameMap.map (fun (sig_deps, _impl_deps) -> sig_deps) map

let extract_impl_map map = FilenameMap.map (fun (_sig_deps, impl_deps) -> impl_deps) map

let of_classic_map map = Classic (FilenameGraph.of_map map)

let of_types_first_map map =
  let sig_dependency_map = extract_sig_map map in
  let sig_dependency_graph = FilenameGraph.of_map sig_dependency_map in
  let implementation_dependency_map = extract_impl_map map in
  let implementation_dependency_graph = FilenameGraph.of_map implementation_dependency_map in
  TypesFirst { sig_dependency_graph; implementation_dependency_graph }

let update old_dep_info partial_dep_graph to_remove =
  let open Partial_dependency_graph in
  match (old_dep_info, partial_dep_graph) with
  | (Classic old_graph, PartialClassicDepGraph updated_map) ->
    Classic (FilenameGraph.update_from_map old_graph updated_map ~to_remove)
  | ( TypesFirst
        { sig_dependency_graph = old_sig_graph; implementation_dependency_graph = old_impl_graph },
      PartialTypesFirstDepGraph updated_map ) ->
    let updated_sig_map = extract_sig_map updated_map in
    let updated_impl_map = extract_impl_map updated_map in
    TypesFirst
      {
        sig_dependency_graph =
          FilenameGraph.update_from_map old_sig_graph updated_sig_map ~to_remove;
        implementation_dependency_graph =
          FilenameGraph.update_from_map old_impl_graph updated_impl_map ~to_remove;
      }
  | _ -> assert false

let implementation_dependency_graph = function
  | Classic graph -> graph
  | TypesFirst { implementation_dependency_graph; _ } -> implementation_dependency_graph

let sig_dependency_graph = function
  | Classic graph -> graph
  | TypesFirst { sig_dependency_graph; _ } -> sig_dependency_graph

let debug_to_string = function
  | Classic graph ->
    let map = FilenameGraph.to_map graph in
    spf "Classic:\n%s" (debug_string_of_filename_map debug_string_of_filename_set map)
  | TypesFirst { sig_dependency_graph; implementation_dependency_graph } ->
    let sig_map = FilenameGraph.to_map sig_dependency_graph in
    let impl_map = FilenameGraph.to_map implementation_dependency_graph in
    spf
      "TypesFirst sig dependency graph:\n%s\nTypesFirst implementation dependency graph:\n%s"
      (debug_string_of_filename_map debug_string_of_filename_set sig_map)
      (debug_string_of_filename_map debug_string_of_filename_set impl_map)
