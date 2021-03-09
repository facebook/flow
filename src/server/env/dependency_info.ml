(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type t = {
  sig_dependency_graph: FilenameGraph.t;
  implementation_dependency_graph: FilenameGraph.t;
}

type partial_dependency_graph =
  (Utils_js.FilenameSet.t * Utils_js.FilenameSet.t) Utils_js.FilenameMap.t

let extract_sig_map map = FilenameMap.map (fun (sig_deps, _impl_deps) -> sig_deps) map

let extract_impl_map map = FilenameMap.map (fun (_sig_deps, impl_deps) -> impl_deps) map

let of_map map =
  let sig_dependency_map = extract_sig_map map in
  let sig_dependency_graph = FilenameGraph.of_map sig_dependency_map in
  let implementation_dependency_map = extract_impl_map map in
  let implementation_dependency_graph = FilenameGraph.of_map implementation_dependency_map in
  { sig_dependency_graph; implementation_dependency_graph }

let update old_dep_info partial_dep_graph to_remove =
  let { sig_dependency_graph = old_sig_graph; implementation_dependency_graph = old_impl_graph } =
    old_dep_info
  in
  let updated_map = partial_dep_graph in
  let updated_sig_map = extract_sig_map updated_map in
  let updated_impl_map = extract_impl_map updated_map in

  {
    sig_dependency_graph = FilenameGraph.update_from_map old_sig_graph updated_sig_map ~to_remove;
    implementation_dependency_graph =
      FilenameGraph.update_from_map old_impl_graph updated_impl_map ~to_remove;
  }

let implementation_dependency_graph dep_info = dep_info.implementation_dependency_graph

let sig_dependency_graph dep_info = dep_info.sig_dependency_graph

let map_filenames f dep_info =
  let { sig_dependency_graph; implementation_dependency_graph } = dep_info in
  {
    sig_dependency_graph = FilenameGraph.map f sig_dependency_graph;
    implementation_dependency_graph = FilenameGraph.map f implementation_dependency_graph;
  }

let debug_to_string dep_info =
  let { sig_dependency_graph; implementation_dependency_graph } = dep_info in
  let sig_map = FilenameGraph.to_map sig_dependency_graph in
  let impl_map = FilenameGraph.to_map implementation_dependency_graph in
  spf
    "Sig dependency graph:\n%s\nImplementation dependency graph:\n%s"
    (debug_string_of_filename_map debug_string_of_filename_set sig_map)
    (debug_string_of_filename_map debug_string_of_filename_set impl_map)
