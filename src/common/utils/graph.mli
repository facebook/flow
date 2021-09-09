(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A directional graph which maintains back edges to facilitate fast traversals in either direction. *)

module Make (Set : Flow_set.S) (Map : WrappedMap.S with type key = Set.elt) :
  Graph_sig.S with type elt = Map.key and type map = Set.t Map.t and type set = Set.t
