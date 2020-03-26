(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type openness =
  | Open
  | Closed

type node

type graph = node IMap.t

let empty_graph : graph = IMap.empty

let find_opt (_ : int) (_ : graph) = None

let openness_of_node (_ : node) = Open
