(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ident = int

exception Tvar_not_found of ident

module Make (Constraints : sig
  type t
end) =
struct
  (** A root structure carries the actual non-trivial state of a tvar, and
      consists of:

      - rank, which is a quantity roughly corresponding to the longest chain of
      gotos pointing to the tvar. It's an implementation detail of the unification
      algorithm that simply has to do with efficiently finding the root of a tree.
      We merge a tree with another tree by converting the root with the lower rank
      to a goto node, and making it point to the root with the higher rank. See
      http://en.wikipedia.org/wiki/Disjoint-set_data_structure for more details on
      this data structure and supported operations.

      - constraints, which carry type information that narrows down the possible
      solutions of the tvar (see below). *)
  type root = {
    rank: int;
    constraints: Constraints.t Lazy.t;
  }

  (** Type variables are unknowns, and we are ultimately interested in constraints
      on their solutions for type inference.

      Type variables form nodes in a "union-find" forest: each tree denotes a set
      of type variables that are considered by the type system to be equivalent.

      There are two kinds of nodes: Goto nodes and Root nodes.

      - All Goto nodes of a tree point, directly or indirectly, to the Root node
      of the tree.
      - A Root node holds the actual non-trivial state of a tvar, represented by a
      root structure (see below). *)
  type node =
    | Goto of ident
    | Root of root

  (* Find the constraints of a type variable in the graph.

     Recall that type variables are either roots or goto nodes. (See
     Constraint for details.) If the type variable is a root, the
     constraints are stored with the type variable. Otherwise, the type variable
     is a goto node, and it points to another type variable: a linked list of such
     type variables must be traversed until a root is reached. *)
  let rec find_graph graph id =
    let (graph', _, constraints) = find_constraints graph id in
    (graph', constraints)

  and find_constraints graph id =
    let (graph', root_id, root) = find_root graph id in
    (graph', root_id, root.constraints)

  (* Find the root of a type variable, potentially traversing a chain of type
     variables, while short-circuiting all the type variables in the chain to the
     root during traversal to speed up future traversals. *)
  and find_root graph id =
    match IMap.find_opt id graph with
    | Some (Goto next_id) ->
      let (graph', root_id, root) = find_root graph next_id in
      let graph'' =
        if root_id != next_id then
          IMap.add id (Goto root_id) graph'
        else
          graph'
      in
      (graph'', root_id, root)
    | Some (Root root) -> (graph, id, root)
    | None -> raise (Tvar_not_found id)
end
