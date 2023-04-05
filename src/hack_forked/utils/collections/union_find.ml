(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
    mutable rank: int;
    mutable constraints: Constraints.t;
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
  type node_ =
    | Goto of { mutable parent: ident }
    | Root of root

  type node = node_ ref

  type graph = node IMap.t

  let create_root constraints = ref (Root { rank = 0; constraints })

  let create_goto parent = ref (Goto { parent })

  (* Find the root of a type variable, potentially traversing a chain of type
     variables, while short-circuiting all the type variables in the chain to the
     root during traversal to speed up future traversals. *)
  let rec find_root graph id =
    match IMap.find_opt id graph with
    | None -> raise (Tvar_not_found id)
    | Some node ->
      (match !node with
      | Root root -> (id, node, root)
      | Goto goto ->
        let ((root_id, _, _) as root) = find_root graph goto.parent in
        goto.parent <- root_id;
        root)

  let find_root_id graph id =
    let (root_id, _, _) = find_root graph id in
    root_id

  (* Find the constraints of a type variable in the graph.

     Recall that type variables are either roots or goto nodes. (See
     Constraint for details.) If the type variable is a root, the
     constraints are stored with the type variable. Otherwise, the type variable
     is a goto node, and it points to another type variable: a linked list of such
     type variables must be traversed until a root is reached. *)
  let find_constraints graph id =
    let (root_id, _, root) = find_root graph id in
    (root_id, root.constraints)

  let find_graph graph id =
    let (_, constraints) = find_constraints graph id in
    constraints
end
