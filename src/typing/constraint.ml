(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

type ident = int

(** Type variables are unknowns, and we are ultimately interested in constraints
    on their solutions for type inference.

    Type variables form nodes in a "union-find" forest: each tree denotes a set
    of type variables that are considered by the type system to be equivalent.

    There are two kinds of nodes: Goto nodes and Root nodes.

    - All Goto nodes of a tree point, directly or indirectly, to the Root node
    of the tree.
    - A Root node holds the actual non-trivial state of a tvar, represented by a
    root structure (see below).
 **)
type node =
  | Goto of ident
  | Root of root

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
    solutions of the tvar (see below).  **)

and root = {
  rank: int;
  constraints: constraints;
}

(** Constraints carry type information that narrows down the possible solutions
    of tvar, and are of two kinds:

    - A Resolved constraint contains a concrete type that is considered by the
    type system to be the solution of the tvar carrying the constraint. In other
    words, the tvar is equivalent to this concrete type in all respects.

    - Unresolved constraints contain bounds that carry both concrete types and
    other tvars as upper and lower bounds (see below).
 **)

and constraints =
  | Resolved of Type.use_op * Type.t
  | FullyResolved of Type.use_op * Type.t
  | Unresolved of bounds

and bounds = {
  mutable lower: (Trace.t * Type.use_op) TypeMap.t;
  mutable upper: Trace.t UseTypeMap.t;
  mutable lowertvars: (Trace.t * Type.use_op) IMap.t;
  mutable uppertvars: (Trace.t * Type.use_op) IMap.t;
}
(** The bounds structure carries the evolving constraints on the solution of an
    unresolved tvar.

    - upper and lower hold concrete upper and lower bounds, respectively. At any
    point in analysis the aggregate lower bound of a tvar is (conceptually) the
    union of the concrete types in lower, and the aggregate upper bound is
    (conceptually) the intersection of the concrete types in upper. (Upper and
    lower are maps, with the types as keys, and trace information as values.)

    - lowertvars and uppertvars hold tvars which are also (latent) lower and
    upper bounds, respectively. See the __flow function for how these structures
    are populated and operated on.  Here the map keys are tvar ids, with trace
    info as values.

    The use_op in the lower TypeMap represents the use_op when a lower bound
    was added.
 **)

let new_bounds () =
  {
    lower = TypeMap.empty;
    upper = UseTypeMap.empty;
    lowertvars = IMap.empty;
    uppertvars = IMap.empty;
  }

let new_unresolved_root () = Root { rank = 0; constraints = Unresolved (new_bounds ()) }

let new_resolved_root t op = Root { rank = 0; constraints = FullyResolved (op, t) }
