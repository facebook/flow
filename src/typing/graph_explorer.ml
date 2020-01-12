(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* Custom efficient graph data structure to track exploration of a graph, as
   nodes and edges are dynamically discovered.

   The primary purpose of the data structure is to immediately detect any node
   that is "fully explored", meaning that all nodes that are transitively
   reachable from it have been explored. As long as some of the nodes that are
   reachable from a node have not been explored yet, the node is not fully
   explored.

   A key assumption about the graph is that every node has the following "0->1"
   property: it transitions in one step from a state where it has been
   discovered (the "0" state) to a step where it has been explored (the "1"
   state), i.e., all of its immediate outgoing edges are added at once.

   Certain tvars in Flow have the "0->1" property in the following sense: they
   transition from unresolved to resolved in one step. We can therefore use the
   data structure here to efficiently manage full resolution for such tvars.

   NOTE: This data structure is not guaranteed to work as intended if we try to
   resolve an already resolved tvar, or never resolve an unresolved
   tvar. Roughly, a tvar can be considered to have the 0->1 property whenever it
   is guaranteed to receive one and only one lower bound over its lifetime.
*)

(* The graph maintains the two sets of nodes separately, with edges going across
   in both directions. In other words, the graph is bipartite.

   There are two kinds of nodes: unexplored and explored. Explored nodes have
   dependencies to unexplored nodes. Unexplored nodes have reverse dependencies
   to explored nodes. Note that we always maintain dependencies (and reverse
   dependencies) in transitively closed form.

   Logically these sets are disjoint. As explained above, each node starts out
   as unexplored, and becomes explored when a set of edges are introduced from
   the node to other nodes.
*)

type unexplored = { mutable rev_deps: ISet.t }

type explored = { mutable deps: ISet.t }

type node =
  | Unexplored of unexplored
  | Explored of explored

module Tbl = Hashtbl.Make (struct
  type t = int

  let equal a b = a = b

  let hash = Hashtbl.hash
end)

type graph = node Tbl.t

let new_graph () = Tbl.create (1 lsl 12)

let find_unexplored id graph =
  match Tbl.find graph id with
  | Unexplored unexplored -> unexplored
  | Explored _ -> raise Not_found

let find_explored id graph =
  match Tbl.find graph id with
  | Unexplored _ -> raise Not_found
  | Explored explored -> explored

let is_finished explored = ISet.is_empty explored.deps

(* status of a node *)
type stat =
  | Found of node
  | Finished
  | Node_not_found

(* look up status of a node in a graph *)
let stat_graph id graph =
  match Tbl.find graph id with
  | exception Not_found -> Node_not_found
  | Unexplored _ as node -> Found node
  | Explored explored as node ->
    if is_finished explored then
      Finished
    else
      Found node

let find_graph id graph =
  match stat_graph id graph with
  | Found node -> node
  | _ -> failwith (spf "expected node %d to exist" id)

let is_finished_node = function
  | Unexplored _ -> false
  | Explored explored -> is_finished explored

let is_unexplored_node = function
  | Unexplored _ -> true
  | Explored _ -> false

(* Adding edges from node id1 to nodes in ids2. We assume that id1 is
   unexplored, whereas ids2 may be explored or unexplored (but not finished). *)

(** NOTE: This process has a lot of similarities with how constraints on usual
    tvars are handled in the context graph. In the future, we might move this
    processing back into that framework, by introducing new kinds of tvars on
    which this processing can apply. **)
let edges graph (id1, ids2) =
  let { rev_deps = unexplored1_rev_deps } = find_unexplored id1 graph in
  let explored1 = { deps = ISet.empty } in
  Tbl.replace graph id1 (Explored explored1);

  let finished_ids = ref ISet.empty in
  let ids2 =
    List.fold_left
      (fun ids2 id2 ->
        if id2 = id1 then
          ids2
        else
          match find_graph id2 graph with
          | Unexplored unexplored2 ->
            explored1.deps <- ISet.add id2 explored1.deps;
            unexplored1_rev_deps
            |> ISet.iter (fun id0 ->
                   let explored0 = find_explored id0 graph in
                   explored0.deps <- ISet.add id2 explored0.deps);

            unexplored2.rev_deps <- ISet.add id1 unexplored2.rev_deps;
            unexplored2.rev_deps <- ISet.union unexplored1_rev_deps unexplored2.rev_deps;

            ids2
          | Explored explored2 -> ISet.union explored2.deps ids2)
      ISet.empty
      ids2
  in
  let ids2 = ISet.remove id1 ids2 in
  explored1.deps <- ISet.union ids2 explored1.deps;
  unexplored1_rev_deps
  |> ISet.iter (fun id0 ->
         let explored0 = find_explored id0 graph in
         explored0.deps <- ISet.union ids2 explored0.deps);

  ids2
  |> ISet.iter (fun id2 ->
         let unexplored2 = find_unexplored id2 graph in
         unexplored2.rev_deps <- ISet.add id1 unexplored2.rev_deps;
         unexplored2.rev_deps <- ISet.union unexplored1_rev_deps unexplored2.rev_deps);

  explored1.deps <- ISet.remove id1 explored1.deps;
  if is_finished explored1 then finished_ids := ISet.add id1 !finished_ids;
  unexplored1_rev_deps
  |> ISet.iter (fun id0 ->
         let explored0 = find_explored id0 graph in
         explored0.deps <- ISet.remove id1 explored0.deps;
         if is_finished explored0 then finished_ids := ISet.add id0 !finished_ids);

  !finished_ids

(* Add a node to the graph *)
let node graph id =
  let unexplored = { rev_deps = ISet.empty } in
  Tbl.add graph id (Unexplored unexplored)
