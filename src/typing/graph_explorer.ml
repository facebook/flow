(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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

(* There are two kinds of nodes: unexplored and explored. Explored nodes have
   dependencies to unexplored nodes. Unexplored nodes have reverse dependencies
   to explored nodes. Note that we always maintain dependencies (and reverse
   dependencies) in transitively closed form. *)

type unexplored = {
  mutable rev_deps: ISet.t;
}

type explored = {
  mutable deps: ISet.t;
}

(* union type of unexplored and explored nodes; useful for aggregation. *)
type node =
| Unexplored of unexplored
| Explored of explored

(* The graph maintains the two sets of nodes separately, with edges going across
   in both directions. In other words, the graph is bipartite.

   For efficiency, we also maintain a third set of nodes, namely the nodes that
   have been fully explored.

   Logically these sets are disjoint. As explained above, each node starts out
   as unexplored, and becomes explored when a set of edges are introduced from
   the node to other nodes. When an explored node has no more dependencies, it
   is moved to the finished set.

   That said, we sometimes aggressively mark nodes as finished (e.g., once we
   reduce signature context graphs after merging), without taking the time to
   clear out those nodes from the other sets. This is fine because we always
   check membership in the set of finished nodes before doing any work.
*)
type graph = {
  mutable unexplored_nodes: unexplored IMap.t;
  mutable explored_nodes: explored IMap.t;
  mutable finished: ISet.t;
}

let new_graph finished = {
  unexplored_nodes = IMap.empty;
  explored_nodes = IMap.empty;
  finished;
}

(* Merge finished from other_graph to graph.

   We don't care about merging explored_nodes and unexplored_nodes from
   other_graph, since those were local to other_graph and should have been
   cleared in any case to optimize space. On the other hand, we do care about
   preserving the explored_nodes and unexplored_nodes in graph, since they may
   still be in use.
*)
let union_finished other_graph graph =
  { graph with finished = ISet.union other_graph.finished graph.finished }

let find_unexplored id graph =
  IMap.find_unsafe id graph.unexplored_nodes

let find_explored id graph =
  IMap.find_unsafe id graph.explored_nodes

(* status of a node *)
type stat =
| Found of node
| Finished
| Not_found

(* look up status of a node in a graph *)
let stat_graph id graph =
  if ISet.mem id graph.finished then Finished else
  match IMap.get id graph.unexplored_nodes with
  | Some unexplored -> Found (Unexplored unexplored)
  | None -> begin match IMap.get id graph.explored_nodes with
    | Some explored -> Found (Explored explored)
    | None -> Not_found
    end

let find_graph id graph =
  match stat_graph id graph with
  | Found node -> node
  | _ -> failwith (spf "expected node %d to exist" id)

let is_finished explored =
  ISet.is_empty explored.deps

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
  let unexplored1 = find_unexplored id1 graph in
  graph.unexplored_nodes <- IMap.remove id1 graph.unexplored_nodes;
  let explored1 = { deps = ISet.empty } in
  graph.explored_nodes <- IMap.add id1 explored1 graph.explored_nodes;

  let finished_ids = ref ISet.empty in

  let ids2 = List.fold_left (fun ids2 id2 ->
    match find_graph id2 graph with
    | Unexplored unexplored2 ->
        explored1.deps <- ISet.add id2 explored1.deps;
        unexplored1.rev_deps |> ISet.iter (fun id0 ->
          let explored0 = find_explored id0 graph in
          explored0.deps <- ISet.add id2 explored0.deps;
        );

        unexplored2.rev_deps <- ISet.add id1 unexplored2.rev_deps;
        unexplored2.rev_deps <-
          ISet.union unexplored1.rev_deps unexplored2.rev_deps;

        ids2

    | Explored explored2 ->
        ISet.union explored2.deps ids2

  ) ISet.empty ids2 in

  let ids2 = ISet.remove id1 ids2 in

  explored1.deps <- ISet.union ids2 explored1.deps;
  unexplored1.rev_deps |> ISet.iter (fun id0 ->
    let explored0 = find_explored id0 graph in
    explored0.deps <- ISet.union ids2 explored0.deps;
  );

  ids2 |> ISet.iter (fun id2 ->
    let unexplored2 = find_unexplored id2 graph in
    unexplored2.rev_deps <- ISet.add id1 unexplored2.rev_deps;
    unexplored2.rev_deps <-
      ISet.union unexplored1.rev_deps unexplored2.rev_deps;
  );

  explored1.deps <- ISet.remove id1 explored1.deps;
  if is_finished explored1
  then finished_ids := ISet.add id1 !finished_ids;
  unexplored1.rev_deps |> ISet.iter (fun id0 ->
    let explored0 = find_explored id0 graph in
    explored0.deps <- ISet.remove id1 explored0.deps;
    if is_finished explored0
    then finished_ids := ISet.add id0 !finished_ids;
  );
  unexplored1.rev_deps <- ISet.empty;

  graph.finished <- ISet.union !finished_ids graph.finished;
  !finished_ids

(* Add a node to the graph *)
let node graph id =
  let unexplored = { rev_deps = ISet.empty } in
  graph.unexplored_nodes <- IMap.add id unexplored graph.unexplored_nodes
