(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Given a possibly-cyclic directed graph where each node has a hash, this
 * module propagates bits along dependency edges.
 *
 * For example, traversing the graph (C -> B -> A) has the effect of
 * incorporating A's hash into B and that updated B's hash into C.
 *
 * The traversal handles cycles by finding strongly connected components
 * (Tarjan's algorithm) and computing a component hash, which is assigned to
 * each node in the component.
 *
 * For a detailed description of Tarjan's algorithm, see:
 * http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm
 *
 * The method of computing a cyclic hash takes inspiration from the interning
 * logic from Skip (https://github.com/skiplang/skip) which also computes hashes
 * for potentially cyclic graphs of objects.
 *)

type read_hash = unit -> int64

type write_hash = int64 -> unit

type node = {
  mutable index: int;
  mutable lowlink: int;
  mutable on_stack: bool;
  visit: visit;
  read_hash: read_hash;
  write_hash: write_hash;
}

and visit = edge -> dep_edge -> unit

and edge = node -> unit

and dep_edge = read_hash -> unit

type cx = {
  mutable count: int;
  mutable stack: node list;
}

let create_cx () = { count = 0; stack = [] }

let create_node visit read_hash write_hash =
  { index = -1; lowlink = -1; on_stack = false; visit; read_hash; write_hash }

(* Note that this function does not unset the on_stack field, which ensures that
 * on_stack can be used to identify in-cycle nodes when computing cycle hashes. *)
let rec collect_scc v acc = function
  | [] -> failwith "unexpected empty stack"
  | w :: stack ->
    if w == v then
      ((w, acc), stack)
    else
      collect_scc v (w :: acc) stack

(* Once we have a cycle, we compute it's hash by visiting the cycle, combining
 * hashes from dependency nodes.
 *
 * Because we find cycles by a recursive traversal, the order of nodes in the
 * cycle is arbitrary. To keep cycle hashes stable, we sort the component by
 * it's local hash value and take the minimum element as the root of the
 * traversal.
 *
 * Recall that nodes in this cycle still have the on_stack field set. We abuse
 * this field as a kind of mark bit here. When we see a node with on_stack set,
 * we know that it is part of the cycle. When we visit the node, we unset the
 * field, to record that we've already visited that part of the cycle. *)
let calc_cycle_hash =
  let compare a b = Int64.compare (a.read_hash ()) (b.read_hash ()) in
  let rec min_elt min = function
    | [] -> min
    | x :: xs ->
      let min =
        if compare min x > 0 then
          x
        else
          min
      in
      min_elt min xs
  in
  (* Traverse the cycle, combining hashes for all dependency edges. *)
  let rec visit xx v =
    (* Unset on_stack to indicate that we've visited this node. *)
    v.on_stack <- false;
    let edge w =
      (* If on_stack is set, then we have not yet visited this part of the
       * cycle, so push it on the stack. *)
      if w.on_stack then visit xx w;
      Xx.update_int64 xx (w.read_hash ())
    in
    let dep_edge read_hash = Xx.update_int64 xx (read_hash ()) in
    v.visit edge dep_edge
  in
  fun (x, xs) ->
    let root = min_elt x xs in
    let xx = Xx.init (root.read_hash ()) in
    visit xx root;
    Xx.digest xx

let rec strongconnect cx v =
  let i = cx.count in
  cx.count <- i + 1;

  (* visit node *)
  v.index <- cx.count;
  v.lowlink <- cx.count;

  (* push on stack *)
  v.on_stack <- true;
  cx.stack <- v :: cx.stack;

  (* visit edges *)
  let edge w =
    if w.index = -1 then begin
      strongconnect cx w;
      v.lowlink <- min v.lowlink w.lowlink
    end else if w.on_stack then
      v.lowlink <- min v.lowlink w.index
  in
  v.visit edge ignore;

  if v.lowlink = v.index then begin
    let (scc, stack) = collect_scc v [] cx.stack in
    cx.stack <- stack;
    let cycle_hash = calc_cycle_hash scc in
    Nel.iter (fun x -> x.write_hash cycle_hash) scc
  end

let root cx node = if node.index = -1 then strongconnect cx node

let read_hash node = node.read_hash ()
