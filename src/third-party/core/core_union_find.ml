(* This code is based on the MLton library set/disjoint.fun, which has the
   following copyright notice.
*)
(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
   {v
           Root
             |
           Inner
        / .. | .. \
     Inner Inner Inner
      /|\   /|\   /|\
      ...   ...   ...
   v}

   We construct the `inverted' tree in the ML representation.
   The direction of the edges is UPWARDS.
   Starting with any ['a t] we can step directly to its parent.
   But we can't (and don't need to) start from the root and step to its children.
*)

(*
   [rank] is an upper bound on the depth of any node in the up-tree.

   Imagine an unlucky sequence of operations in which you create N
   individual [t]-values and then union them together in such a way
   that you always pick the root of each tree to union together, so that
   no path compression takes place.  If you don't take care to somehow
   balance the resulting up-tree, it is possible that you end up with one
   big long chain of N links, and then calling [representative] on the
   deepest node takes Theta(N) time.  With the balancing scheme of never
   increasing the rank of a node unnecessarily, it would take O(log N).
*)
type 'a root = {
  mutable value: 'a;
  mutable rank: int;
}

type 'a t = { mutable node: 'a node }

and 'a node =
  | Inner of 'a t
  (* [Inner x] is a node whose parent is [x]. *)
  | Root of 'a root

let invariant _ t =
  let rec loop t depth =
    match t.node with
    | Inner t -> loop t (depth + 1)
    | Root r -> assert (depth <= r.rank)
  in
  loop t 0

let create v = { node = Root { value = v; rank = 0 } }

(* invariants:
   [inner.node] = [inner_node] = [Inner t].
   [descendants] are the proper descendants of [inner] we've visited.
*)
let rec compress t ~inner_node ~inner ~descendants =
  match t.node with
  | Root r ->
    (* t is the root of the tree.
       Re-point all descendants directly to it by setting them to [Inner t].
       Note: we don't re-point [inner] as it already points there. *)
    Base.List.iter descendants ~f:(fun t -> t.node <- inner_node);
    (t, r)
  | Inner t' as node -> compress t' ~inner_node:node ~inner:t ~descendants:(inner :: descendants)

let representative t =
  match t.node with
  | Root r -> (t, r)
  | Inner t' as node -> compress t' ~inner_node:node ~inner:t ~descendants:[]

let root t =
  match t.node with
  | Root r ->
    (* avoid tuple allocation in the fast path *)
    r
  | _ -> snd (representative t)

let rank t = (root t).rank

let get t = (root t).value

let set t v = (root t).value <- v

let same_class t1 t2 = Base.phys_equal (root t1) (root t2)

let union t1 t2 =
  let (t1, r1) = representative t1 in
  let (t2, r2) = representative t2 in
  if Base.phys_equal r1 r2 then
    ()
  else
    let n1 = r1.rank in
    let n2 = r2.rank in
    if n1 < n2 then
      t1.node <- Inner t2
    else (
      t2.node <- Inner t1;
      if n1 = n2 then r1.rank <- r1.rank + 1
    )

let is_compressed t =
  invariant ignore t;
  match t.node with
  | Root _ -> true
  | Inner t ->
    (match t.node with
    | Root _ -> true
    | Inner _ -> false)

module Private = struct
  let is_compressed = is_compressed

  let rank = rank
end
