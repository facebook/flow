(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* This code has been copy/pasted from the OCaml standard library.
 * There is only a slight modification.
 * Every element of the Avl tree is augmented with an additional element
 * of a monoid.
 * During all the operations on the tree, the element of the monoid
 * is maintained, the composition is always from left to right.
 * So if you have the tree ((0, Empty, Empty), 34, (45, Empty, Empty)
 * The "augmented tree" will look like this:
 * (make 0, 0, Empty, Empty), 
 * (compose (make 0) (compose ((make 34) (make 45)))), 34, 
 * (make 34, 45, Empty, Empty)
 *
 * We need this data-structure to quickly scan a tree.
 * We want 2 operations to be fast:
 * 1) add a file with a timestamp to the tree
 * 2) fetch all the files younger than time t
 *
 * The element of the monoid is helping us to make 2) fast.
 * Our composition function is "max", this way for any given
 * node we know how old the youngest element in a node is.
 * If the youngest element is too old, we can cut the branch.
 * (cf function walk in monoidAvl.ml)
 *)

module type MonoidOrderedType = sig
  type elt

  val compare : elt -> elt -> int

  type monoelt

  val neutral : monoelt

  (* This better be associative *)
  val compose : monoelt -> monoelt -> monoelt

  val make : elt -> monoelt
end

module type S = sig
  type elt

  type monoelt

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val partition : (elt -> bool) -> t -> t * t

  val cardinal : t -> int

  val elements : t -> elt list

  val min_elt : t -> elt

  val max_elt : t -> elt

  val choose : t -> elt

  val split : elt -> t -> t * bool * t

  val walk : (monoelt -> bool) -> (elt -> unit) -> t -> unit
end

module Make (Ord : MonoidOrderedType) = struct
  type elt = Ord.elt

  type monoelt = Ord.monoelt

  type t =
    | Empty
    | Node of monoelt * t * elt * t * int

  let height = function
    | Empty -> 0
    | Node (_, _, _, _, h) -> h

  let get_monoelt = function
    | Empty -> Ord.neutral
    | Node (x, _, _, _, _) -> x

  let create l v r =
    let hl =
      match l with
      | Empty -> 0
      | Node (_, _, _, _, h) -> h
    in
    let hr =
      match r with
      | Empty -> 0
      | Node (_, _, _, _, h) -> h
    in
    let monoelt = Ord.make v in
    let monoelt = Ord.compose (get_monoelt l) monoelt in
    let monoelt = Ord.compose monoelt (get_monoelt r) in
    Node
      ( monoelt,
        l,
        v,
        r,
        if hl >= hr then
          hl + 1
        else
          hr + 1 )

  let bal l v r =
    let hl =
      match l with
      | Empty -> 0
      | Node (_, _, _, _, h) -> h
    in
    let hr =
      match r with
      | Empty -> 0
      | Node (_, _, _, _, h) -> h
    in
    if hl > hr + 2 then
      match l with
      | Empty -> invalid_arg "Set.bal"
      | Node (_, ll, lv, lr, _) ->
        if height ll >= height lr then
          create ll lv (create lr v r)
        else (
          match lr with
          | Empty -> invalid_arg "Set.bal"
          | Node (_, lrl, lrv, lrr, _) ->
            create (create ll lv lrl) lrv (create lrr v r)
        )
    else if hr > hl + 2 then
      match r with
      | Empty -> invalid_arg "Set.bal"
      | Node (_, rl, rv, rr, _) ->
        if height rr >= height rl then
          create (create l v rl) rv rr
        else (
          match rl with
          | Empty -> invalid_arg "Set.bal"
          | Node (_, rll, rlv, rlr, _) ->
            create (create l v rll) rlv (create rlr rv rr)
        )
    else
      create l v r

  let rec add x = function
    | Empty -> Node (Ord.make x, Empty, x, Empty, 1)
    | Node (_, l, v, r, _) ->
      let c = Ord.compare x v in
      if c = 0 then
        create l x r
      else if c < 0 then
        bal (add x l) v r
      else
        bal l v (add x r)

  let rec join l v r =
    match (l, r) with
    | (Empty, _) -> add v r
    | (_, Empty) -> add v l
    | (Node (_, ll, lv, lr, lh), Node (_, rl, rv, rr, rh)) ->
      if lh > rh + 2 then
        bal ll lv (join lr v r)
      else if rh > lh + 2 then
        bal (join l v rl) rv rr
      else
        create l v r

  let rec min_elt = function
    | Empty -> raise Not_found
    | Node (_, Empty, v, _, _) -> v
    | Node (_, l, _, _, _) -> min_elt l

  let rec max_elt = function
    | Empty -> raise Not_found
    | Node (_, _, v, Empty, _) -> v
    | Node (_, _, _, r, _) -> max_elt r

  let rec remove_min_elt = function
    | Empty -> invalid_arg "Set.remove_min_elt"
    | Node (_, Empty, _, r, _) -> r
    | Node (_, l, v, r, _) -> bal (remove_min_elt l) v r

  let merge t1 t2 =
    match (t1, t2) with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

  let concat t1 t2 =
    match (t1, t2) with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

  let rec split x = function
    | Empty -> (Empty, false, Empty)
    | Node (_, l, v, r, _) ->
      let c = Ord.compare x v in
      if c = 0 then
        (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split x l in
        (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = split x r in
        (join l v lr, pres, rr)

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false

  let rec mem x = function
    | Empty -> false
    | Node (_, l, v, r, _) ->
      let c = Ord.compare x v in
      c = 0
      || mem
           x
           ( if c < 0 then
             l
           else
             r )

  let singleton x = Node (Ord.make x, Empty, x, Empty, 1)

  let rec remove x = function
    | Empty -> Empty
    | Node (_, l, v, r, _) ->
      let c = Ord.compare x v in
      if c = 0 then
        merge l r
      else if c < 0 then
        bal (remove x l) v r
      else
        bal l v (remove x r)

  let rec union s1 s2 =
    match (s1, s2) with
    | (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node (_, l1, v1, r1, h1), Node (_, l2, v2, r2, h2)) ->
      if h1 >= h2 then
        if h2 = 1 then
          add v2 s1
        else
          let (l2, _, r2) = split v1 s2 in
          join (union l1 l2) v1 (union r1 r2)
      else if h1 = 1 then
        add v1 s2
      else
        let (l1, _, r1) = split v2 s1 in
        join (union l1 l2) v2 (union r1 r2)

  let rec inter s1 s2 =
    match (s1, s2) with
    | (Empty, _) -> Empty
    | (_, Empty) -> Empty
    | (Node (_, l1, v1, r1, _), t2) ->
      (match split v1 t2 with
      | (l2, false, r2) -> concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) -> join (inter l1 l2) v1 (inter r1 r2))

  let rec diff s1 s2 =
    match (s1, s2) with
    | (Empty, _) -> Empty
    | (t1, Empty) -> t1
    | (Node (_, l1, v1, r1, _), t2) ->
      (match split v1 t2 with
      | (l2, false, r2) -> join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) -> concat (diff l1 l2) (diff r1 r2))

  type enumeration =
    | End
    | More of elt * t * enumeration

  let rec cons_enum s e =
    match s with
    | Empty -> e
    | Node (_, l, v, r, _) -> cons_enum l (More (v, r, e))

  let rec compare_aux e1 e2 =
    match (e1, e2) with
    | (End, End) -> 0
    | (End, _) -> -1
    | (_, End) -> 1
    | (More (v1, r1, e1), More (v2, r2, e2)) ->
      let c = Ord.compare v1 v2 in
      if c <> 0 then
        c
      else
        compare_aux (cons_enum r1 e1) (cons_enum r2 e2)

  let compare s1 s2 = compare_aux (cons_enum s1 End) (cons_enum s2 End)

  let equal s1 s2 = compare s1 s2 = 0

  let rec subset s1 s2 =
    match (s1, s2) with
    | (Empty, _) -> true
    | (_, Empty) -> false
    | (Node (_, l1, v1, r1, _), (Node (_, l2, v2, r2, _) as t2)) ->
      let c = Ord.compare v1 v2 in
      if c = 0 then
        subset l1 l2 && subset r1 r2
      else if c < 0 then
        let node = create l1 v1 Empty in
        subset node l2 && subset r1 t2
      else
        let node = create Empty v1 r1 in
        subset node r2 && subset l1 t2

  let rec iter f = function
    | Empty -> ()
    | Node (_, l, v, r, _) ->
      iter f l;
      f v;
      iter f r

  let rec fold f s accu =
    match s with
    | Empty -> accu
    | Node (_, l, v, r, _) -> fold f r (f v (fold f l accu))

  let rec for_all p = function
    | Empty -> true
    | Node (_, l, v, r, _) -> p v && for_all p l && for_all p r

  let rec exists p = function
    | Empty -> false
    | Node (_, l, v, r, _) -> p v || exists p l || exists p r

  let filter p s =
    let rec filt accu = function
      | Empty -> accu
      | Node (_, l, v, r, _) ->
        filt
          (filt
             ( if p v then
               add v accu
             else
               accu )
             l)
          r
    in
    filt Empty s

  let partition p s =
    let rec part ((t, f) as accu) = function
      | Empty -> accu
      | Node (_, l, v, r, _) ->
        part
          (part
             ( if p v then
               (add v t, f)
             else
               (t, add v f) )
             l)
          r
    in
    part (Empty, Empty) s

  let rec cardinal = function
    | Empty -> 0
    | Node (_, l, _, r, _) -> cardinal l + 1 + cardinal r

  let rec elements_aux accu = function
    | Empty -> accu
    | Node (_, l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

  let elements s = elements_aux [] s

  let choose = min_elt

  let rec walk cut_branch work t =
    match t with
    | Empty -> ()
    | Node (elt, _, _, _, _) when cut_branch elt -> ()
    | Node (_, l, key, r, _) ->
      walk cut_branch work l;
      if cut_branch (Ord.make key) then
        ()
      else
        work key;
      walk cut_branch work r
end
