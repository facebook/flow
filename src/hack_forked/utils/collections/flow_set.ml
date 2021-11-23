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

(* This module has been inspired from the OCaml standard library.
 * There are some modifications to make it run fast.
 * - It adds a Leaf node to avoid excessive allocation for singleton set
 * - In the hot [bal] function when we we know it has to be [Node], we do
 *   an unsafe cast to avoid some unneeded tests
 * - Functions not need comparison functions are lifted outside functors
 * - We can add more utilities relying on the internals in the future
 *)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type elt

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val disjoint : t -> t -> bool

  val diff : t -> t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val map : (elt -> elt) -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val partition : (elt -> bool) -> t -> t * t

  val cardinal : t -> int

  val elements : t -> elt list

  val min_elt : t -> elt

  val min_elt_opt : t -> elt option

  val max_elt : t -> elt

  val max_elt_opt : t -> elt option

  val choose : t -> elt

  val choose_opt : t -> elt option

  val find : elt -> t -> elt

  val find_opt : elt -> t -> elt option

  val to_seq : t -> elt Seq.t

  val of_list : elt list -> t

  val make_pp : (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
end

type 'elt t0 =
  | Empty
  | Leaf of 'elt
  | Node of {
      h: int;
      v: 'elt;
      l: 'elt t0;
      r: 'elt t0;
    }

type 'elt partial_node = {
  h: int;
  v: 'elt;
  l: 'elt t0;
  r: 'elt t0;
}

external ( ~! ) : 'elt t0 -> 'elt partial_node = "%identity"

type ('elt, 't) enumeration0 =
  | End
  | More of 'elt * 't * ('elt, 't) enumeration0

let rec cons_enum s e =
  match s with
  | Empty -> e
  | Leaf v -> More (v, Empty, e)
  | Node { l; v; r; _ } -> cons_enum l (More (v, r, e))

let rec seq_of_enum_ c () =
  match c with
  | End -> Seq.Nil
  | More (x, t, rest) -> Seq.Cons (x, seq_of_enum_ (cons_enum t rest))

let to_seq c = seq_of_enum_ (cons_enum c End)

let[@inline] height = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node { h; _ } -> h

let[@inline] singleton x = Leaf x

(* FIXME: we should check to avoid creating unneeded Node
   - node
   - Node
   This function produce Node of height at least [1]
*)
let unsafe_node ~l ~v ~r =
  match (l, r) with
  | (Empty, Empty) -> singleton v
  | (Leaf _, Empty)
  | (Leaf _, Leaf _)
  | (Empty, Leaf _) ->
    Node { l; v; r; h = 2 }
  | (Node { h; _ }, (Leaf _ | Empty))
  | ((Leaf _ | Empty), Node { h; _ }) ->
    Node { l; v; r; h = h + 1 }
  | (Node { h = hl; _ }, Node { h = hr; _ }) ->
    let h =
      if hl >= hr then
        hl + 1
      else
        hr + 1
    in
    Node { l; v; r; h }

(* Creates a new node with left son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | height l - height r | <= 2.
   Inline expansion of height for better speed. *)

let create l v r =
  let hl = height l in
  let hr = height r in
  Node
    {
      l;
      v;
      r;
      h =
        ( if hl >= hr then
          hl + 1
        else
          hr + 1
        );
    }

(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | height l - height r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)

let bal l v r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    (* hl is at least of height > 2 [3], so it should be [Node]
       Note having in-efficient nodes like [Node (empty,v,empty)] won't affect
       correctness here, since it will be even more likely to be [Node]
       But we are stricter with height
    *)
    let { l = ll; v = lv; r = lr; _ } = ~!l in
    if height ll >= height lr then
      create ll lv (unsafe_node ~l:lr ~v ~r)
    else
      (* Int his path hlr > hll while hl = hlr + 1 so [hlr] > 1, so it should be [Node]*)
      let { l = lrl; v = lrv; r = lrr; _ } = ~!lr in
      create (unsafe_node ~l:ll ~v:lv ~r:lrl) lrv (unsafe_node ~l:lrr ~v ~r)
  else if hr > hl + 2 then
    (* hr is at least of height > 2 [3], so it should be [Node] *)
    let { l = rl; v = rv; r = rr; _ } = ~!r in
    if height rr >= height rl then
      create (unsafe_node ~l ~v ~r:rl) rv rr
    else
      (* In this path hrl > hrr while hr = hrl + 1, so [hrl] > 1, so it should be [Node] *)
      let { l = rll; v = rlv; r = rlr; _ } = ~!rl in
      create (unsafe_node ~l ~v ~r:rll) rlv (unsafe_node ~l:rlr ~v:rv ~r:rr)
  else
    unsafe_node ~l ~v ~r

(* Beware: those two functions assume that the added v is *strictly*
   smaller (or bigger) than all the present elements in the tree; it
   does not test for equality with the current min (or max) element.
   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_element x = function
  | Empty -> singleton x
  | Leaf v -> unsafe_node ~l:(singleton x) ~v ~r:Empty
  | Node { l; v; r; _ } -> bal (add_min_element x l) v r

let rec add_max_element x = function
  | Empty -> singleton x
  | Leaf v -> unsafe_node ~l:Empty ~v ~r:(singleton x)
  | Node { l; v; r; _ } -> bal l v (add_max_element x r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_min_element v r
  | (_, Empty) -> add_max_element v l
  | (Leaf _, Leaf _) -> unsafe_node ~l ~v ~r
  | (Leaf _, Node { l = rl; v = rv; r = rr; h = rh }) ->
    if rh > 3 then
      bal (join l v rl) rv rr
    else
      create l v r
  | (Node { l = ll; v = lv; r = lr; h = lh }, Leaf _) ->
    if lh > 3 then
      bal ll lv (join lr v r)
    else
      create l v r
  | (Node { l = ll; v = lv; r = lr; h = lh }, Node { l = rl; v = rv; r = rr; h = rh }) ->
    if lh > rh + 2 then
      bal ll lv (join lr v r)
    else if rh > lh + 2 then
      bal (join l v rl) rv rr
    else
      create l v r

(* Smallest and greatest element of a set *)

let rec min_elt = function
  | Empty -> raise Not_found
  | Leaf v -> v
  | Node { l = Empty; v; _ } -> v
  | Node { l; _ } -> min_elt l

let rec min_elt_opt = function
  | Empty -> None
  | Leaf v -> Some v
  | Node { l = Empty; v; _ } -> Some v
  | Node { l; _ } -> min_elt_opt l

let rec max_elt = function
  | Empty -> raise Not_found
  | Node { v; r = Empty; _ } -> v
  | Leaf v -> v
  | Node { r; _ } -> max_elt r

let rec max_elt_opt = function
  | Empty -> None
  | Node { v; r = Empty; _ } -> Some v
  | Leaf v -> Some v
  | Node { r; _ } -> max_elt_opt r

(* Remove the smallest element of the given set *)

let rec remove_min_elt = function
  | Empty -> invalid_arg "Set.remove_min_elt"
  | Leaf _ -> Empty
  | Node { l = Empty; r; _ } -> r
  | Node { l; v; r; _ } -> bal (remove_min_elt l) v r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assume | height l - height r | <= 2. *)

let merge t1 t2 =
  match (t1, t2) with
  | (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
  | (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

let rec cardinal = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node { l; r; _ } -> cardinal l + 1 + cardinal r

let rec elements_aux accu = function
  | Empty -> accu
  | Leaf v -> v :: accu
  | Node { l; v; r; _ } -> elements_aux (v :: elements_aux accu r) l

let elements s = elements_aux [] s

let empty = Empty

let[@inline] is_empty = function
  | Empty -> true
  | _ -> false

let of_sorted_list l =
  let rec sub n l =
    match (n, l) with
    | (0, l) -> (Empty, l)
    | (1, x0 :: l) -> (singleton x0, l)
    | (2, x0 :: x1 :: l) -> (Node { l = singleton x0; v = x1; r = Empty; h = 2 }, l)
    | (3, x0 :: x1 :: x2 :: l) -> (Node { l = singleton x0; v = x1; r = singleton x2; h = 2 }, l)
    | (n, l) ->
      let nl = n / 2 in
      let (left, l) = sub nl l in
      (match l with
      | [] -> assert false
      | mid :: l ->
        let (right, l) = sub (n - nl - 1) l in
        (create left mid right, l))
  in
  fst (sub (List.length l) l)

type 'a t1 = 'a t0 = private
  | Empty
  | Leaf of 'a
  | Node of {
      h: int;
      v: 'a;
      l: 'a t0;
      r: 'a t0;
    }

module Make (Ord : OrderedType) : S with type elt = Ord.t = struct
  type elt = Ord.t

  type t = elt t1

  let singleton = singleton

  (* Insertion of one element *)
  let min_elt_opt = min_elt_opt

  let max_elt_opt = max_elt_opt

  let min_elt = min_elt

  let max_elt = max_elt

  let elements = elements

  let cardinal = cardinal

  let is_empty = is_empty

  let empty = empty

  let choose = min_elt

  let choose_opt = min_elt_opt

  let rec add x t =
    match t with
    | Empty -> singleton x
    | Leaf v ->
      let c = Ord.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        unsafe_node ~l:(singleton x) ~v ~r:empty
      else
        unsafe_node ~l:t ~v:x ~r:empty
    | Node { l; v; r; _ } as t ->
      let c = Ord.compare x v in
      if c = 0 then
        t
      else if c < 0 then
        let ll = add x l in
        if l == ll then
          t
        else
          bal ll v r
      else
        let rr = add x r in
        if r == rr then
          t
        else
          bal l v rr

  let ( @> ) = add
  (* Splitting.  split x s returns a triple (l, present, r) where
      - l is the set of elements of s that are < x
      - r is the set of elements of s that are > x
      - present is false if s contains no element equal to x,
        or true if s contains an element equal to x. *)

  let rec split x tree =
    match tree with
    | Empty -> (empty, false, empty)
    | Leaf v ->
      let c = Ord.compare x v in
      if c = 0 then
        (empty, true, empty)
      else if c < 0 then
        (empty, false, tree)
      else
        (tree, false, empty)
    | Node { l; v; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then
        (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split x l in
        (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = split x r in
        (join l v lr, pres, rr)

  (* Implementation of the set operations *)

  let rec mem x = function
    | Empty -> false
    | Leaf v ->
      let c = Ord.compare x v in
      c = 0
    | Node { l; v; r; _ } ->
      let c = Ord.compare x v in
      c = 0
      || mem
           x
           ( if c < 0 then
             l
           else
             r
           )

  let rec remove x tree =
    match tree with
    | Empty -> empty
    | Leaf v ->
      let c = Ord.compare x v in
      if c = 0 then
        empty
      else
        tree
    | Node { l; v; r; _ } as t ->
      let c = Ord.compare x v in
      if c = 0 then
        merge l r
      else if c < 0 then
        let ll = remove x l in
        if l == ll then
          t
        else
          bal ll v r
      else
        let rr = remove x r in
        if r == rr then
          t
        else
          bal l v rr

  let rec union s1 s2 =
    match (s1, s2) with
    | (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Leaf v, s2) -> add v s2
    | (s1, Leaf v) -> add v s1
    | (Node { l = l1; v = v1; r = r1; h = h1 }, Node { l = l2; v = v2; r = r2; h = h2 }) ->
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
    | (Empty, _) -> empty
    | (_, Empty) -> empty
    | (Leaf v, _) ->
      if mem v s2 then
        s1
      else
        empty
    | (Node { l = l1; v = v1; r = r1; _ }, t2) ->
      (match split v1 t2 with
      | (l2, false, r2) -> concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) -> join (inter l1 l2) v1 (inter r1 r2))

  (* Same as split, but compute the left and right subtrees
     only if the pivot element is not in the set.  The right subtree
     is computed on demand. *)

  type split_bis =
    | Found
    | NotFound of t * (unit -> t)

  let rec split_bis x = function
    | Empty -> NotFound (empty, (fun () -> empty))
    | Leaf v ->
      let c = Ord.compare x v in
      if c = 0 then
        Found
      else
        NotFound (empty, (fun () -> empty))
    | Node { l; v; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then
        Found
      else if c < 0 then
        match split_bis x l with
        | Found -> Found
        | NotFound (ll, rl) -> NotFound (ll, (fun () -> join (rl ()) v r))
      else (
        match split_bis x r with
        | Found -> Found
        | NotFound (lr, rr) -> NotFound (join l v lr, rr)
      )

  let rec disjoint s1 s2 =
    match (s1, s2) with
    | (Empty, _)
    | (_, Empty) ->
      true
    | (Leaf v, s)
    | (s, Leaf v) ->
      not (mem v s)
    | (Node { l = l1; v = v1; r = r1; _ }, t2) ->
      if s1 == s2 then
        false
      else (
        match split_bis v1 t2 with
        | NotFound (l2, r2) -> disjoint l1 l2 && disjoint r1 (r2 ())
        | Found -> false
      )

  let rec diff s1 s2 =
    match (s1, s2) with
    | (Empty, _) -> empty
    | (t1, Empty) -> t1
    | (Leaf v, _) ->
      if mem v s2 then
        empty
      else
        s1
    | (Node { l = l1; v = v1; r = r1; _ }, t2) ->
      (match split v1 t2 with
      | (l2, false, r2) -> join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) -> concat (diff l1 l2) (diff r1 r2))

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
    | (Leaf v1, Leaf v2) ->
      let c = Ord.compare v1 v2 in
      if c = 0 then
        true
      else
        false
    | (Node { v = v1; h; _ }, Leaf v2) ->
      h = 1
      && (* conservative here *)
      Ord.compare v1 v2 = 0
    | (Leaf v1, Node { l = l2; v = v2; r = r2; _ }) ->
      let c = Ord.compare v1 v2 in
      if c = 0 then
        true
      else if c < 0 then
        subset s1 l2
      else
        subset s1 r2
    | (Node { l = l1; v = v1; r = r1; _ }, (Node { l = l2; v = v2; r = r2; _ } as t2)) ->
      let c = Ord.compare v1 v2 in
      if c = 0 then
        subset l1 l2 && subset r1 r2
      else if c < 0 then
        (* Better to keep invariant here, since our unsafe code relies on such invariant
         *)
        subset (unsafe_node ~l:l1 ~v:v1 ~r:empty) l2 && subset r1 t2
      else
        subset (unsafe_node ~l:empty ~v:v1 ~r:r1) r2 && subset l1 t2

  let rec iter f = function
    | Empty -> ()
    | Leaf v -> f v
    | Node { l; v; r; _ } ->
      iter f l;
      f v;
      iter f r

  let rec fold f s accu =
    match s with
    | Empty -> accu
    | Leaf v -> f v accu
    | Node { l; v; r; _ } -> fold f r (f v (fold f l accu))

  let rec for_all p = function
    | Empty -> true
    | Leaf v -> p v
    | Node { l; v; r; _ } -> p v && for_all p l && for_all p r

  let rec exists p = function
    | Empty -> false
    | Leaf v -> p v
    | Node { l; v; r; _ } -> p v || exists p l || exists p r

  let rec filter p tree =
    match tree with
    | Empty -> empty
    | Leaf v ->
      let pv = p v in
      if pv then
        tree
      else
        empty
    | Node { l; v; r; _ } as t ->
      (* call [p] in the expected left-to-right order *)
      let l' = filter p l in
      let pv = p v in
      let r' = filter p r in
      if pv then
        if l == l' && r == r' then
          t
        else
          join l' v r'
      else
        concat l' r'

  let rec partition p tree =
    match tree with
    | Empty -> (empty, empty)
    | Leaf v ->
      let pv = p v in
      if pv then
        (tree, empty)
      else
        (empty, tree)
    | Node { l; v; r; _ } ->
      (* call [p] in the expected left-to-right order *)
      let (lt, lf) = partition p l in
      let pv = p v in
      let (rt, rf) = partition p r in
      if pv then
        (join lt v rt, concat lf rf)
      else
        (concat lt rt, join lf v rf)

  let rec find x = function
    | Empty -> raise Not_found
    | Leaf v ->
      let c = Ord.compare x v in
      if c = 0 then
        v
      else
        raise Not_found
    | Node { l; v; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then
        v
      else
        find
          x
          ( if c < 0 then
            l
          else
            r
          )

  let rec find_opt x = function
    | Empty -> None
    | Leaf v ->
      let c = Ord.compare x v in
      if c = 0 then
        Some v
      else
        None
    | Node { l; v; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then
        Some v
      else
        find_opt
          x
          ( if c < 0 then
            l
          else
            r
          )

  let try_join l v r =
    (* [join l v r] can only be called when (elements of l < v <
       elements of r); use [try_join l v r] when this property may
       not hold, but you hope it does hold in the common case *)
    if (is_empty l || Ord.compare (max_elt l) v < 0) && (is_empty r || Ord.compare v (min_elt r) < 0)
    then
      join l v r
    else
      union l (add v r)

  let rec map f tree =
    match tree with
    | Empty -> empty
    | Leaf v ->
      let v' = f v in
      if v == v' then
        tree
      else
        singleton v'
    | Node { l; v; r; _ } as t ->
      (* enforce left-to-right evaluation order *)
      let l' = map f l in
      let v' = f v in
      let r' = map f r in
      if l == l' && v == v' && r == r' then
        t
      else
        try_join l' v' r'

  let of_list l =
    match l with
    | [] -> empty
    | [x0] -> singleton x0
    | [x0; x1] -> x1 @> singleton x0
    | [x0; x1; x2] -> x2 @> x1 @> singleton x0
    | [x0; x1; x2; x3] -> x3 @> x2 @> x1 @> singleton x0
    | [x0; x1; x2; x3; x4] -> x4 @> x3 @> x2 @> x1 @> singleton x0
    | _ -> of_sorted_list (List.sort_uniq Ord.compare l)

  let to_seq = to_seq

  let make_pp pp_key fmt iset =
    Format.fprintf fmt "@[<2>{";
    let elements = elements iset in
    (match elements with
    | [] -> ()
    | _ -> Format.fprintf fmt " ");
    ignore
      (List.fold_left
         (fun sep s ->
           if sep then Format.fprintf fmt ";@ ";
           pp_key fmt s;
           true)
         false
         elements
      );
    (match elements with
    | [] -> ()
    | _ -> Format.fprintf fmt " ");
    Format.fprintf fmt "@,}@]"
end
