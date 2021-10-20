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
 * - It adds a Leaf node to avoid excessive allocation for singleton map
 * - In the hot [bal] function when we know it has to be [Node], we do
 *   an unsafe cast to avoid some unneeded tests
 * - Functions not needing comparison functions are lifted outside functors
 * - Leaf node is cast as a tuple to save some allocations
 * - We add some utilities e.g, [adjust] and can add more relying on the
 *   internals in the future
 *)

type ('k, 'v) t0 =
  | Empty
  | Leaf of {
      v: 'k;
      d: 'v;
    }
  | Node of {
      h: int;
      v: 'k;
      d: 'v;
      l: ('k, 'v) t0;
      r: ('k, 'v) t0;
    }

type ('k, 'v) partial_node = {
  h: int;
  v: 'k;
  d: 'v;
  l: ('k, 'v) t0;
  r: ('k, 'v) t0;
}

type ('k, 'v) leaf_tuple = 'k * 'v

external ( ~!! ) : ('k, 'v) t0 -> ('k, 'v) leaf_tuple = "%identity"

external ( ~! ) : ('k, 'v) t0 -> ('k, 'v) partial_node = "%identity"

let[@inline] height = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node { h; _ } -> h

let singleton x d = Leaf { v = x; d }

let sorted_two_nodes_larger node v d = Node { l = node; v; d; r = Empty; h = 2 }

let sorted_two_nodes_smaller v d node = Node { l = Empty; v; d; r = node; h = 2 }

let create l x d r =
  let hl = height l in
  let hr = height r in
  let h =
    if hl >= hr then
      hl + 1
    else
      hr + 1
  in
  if h = 1 then
    singleton x d
  else
    Node { l; v = x; d; r; h }

(* The result can not be leaf *)
let node l x d r =
  let hl = height l in
  let hr = height r in
  let h =
    if hl >= hr then
      hl + 1
    else
      hr + 1
  in
  Node { l; v = x; d; r; h }

let bal l x d r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    let { l = ll; v = lv; d = ld; r = lr; _ } = ~!l in
    if height ll >= height lr then
      node ll lv ld (create lr x d r)
    else
      let { l = lrl; v = lrv; d = lrd; r = lrr; _ } = ~!lr in
      node (create ll lv ld lrl) lrv lrd (create lrr x d r)
  else if hr > hl + 2 then
    let { l = rl; v = rv; d = rd; r = rr; _ } = ~!r in
    if height rr >= height rl then
      node (create l x d rl) rv rd rr
    else
      let { l = rll; v = rlv; d = rld; r = rlr; _ } = ~!rl in
      node (create l x d rll) rlv rld (create rlr rv rd rr)
  else
    create l x d r

let empty = Empty

let[@inline] is_empty = function
  | Empty -> true
  | _ -> false

type ('key, 'a) enumeration =
  | End
  | More of 'key * 'a * ('key, 'a) t0 * ('key, 'a) enumeration

let rec cons_enum m e =
  match m with
  | Empty -> e
  | Leaf { v; d } -> More (v, d, empty, e)
  | Node { l; v; d; r; _ } -> cons_enum l (More (v, d, r, e))

let rec min_binding tree =
  match tree with
  | Empty -> raise Not_found
  | Leaf _ -> ~!!tree
  | Node { l = Empty; v; d; _ } -> (v, d)
  | Node { l; _ } -> min_binding l

let rec min_binding_from_node_unsafe tree =
  let { l; v; d; _ } = ~!tree in
  match l with
  | Empty -> (v, d)
  | Leaf _ -> ~!!l
  | Node _ -> min_binding_from_node_unsafe l

let rec min_binding_opt tree =
  match tree with
  | Empty -> None
  | Leaf { v; d } -> Some (v, d)
  | Node { l = Empty; v; d; _ } -> Some (v, d)
  | Node { l; _ } -> min_binding_opt l

let rec max_binding tree =
  match tree with
  | Empty -> raise Not_found
  | Leaf _ -> ~!!tree
  | Node { v; d; r = Empty; _ } -> (v, d)
  | Node { r; _ } -> max_binding r

let rec max_binding_opt tree =
  match tree with
  | Empty -> None
  | Leaf { v; d } -> Some (v, d)
  | Node { v; d; r = Empty; _ } -> Some (v, d)
  | Node { r; _ } -> max_binding_opt r

let rec remove_min_binding_from_node_unsafe tree =
  let { l; v; d; r; _ } = ~!tree in
  match l with
  | Empty -> r
  | Leaf _ -> bal Empty v d r
  | Node _ -> bal (remove_min_binding_from_node_unsafe l) v d r

(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_node node tree =
  match tree with
  | Empty -> node
  | Leaf { v; d } -> sorted_two_nodes_larger node v d
  | Node { l; v; d; r; _ } -> bal (add_min_node node l) v d r

let rec add_min_binding k x tree =
  match tree with
  | Empty -> singleton k x
  | Leaf _ -> sorted_two_nodes_smaller k x tree
  | Node { l; v; d; r; _ } -> bal (add_min_binding k x l) v d r

let rec add_max_node node tree =
  match tree with
  | Empty -> node
  | Leaf { v; d; _ } -> sorted_two_nodes_smaller v d node
  | Node { l; v; d; r; _ } -> bal l v d (add_max_node node r)

let rec add_max_binding k x tree =
  match tree with
  | Empty -> singleton k x
  | Leaf _ -> sorted_two_nodes_larger tree k x
  | Node { l; v; d; r; _ } -> bal l v d (add_max_binding k x r)

let internal_merge t1 t2 =
  match (t1, t2) with
  | (Empty, t) -> t
  | (t, Empty) -> t
  | (Leaf _, t) -> add_min_node t1 t
  | (t, Leaf _) -> add_max_node t2 t
  | (Node _, Node _) ->
    let (x, d) = min_binding_from_node_unsafe t2 in
    bal t1 x d (remove_min_binding_from_node_unsafe t2)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v d r =
  match (l, r) with
  | (Empty, _) -> add_min_binding v d r
  | (_, Empty) -> add_max_binding v d l
  | (Leaf _, Leaf _) -> Node { l; v; d; r; h = 2 }
  | (Leaf _, Node { l = rl; v = rv; d = rd; r = rr; h = rh }) ->
    if rh > 3 then
      bal (join l v d rl) rv rd rr
    else
      create l v d r
  | (Node { l = ll; v = lv; d = ld; r = lr; h = lh }, Leaf _) ->
    if lh > 3 then
      bal ll lv ld (join lr v d r)
    else
      create l v d r
  | ( Node { l = ll; v = lv; d = ld; r = lr; h = lh },
      Node { l = rl; v = rv; d = rd; r = rr; h = rh }
    ) ->
    if lh > rh + 2 then
      bal ll lv ld (join lr v d r)
    else if rh > lh + 2 then
      bal (join l v d rl) rv rd rr
    else
      create l v d r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
  | (Empty, t) -> t
  | (t, Empty) -> t
  | (Leaf _, t) -> add_min_node t1 t
  | (t, Leaf _) -> add_max_node t2 t
  | (Node _, Node _) ->
    let (x, d) = min_binding_from_node_unsafe t2 in
    join t1 x d (remove_min_binding_from_node_unsafe t2)

let concat_or_join t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2

let rec iter f = function
  | Empty -> ()
  | Leaf { v; d } -> f v d
  | Node { l; v; d; r; _ } ->
    iter f l;
    f v d;
    iter f r

let rec map f = function
  | Empty -> Empty
  | Leaf { v; d } ->
    let d' = f d in
    Leaf { v; d = d' }
  | Node { l; v; d; r; h } ->
    let l' = map f l in
    let d' = f d in
    let r' = map f r in
    Node { l = l'; v; d = d'; r = r'; h }

let rec mapi f = function
  | Empty -> Empty
  | Leaf { v; d } ->
    let d' = f v d in
    Leaf { v; d = d' }
  | Node { l; v; d; r; h } ->
    let l' = mapi f l in
    let d' = f v d in
    let r' = mapi f r in
    Node { l = l'; v; d = d'; r = r'; h }

let rec fold f m accu =
  match m with
  | Empty -> accu
  | Leaf { v; d } -> f v d accu
  | Node { l; v; d; r; _ } -> fold f r (f v d (fold f l accu))

let rec keys_aux accu tree =
  match tree with
  | Empty -> accu
  | Leaf { v; _ } -> v :: accu
  | Node { l; v; r; _ } -> keys_aux (v :: keys_aux accu r) l

let keys s = keys_aux [] s

let ordered_keys = keys

let rec for_all p = function
  | Empty -> true
  | Leaf { v; d } -> p v d
  | Node { l; v; d; r; _ } -> p v d && for_all p l && for_all p r

let rec exists p = function
  | Empty -> false
  | Leaf { v; d } -> p v d
  | Node { l; v; d; r; _ } -> p v d || exists p l || exists p r

let rec filter p tree =
  match tree with
  | Empty -> Empty
  | Leaf { v; d } ->
    if p v d then
      tree
    else
      empty
  | Node { l; v; d; r; _ } as m ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter p l in
    let pvd = p v d in
    let r' = filter p r in
    if pvd then
      if l == l' && r == r' then
        m
      else
        join l' v d r'
    else
      concat l' r'

let rec cardinal = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node { l; r; _ } -> cardinal l + 1 + cardinal r

let rec bindings_aux accu tree =
  match tree with
  | Empty -> accu
  | Leaf _ -> ~!!tree :: accu
  | Node { l; v; d; r; _ } -> bindings_aux ((v, d) :: bindings_aux accu r) l

let bindings s = bindings_aux [] s

type ('k, 'v) t1 = ('k, 'v) t0 =
  | Empty
  | Leaf of {
      v: 'k;
      d: 'v;
    }
  | Node of {
      h: int;
      v: 'k;
      d: 'v;
      l: ('k, 'v) t0;
      r: ('k, 'v) t0;
    }

module type OrderedType = sig
  type t

  val compare : t -> t -> int
  (* val equal : t -> t -> bool *)
end

module type S = sig
  type key

  type +'a t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val mem : key -> 'a t -> bool

  val add : key -> 'a -> 'a t -> 'a t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val adjust : key -> ('a option -> 'a) -> 'a t -> 'a t

  val singleton : key -> 'a -> 'a t

  (* when [remove k map] failed to remove [k], the original [map] is returned *)
  val remove : key -> 'a t -> 'a t

  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t

  val cardinal : 'a t -> int

  val bindings : 'a t -> (key * 'a) list

  val min_binding : 'a t -> key * 'a

  val min_binding_opt : 'a t -> (key * 'a) option

  val max_binding : 'a t -> key * 'a

  val max_binding_opt : 'a t -> (key * 'a) option

  val keys : 'a t -> key list

  val ordered_keys : 'a t -> key list

  val ident_map_key : ?combine:('a -> 'a -> 'a) -> (key -> key) -> 'a t -> 'a t

  val choose : 'a t -> key * 'a

  val choose_opt : 'a t -> (key * 'a) option

  val split : key -> 'a t -> 'a t * 'a option * 'a t

  val find : key -> 'a t -> 'a

  val find_opt : key -> 'a t -> 'a option

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end

module Make (Ord : OrderedType) : S with type key = Ord.t = struct
  type key = Ord.t

  type 'a t = (key, 'a) t1

  let rec add x data m =
    match m with
    | Empty -> singleton x data
    | Leaf { v; d } ->
      let c = Ord.compare x v in
      if c = 0 then
        if d == data then
          m
        else
          Leaf { v; d = data }
      else if c < 0 then
        sorted_two_nodes_smaller x data m
      else
        sorted_two_nodes_larger m x data
    | Node { l; v; d; r; h } as m ->
      let c = Ord.compare x v in
      if c = 0 then
        if d == data then
          m
        else
          Node { l; v = x; d = data; r; h }
      else if c < 0 then
        let ll = add x data l in
        if l == ll then
          m
        else
          bal ll v d r
      else
        let rr = add x data r in
        if r == rr then
          m
        else
          bal l v d rr

  let rec find x = function
    | Empty -> raise Not_found
    | Leaf { v; d } ->
      let c = Ord.compare x v in
      if c = 0 then
        d
      else
        raise Not_found
    | Node { l; v; d; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then
        d
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
    | Leaf { v; d } ->
      let c = Ord.compare x v in
      if c = 0 then
        Some d
      else
        None
    | Node { l; v; d; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then
        Some d
      else
        find_opt
          x
          ( if c < 0 then
            l
          else
            r
          )

  let rec mem x = function
    | Empty -> false
    | Leaf { v; _ } -> Ord.compare x v = 0
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
    | Empty -> tree
    | Leaf { v; _ } ->
      let c = Ord.compare x v in
      if c = 0 then
        empty
      else
        tree
    | Node { l; v; d; r; _ } as m ->
      let c = Ord.compare x v in
      if c = 0 then
        internal_merge l r
      else if c < 0 then
        let ll = remove x l in
        if l == ll then
          m
        else
          bal ll v d r
      else
        let rr = remove x r in
        if r == rr then
          m
        else
          bal l v d rr

  let rec adjust x (f : 'a option -> 'a) tree =
    match tree with
    | Empty ->
      let data = f None in
      singleton x data
    | Leaf { v; d } ->
      (* check *)
      let c = Ord.compare x v in
      if c = 0 then
        let data = f (Some d) in
        if d == data then
          tree
        else
          Leaf { v; d = data }
      else
        let data = f None in
        if c < 0 then
          sorted_two_nodes_smaller x data tree
        else
          sorted_two_nodes_larger tree x data
    | Node { l; v; d; r; h } as m ->
      let c = Ord.compare x v in
      if c = 0 then
        let data = f (Some d) in
        if d == data then
          m
        else
          Node { l; v = x; d = data; r; h }
      else if c < 0 then
        let ll = adjust x f l in
        if l == ll then
          m
        else
          bal ll v d r
      else
        let rr = adjust x f r in
        if r == rr then
          m
        else
          bal l v d rr

  let rec update x f tree =
    match tree with
    | Empty ->
      begin
        match f None with
        | None -> Empty
        | Some data -> singleton x data
      end
    | Leaf { v; d } ->
      (* check *)
      let c = Ord.compare x v in
      if c = 0 then
        match f (Some d) with
        | None -> empty (* It exists, None means deletion *)
        | Some data ->
          if d == data then
            tree
          else
            Leaf { v; d = data }
      else begin
        match f None with
        | None -> tree
        | Some data ->
          if c < 0 then
            sorted_two_nodes_smaller x data tree
          else
            sorted_two_nodes_larger tree x data
      end
    | Node { l; v; d; r; h } as m ->
      let c = Ord.compare x v in
      if c = 0 then
        match f (Some d) with
        | None -> internal_merge l r
        | Some data ->
          if d == data then
            m
          else
            Node { l; v = x; d = data; r; h }
      else if c < 0 then
        let ll = update x f l in
        if l == ll then
          m
        else
          bal ll v d r
      else
        let rr = update x f r in
        if r == rr then
          m
        else
          bal l v d rr

  let rec split x tree =
    match tree with
    | Empty -> (Empty, None, Empty)
    | Leaf { v; d } ->
      let c = Ord.compare x v in
      if c = 0 then
        (empty, Some d, empty)
      else if c < 0 then
        (empty, None, tree)
      else
        (tree, None, empty)
    | Node { l; v; d; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then
        (l, Some d, r)
      else if c < 0 then
        let (ll, pres, rl) = split x l in
        (ll, pres, join rl v d r)
      else
        let (lr, pres, rr) = split x r in
        (join l v d lr, pres, rr)

  let rec merge f s1 s2 =
    match (s1, s2) with
    | (Empty, Empty) -> Empty
    | (Leaf { v; d }, Empty) ->
      begin
        match f v (Some d) None with
        | None -> empty
        | Some data -> Leaf { v; d = data }
      end
    | (Empty, Leaf { v; d }) ->
      begin
        match f v None (Some d) with
        | None -> empty
        | Some data -> Leaf { v; d = data }
      end
    | (Leaf { v = v1; d = d1 }, Leaf _) ->
      let (l2, d2, r2) = split v1 s2 in
      concat_or_join (merge f empty l2) v1 (f v1 (Some d1) d2) (merge f empty r2)
    | (Node { l = l1; v = v1; d = d1; r = r1; h = h1 }, _) when h1 >= height s2 ->
      let (l2, d2, r2) = split v1 s2 in
      concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
    | (_, Node { l = l2; v = v2; d = d2; r = r2; _ }) ->
      let (l1, d1, r1) = split v2 s1 in
      concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
    | (Node _, (Empty | Leaf _)) -> assert false

  let rec union f s1 s2 =
    match (s1, s2) with
    | (Empty, s)
    | (s, Empty) ->
      s
    | (s, Leaf { v; d }) ->
      update
        v
        (fun d2 ->
          match d2 with
          | None -> Some d
          | Some d2 -> f v d2 d)
        s
    | (Leaf { v; d }, s) ->
      (* add v d s *)
      update
        v
        (fun d2 ->
          match d2 with
          | None -> Some d
          | Some d2 -> f v d d2)
        s
    | ( Node { l = l1; v = v1; d = d1; r = r1; h = h1 },
        Node { l = l2; v = v2; d = d2; r = r2; h = h2 }
      ) ->
      if h1 >= h2 then
        let (l2, d2, r2) = split v1 s2 in
        let l = union f l1 l2 and r = union f r1 r2 in
        match d2 with
        | None -> join l v1 d1 r
        | Some d2 -> concat_or_join l v1 (f v1 d1 d2) r
      else
        let (l1, d1, r1) = split v2 s1 in
        let l = union f l1 l2 and r = union f r1 r2 in
        (match d1 with
        | None -> join l v2 d2 r
        | Some d1 -> concat_or_join l v2 (f v2 d1 d2) r)

  let rec partition p tree =
    match tree with
    | Empty -> (Empty, Empty)
    | Leaf { v; d } ->
      if p v d then
        (tree, empty)
      else
        (empty, tree)
    | Node { l; v; d; r; _ } ->
      (* call [p] in the expected left-to-right order *)
      let (lt, lf) = partition p l in
      let pvd = p v d in
      let (rt, rf) = partition p r in
      if pvd then
        (join lt v d rt, concat lf rf)
      else
        (concat lt rt, join lf v d rf)

  let compare cmp m1 m2 =
    let rec compare_aux e1 e2 =
      match (e1, e2) with
      | (End, End) -> 0
      | (End, _) -> -1
      | (_, End) -> 1
      | (More (v1, d1, r1, e1), More (v2, d2, r2, e2)) ->
        let c = Ord.compare v1 v2 in
        if c <> 0 then
          c
        else
          let c = cmp d1 d2 in
          if c <> 0 then
            c
          else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in
    compare_aux (cons_enum m1 End) (cons_enum m2 End)

  let equal cmp m1 m2 =
    let rec equal_aux e1 e2 =
      match (e1, e2) with
      | (End, End) -> true
      | (End, _) -> false
      | (_, End) -> false
      | (More (v1, d1, r1, e1), More (v2, d2, r2, e2)) ->
        Ord.compare v1 v2 = 0 && cmp d1 d2 && equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in
    equal_aux (cons_enum m1 End) (cons_enum m2 End)

  let cardinal = cardinal

  let bindings = bindings

  let keys = keys

  let choose = min_binding

  let choose_opt = min_binding_opt

  let empty = empty

  let singleton = singleton

  let is_empty = is_empty

  let min_binding = min_binding

  let min_binding_opt = min_binding_opt

  let max_binding = max_binding

  let max_binding_opt = max_binding_opt

  let fold = fold

  let iter = iter

  let for_all = for_all

  let exists = exists

  let mapi = mapi

  let map = map

  let filter = filter

  let ordered_keys = keys

  let ident_map_key ?combine f map =
    let (map_, changed) =
      fold
        (fun key item (map_, changed) ->
          let new_key = f key in
          ( (* add ?combine new_key item map_ *)
            (match combine with
            | None -> add new_key item map_
            | Some combine ->
              adjust
                new_key
                (fun opt ->
                  match opt with
                  | None -> item
                  | Some old_value -> combine old_value item)
                map_),
            changed || new_key != key
          ))
        map
        (empty, false)
    in
    if changed then
      map_
    else
      map
end
