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
      Node { l = rl; v = rv; d = rd; r = rr; h = rh } ) ->
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

let rec for_all p = function
  | Empty -> true
  | Leaf { v; d } -> p v d
  | Node { l; v; d; r; _ } -> p v d && for_all p l && for_all p r

let rec exists p = function
  | Empty -> false
  | Leaf { v; d } -> p v d
  | Node { l; v; d; r; _ } -> p v d || exists p l || exists p r

(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

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

