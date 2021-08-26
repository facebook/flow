(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
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

  val choose : 'a t -> key * 'a

  val choose_opt : 'a t -> (key * 'a) option

  val split : key -> 'a t -> 'a t * 'a option * 'a t

  val find : key -> 'a t -> 'a

  val find_opt : key -> 'a t -> 'a option

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end

open Flow_map_common

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
          (if c < 0 then
            l
          else
            r)

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
          (if c < 0 then
            l
          else
            r)

  let rec mem x = function
    | Empty -> false
    | Leaf { v; _ } -> Ord.compare x v = 0
    | Node { l; v; r; _ } ->
      let c = Ord.compare x v in
      c = 0
      || mem
           x
           (if c < 0 then
             l
           else
             r)

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
        Node { l = l2; v = v2; d = d2; r = r2; h = h2 } ) ->
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
end
