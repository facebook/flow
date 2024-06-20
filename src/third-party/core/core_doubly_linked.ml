include Core_doubly_linked_intf

(* INVARIANT: This exception is raised if a list is mutated during a pending iteration.

   This invariant is guaranteed by the Header and Elt modules in conjunction.  All
   downstream code in this module need not be concerned with this invariant. *)
exception Attempt_to_mutate_list_during_iteration

let phys_equal = ( == )

module Header : sig
  type t

  val create : unit -> t

  val equal : t -> t -> bool

  val incr_length : by:int -> t -> unit

  val check_no_pending_iterations : t -> unit
end = struct
  type s = {
    mutable length: int;
    pending_iterations: int;
  }

  type t = s Core_union_find.t

  let create () = Core_union_find.create { length = 1; pending_iterations = 0 }

  let equal (t1 : t) t2 = Core_union_find.same_class t1 t2

  let union_find_get__check_no_pending_iterations t =
    let s = Core_union_find.get t in
    if s.pending_iterations > 0 then
      raise Attempt_to_mutate_list_during_iteration
    else
      s

  let check_no_pending_iterations t = ignore (union_find_get__check_no_pending_iterations t : s)

  let incr_length ~by:n t =
    let s = union_find_get__check_no_pending_iterations t in
    s.length <- s.length + n
end

module Elt : sig
  type 'a t

  val header : 'a t -> Header.t

  val equal : 'a t -> 'a t -> bool

  val create : 'a -> 'a t

  val value : 'a t -> 'a

  val set : 'a t -> 'a -> unit

  val unlink : 'a t -> unit

  val split_or_splice_before : 'a t -> 'a t -> unit

  val split_or_splice_after : 'a t -> 'a t -> unit

  val insert_after : 'a t -> 'a -> 'a t

  val insert_before : 'a t -> 'a -> 'a t

  val unlink_before : 'a t -> 'a t

  val next : 'a t -> 'a t
end = struct
  type 'a t = {
    mutable value: 'a;
    mutable prev: 'a t;
    mutable next: 'a t;
    mutable header: Header.t;
  }

  let equal = phys_equal

  let next t = t.next

  let header t = t.header

  let create_aux v header =
    let rec t = { value = v; prev = t; next = t; header } in
    t

  let is_singleton t = equal t t.prev

  let create v = create_aux v (Header.create ())

  let value t = t.value

  let set t v = t.value <- v

  (*
     [split_or_splice] is sufficient as the lone primitive for
     accomplishing all pointer updates on cyclic loops of list nodes.
     It takes two "gaps" between adjacent linked list nodes.  If the gaps
     point into the same list, the result is that it will be split into
     two lists afterwards.  If the gaps point into different lists, the
     result is that they will be spliced together into one list afterwards.

     {v
       Before                      After
           -----+        +-----         -----+               +-----
              A |  <-->  | B               A |  <---   --->  | B
           -----+        +-----         -----+      \ /      +-----
                                                     X
           -----+        +-----         -----+      / \      +-----
              C |  <-->  | D               C |  <---   --->  | D
           -----+        +-----         -----+               +-----
     v} *)

  let unsafe_split_or_splice ~prev1:a ~next1:b ~prev2:c ~next2:d =
    a.next <- d;
    d.prev <- a;
    c.next <- b;
    b.prev <- c

  let unsafe_split_or_splice_after t1 t2 =
    unsafe_split_or_splice ~next1:t1.next ~prev1:t1.next.prev ~next2:t2.next ~prev2:t2.next.prev

  let unsafe_split_or_splice_before t1 t2 =
    unsafe_split_or_splice ~prev1:t1.prev ~next1:t1.prev.next ~prev2:t2.prev ~next2:t2.prev.next

  let check_two_nodes_no_pending_iterations t1 t2 =
    Header.check_no_pending_iterations t1.header;
    if not (Header.equal t1.header t2.header) then Header.check_no_pending_iterations t2.header

  (* We redefine safe versions for export *)
  let split_or_splice_after t1 t2 =
    check_two_nodes_no_pending_iterations t1 t2;
    unsafe_split_or_splice_after t1 t2

  let split_or_splice_before t1 t2 =
    check_two_nodes_no_pending_iterations t1 t2;
    unsafe_split_or_splice_before t1 t2

  let insert_before t v =
    Header.incr_length t.header ~by:1;
    let node = create_aux v t.header in
    unsafe_split_or_splice_before t node;
    node

  let insert_after t v =
    Header.incr_length t.header ~by:1;
    let node = create_aux v t.header in
    unsafe_split_or_splice_after t node;
    node

  let dummy_header = Header.create ()

  let unlink_before t =
    let node = t.prev in
    if is_singleton node then
      node
    else (
      Header.incr_length t.header ~by:(-1);
      unsafe_split_or_splice_before t node;
      node.header <- dummy_header;
      node
    )

  let unlink_after t =
    let node = t.next in
    if is_singleton node then
      node
    else (
      Header.incr_length t.header ~by:(-1);
      unsafe_split_or_splice_after t node;
      node.header <- dummy_header;
      node
    )

  let unlink t = ignore (unlink_after t.prev : _ t)
end

type 'a t = 'a Elt.t option ref

let create (type a) () : a t = ref None

let of_list = function
  | [] -> create ()
  | x :: xs ->
    let first = Elt.create x in
    let _last = Base.List.fold xs ~init:first ~f:(fun a b -> Elt.insert_after a b) in
    ref (Some first)

let of_array = function
  | [||] -> create ()
  | arr ->
    let first = Elt.create arr.(0) in
    let rec loop arr elt i =
      if i < Array.length arr then loop arr (Elt.insert_after elt arr.(i)) (i + 1)
    in
    loop arr first 1;
    ref (Some first)

let clear t = t := None

exception Elt_does_not_belong_to_list

let insert_after t elt v =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Header.equal (Elt.header first) (Elt.header elt) then
      Elt.insert_after elt v
    else
      raise Elt_does_not_belong_to_list

let insert_before t elt v =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Elt.equal elt first then (
      let new_elt = Elt.insert_before first v in
      t := Some new_elt;
      new_elt
    ) else if Header.equal (Elt.header first) (Elt.header elt) then
      Elt.insert_before elt v
    else
      raise Elt_does_not_belong_to_list

let insert_empty t v =
  let new_elt = Elt.create v in
  t := Some new_elt;
  new_elt

let insert_last t v =
  match !t with
  | None -> insert_empty t v
  | Some first -> Elt.insert_before first v

let insert_first t v =
  match !t with
  | None -> insert_empty t v
  | Some first ->
    let new_elt = Elt.insert_before first v in
    t := Some new_elt;
    new_elt

let remove_last t =
  match !t with
  | None -> None
  | Some first ->
    let last = Elt.unlink_before first in
    if Elt.equal first last then t := None;
    Some (Elt.value last)

let remove_first t =
  match !t with
  | None -> None
  | Some first ->
    let second = Elt.next first in
    Elt.unlink first;
    t :=
      if Elt.equal first second then
        None
      else
        Some second;
    Some (Elt.value first)

let remove t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Elt.equal elt first then
      ignore (remove_first t : _ option)
    else if Header.equal (Elt.header first) (Elt.header elt) then
      Elt.unlink elt
    else
      raise Elt_does_not_belong_to_list

exception Invalid_move__elt_equals_anchor

let move_before t elt ~anchor =
  if Elt.equal anchor elt then raise Invalid_move__elt_equals_anchor;
  if Header.equal (Elt.header anchor) (Elt.header elt) then
    match !t with
    | None -> raise Elt_does_not_belong_to_list
    | Some first ->
      if Header.equal (Elt.header first) (Elt.header elt) then (
        (* unlink [elt] *)
        let after_elt = Elt.next elt in
        Elt.split_or_splice_before elt after_elt;
        let first =
          if Elt.equal first elt then (
            t := Some after_elt;
            after_elt
          ) else
            first
        in
        (* splice [elt] in before [anchor] *)
        Elt.split_or_splice_before anchor elt;
        if Elt.equal first anchor then t := Some elt
      ) else
        raise Elt_does_not_belong_to_list
  else
    raise Elt_does_not_belong_to_list

let move_to_front t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first -> if not (Elt.equal elt first) then move_before t elt ~anchor:first

let move_after t elt ~anchor =
  if Elt.equal anchor elt then raise Invalid_move__elt_equals_anchor;
  if Header.equal (Elt.header anchor) (Elt.header elt) then
    match !t with
    | None -> raise Elt_does_not_belong_to_list
    | Some first ->
      if Header.equal (Elt.header first) (Elt.header elt) then (
        (* unlink [elt] *)
        let after_elt = Elt.next elt in
        Elt.split_or_splice_before elt after_elt;
        if Elt.equal first elt then t := Some after_elt;
        (* splice [elt] in after [anchor] *)
        Elt.split_or_splice_after anchor elt
      ) else
        raise Elt_does_not_belong_to_list
  else
    raise Elt_does_not_belong_to_list
