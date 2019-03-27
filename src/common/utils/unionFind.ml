(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a entry = {
  value: 'a;
  mutable parent: int;
  mutable next: int;
  mutable rank: int;
}
type 'a t = {
  indices: ('a, int) Hashtbl.t;
  mutable entries: 'a entry option array;
}

let default_size = 32

let make_with_size n = {
  indices = Hashtbl.create n;
  entries = Array.make n None;
}

let make () = make_with_size default_size

(* Number of entries *)
let entry_count t = Hashtbl.length t.indices

let add_unsafe t x i =
  Hashtbl.add t.indices x i;
  t.entries.(i) <- Some {
    value = x;
    parent = i;
    next = i;
    rank = 0;
  }

let get_entry t i = match t.entries.(i) with
  | Some x -> x
  | None -> raise Not_found

let get_value t i = (get_entry t i).value
let get_parent t i = (get_entry t i).parent
let get_next t i = (get_entry t i).next
let get_rank t i = (get_entry t i).rank

let set_parent t i parent = (get_entry t i).parent <- parent
let set_next t i next = (get_entry t i).next <- next
let set_rank t i rank = (get_entry t i).rank <- rank

let of_list lst =
  let len = List.length lst in
  let t = make_with_size (Utils_js.get_next_power_of_two len) in
  List.iteri (fun i x -> add_unsafe t x i) lst;
  t

let grow t =
  let old_arr_size = Array.length t.entries in
  let new_arr_size = old_arr_size * 2 in
  let new_entries = Array.make new_arr_size None in
  Array.blit t.entries 0 new_entries 0 old_arr_size;
  t.entries <- new_entries

(* Add the given value, and return its index *)
let add_ t x =
  let next_index = entry_count t in
  if Array.length t.entries = next_index then
    grow t;
  add_unsafe t x next_index;
  next_index

let add t x = ignore (add_ t x)

let lookup_or_add t x =
  if Hashtbl.mem t.indices x then
    Hashtbl.find t.indices x
  else
    add_ t x

let rec find_root_index t i =
  let parent = get_parent t i in
  if parent = i then
    i
  else
    let root_index = find_root_index t parent in
    (* path compression *)
    set_parent t i root_index;
    root_index

let union t x1 x2 =
  let i1 = lookup_or_add t x1 in
  let i2 = lookup_or_add t x2 in
  let i1_root = find_root_index t i1 in
  let i2_root = find_root_index t i2 in
  if i1_root <> i2_root then begin
    (* merge the circular linked lists *)
    let tmp = get_next t i1 in
    set_next t i1 (get_next t i2);
    set_next t i2 tmp;
    (* set the parent pointer according to rank *)
    let rank1 = get_rank t i1_root in
    let rank2 = get_rank t i2_root in
    if rank1 < rank2 then
      set_parent t i1_root i2_root
    else if rank1 > rank2 then
      set_parent t i2_root i1_root
    else
      set_parent t i1_root i2_root;
      set_rank t i2_root (rank2 + 1)
  end

let find t x =
  (* Raises Not_found if x is not present in the Hashtbl *)
  let i = Hashtbl.find t.indices x in
  let root_index = find_root_index t i in
  get_value t root_index

let rec members_of_index t initial_index i acc =
  let acc = (get_value t i) :: acc in
  let next = get_next t i in
  if next = initial_index then
    acc
  else
    members_of_index t initial_index next acc

let members t x =
  (* Raises Not_found if x is not present in the Hashtbl *)
  let i = Hashtbl.find t.indices x in
  members_of_index t i i []
