(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a t = {
  mutable arr: 'a option array;
  mutable size: int;
}

exception Out_of_bounds_set of string

let make size =
  {
    arr = Array.make size None;
    (* 0, not the given `size`. See the comment for this function in the `.mli` file. *)
    size = 0;
  }

let get arr i =
  if i < 0 || i >= arr.size then
    None
  else
    arr.arr.(i)

let change_capacity arr new_capacity =
  let new_array = Array.make new_capacity None in
  Array.blit arr.arr 0 new_array 0 arr.size;
  arr.arr <- new_array

let expand_if_needed arr =
  let old_capacity = Array.length arr.arr in
  if arr.size = old_capacity then
    let new_capacity = max (old_capacity * 2) 1 in
    change_capacity arr new_capacity

let set arr i x =
  if i >= arr.size || i < 0 then
    raise (Out_of_bounds_set (Printf.sprintf "Index: %d, size: %d" i arr.size));
  arr.arr.(i) <- Some x

let push arr elt =
  expand_if_needed arr;
  arr.arr.(arr.size) <- Some elt;
  arr.size <- arr.size + 1

let shrink arr = if arr.size <> Array.length arr.arr then change_capacity arr arr.size

let size arr = arr.size

let underlying_array_size_do_not_use arr = Array.length arr.arr

let to_hashtbl arr =
  let tbl = Hashtbl.create arr.size in
  for i = 0 to arr.size - 1 do
    match arr.arr.(i) with
    | Some v -> Hashtbl.add tbl v i
    | None -> ()
  done;
  tbl
