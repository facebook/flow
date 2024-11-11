(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = int [@@deriving show]

(* Stay under Sys.int_size (31 under 32-bit) just to be safe.
 * This should be enough for all cases using bitsets in flow. *)
let max_size = 31

let assert_within_supported_size logical_size =
  if logical_size > max_size then
    failwith (Printf.sprintf "logical_size >= %d" max_size)
  else if logical_size < 0 then
    failwith "logical_size < 0"

let all_zero logical_size =
  assert_within_supported_size logical_size;
  0

let all_one logical_size =
  assert_within_supported_size logical_size;
  (1 lsl logical_size) - 1

let mem logical_index bitset = bitset land (1 lsl logical_index) <> 0

let set logical_index bitset = bitset lor (1 lsl logical_index)

let unset logical_index bitset = bitset land lnot (1 lsl logical_index)

let is_subset bit_set_a bit_set_b = bit_set_a lor bit_set_b = bit_set_b

let no_overlap bit_set_a bit_set_b = bit_set_a land bit_set_b = 0

let equal = Int.equal

let compare = Int.compare

let to_string = string_of_int
