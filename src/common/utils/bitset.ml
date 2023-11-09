(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = bytes [@@deriving show]

let bytes_size logical_size =
  if logical_size mod 8 = 0 then
    logical_size / 8
  else
    (logical_size / 8) + 1

let logical_index_to_actual_indices logical_index = (logical_index / 8, logical_index mod 8)

let all_zero logical_size = Bytes.make (bytes_size logical_size) (Char.chr 0)

let all_one logical_size =
  let bytes_size = bytes_size logical_size in
  let bitset = Bytes.make bytes_size (Char.chr 255) in
  if bytes_size > 0 && logical_size mod 8 <> 0 then
    (* We want to maintain the invariant that all the extra bytes are all zero *)
    Bytes.set_uint8 bitset (bytes_size - 1) ((1 lsl (logical_size mod 8)) - 1);
  bitset

let mem logical_index bitset =
  let (byte_idx, bit_idx) = logical_index_to_actual_indices logical_index in
  let byte = Bytes.get_uint8 bitset byte_idx in
  byte land (1 lsl bit_idx) <> 0

let set bitset logical_index =
  let (byte_idx, bit_idx) = logical_index_to_actual_indices logical_index in
  let byte = Bytes.get_uint8 bitset byte_idx in
  let byte = byte lor (1 lsl bit_idx) in
  Bytes.set_uint8 bitset byte_idx byte

let unset bitset logical_index =
  let (byte_idx, bit_idx) = logical_index_to_actual_indices logical_index in
  let byte = Bytes.get_uint8 bitset byte_idx in
  let byte = byte land lnot (1 lsl bit_idx) in
  Bytes.set_uint8 bitset byte_idx byte

let is_subset bit_set_a bit_set_b =
  let l = Bytes.length bit_set_a in
  if l <> Bytes.length bit_set_b then failwith "Comparing bitsets of different sizes";
  let rec loop i =
    if i >= l then
      true
    else
      let a = Bytes.get_uint8 bit_set_a i in
      let b = Bytes.get_uint8 bit_set_b i in
      (* If the union of a and b is equal to b, then a <= b *)
      if a lor b = b then
        loop (i + 1)
      else
        false
  in
  loop 0

let no_overlap bit_set_a bit_set_b =
  let l = Bytes.length bit_set_a in
  if l <> Bytes.length bit_set_b then failwith "Comparing bitsets of different sizes";
  let rec loop i =
    if i >= l then
      true
    else
      let a = Bytes.get_uint8 bit_set_a i in
      let b = Bytes.get_uint8 bit_set_b i in
      if a land b = 0 then
        loop (i + 1)
      else
        false
  in
  loop 0
