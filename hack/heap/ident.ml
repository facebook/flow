(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

external hh_counter_next : unit -> int = "hh_counter_next"

type t = int [@@deriving eq]

let compare x y = x - y

let track_names = ref false

let trace = ref IMap.empty

let tmp () =
  let res = hh_counter_next () in
  if !track_names then
    trace := IMap.add res ("__tmp" ^ string_of_int res) !trace;
  res

let to_string x =
  (try IMap.find_unsafe x !trace with Not_found -> "v" ^ string_of_int x)

let debug ?normalize:(f = (fun x -> x)) x =
  let normalized_x = string_of_int (f x) in
  try IMap.find_unsafe x !trace ^ "[" ^ normalized_x ^ "]"
  with Not_found -> "tvar_" ^ normalized_x

let get_name x =
  assert !track_names;
  IMap.find_unsafe x !trace

let set_name x y = trace := IMap.add x y !trace

let make x =
  let res = hh_counter_next () in
  if !track_names then set_name res x;
  res

let pp = Format.pp_print_int
