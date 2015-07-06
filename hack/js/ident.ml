(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)



type t = int

module IMap = Map.Make (struct
  type t = int
  let compare = (-)
end)

let debug = ref false
let foo = 0
let counter = ref 1
let trace = ref IMap.empty
let origin = ref IMap.empty
let origin_id = ref IMap.empty

let make x =
  incr counter ;
  let res = !counter in
  if !debug then
    trace := IMap.add res x !trace ;
  res

let fresh x =
  incr counter ;
  let res = !counter in
  if !debug then begin
    let name = IMap.find x !trace in
    trace := IMap.add res name !trace ;
  end;
  res

let tmp () =
  incr counter ;
  let res = !counter in
  if !debug then begin
    trace := IMap.add res ("__tmp"^string_of_int res) !trace ;
  end;
  res

let compare x y = x - y

let to_string x =
  let v =
    try IMap.find x !trace
    with Not_found -> "v"^string_of_int x
  in
  try
    let md_id = IMap.find x !origin in
    md_id ^ "." ^ v
  with Not_found -> v

let no_origin x =
  origin := IMap.remove x !origin ;
  origin_id := IMap.remove x !origin_id

let expand_name md x =
  let md_name = IMap.find md !trace in
  origin_id := IMap.add x md !origin_id ;
  origin := IMap.add x md_name !origin

let debug ?normalize:(f=fun x->x) x =
  let normalized_x = string_of_int (f x) in
  try IMap.find x !trace^"["^normalized_x^"]"
  with Not_found -> "tvar_"^normalized_x

let print x =
  Printf.printf "%s\n" (debug x)

let origin x =
  IMap.find x !origin

let origin_id x =
  IMap.find x !origin_id

let to_ustring x =
  let s = to_string x in
  s ^ string_of_int x

let full x =
  let v =
    try IMap.find x !trace
    with Not_found -> "v"^string_of_int x
  in
  let md = try origin x with Not_found -> "" in
  if md = ""
  then v
  else md ^ "_" ^ v

let set_name x y =
  trace := IMap.add x y !trace
