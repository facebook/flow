(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module S = struct
  type t = int
  let compare x y = x - y
end

include S

let ctr = ref 1

let next () =
  incr ctr;
  !ctr

let track_names = ref false
let trace = ref IMap.empty

let to_string x =
  match IMap.get x !trace with
  | Some s -> s
  | None -> string_of_int x

let pp fmt x = Format.pp_print_string fmt (to_string x)

let to_int x = x

let get_name x =
  match IMap.get x !trace with
  | Some s -> s
  | None -> to_string x

let make x =
  let res = next () in
  if !track_names then trace := IMap.add res x !trace;
  res

(* `make` always returns a positive value. By multiplying the hash by -1 we
 * ensure that the value returned by `get` never overlaps with those returned
 * by `make` *)
let get x =
  let res = -(Hashtbl.hash x) in
  if !track_names then trace := IMap.add res x !trace;
  res

let tmp () =
  let res = next () in
  if !track_names then begin
    trace := IMap.add res ("__tmp"^string_of_int res) !trace ;
  end;
  res

module Set = Set.Make(S)
module Map = MyMap.Make(S)
