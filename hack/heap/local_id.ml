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

external hh_counter_next : unit -> int = "hh_counter_next"

let track_names = ref false
let trace = ref IMap.empty

let to_string x = string_of_int x

let to_int x = x

let get_name x =
  assert (!track_names);
  IMap.find_unsafe x !trace

let make x =
  let res = hh_counter_next () in
  if !track_names then trace := IMap.add res x !trace;
  res

let make_fake = Hashtbl.hash

let tmp () =
  let res = hh_counter_next () in
  if !track_names then begin
    trace := IMap.add res ("__tmp"^string_of_int res) !trace ;
  end;
  res

module Set = Set.Make(S)
module Map = MyMap.Make(S)
