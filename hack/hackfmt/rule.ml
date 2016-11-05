(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type kind =
  | N
  | Simple
  | Argument

type t = {
  kind: kind;
  value: int option;
  max_value: int;
  id: int; (* need some way to compare rules for equivalency *)
}

let _nextid = ref 0
let get_id () =
  let id = !_nextid in
  _nextid := !_nextid + 1;
  id

let __debug_value r =
  match r.value with
    | None -> "none"
    | Some d -> string_of_int d

let to_string r =
  let kind = match r.kind with
    | N -> "N"
    | Simple -> "Simple"
    | Argument -> "Argument"
  in
  kind ^ " " ^ (__debug_value r)

let null_rule () =
  { kind = N; value = None; max_value = 0; id = -1; }

let simple_rule () =
  { kind = Simple; value = None; max_value = 1; id = get_id (); }

let argument_rule () =
  { kind = Argument; value = None; max_value = 1; id = get_id (); }

let is_split r =
  match r.value with
    | None
    | Some 0 -> false
    | _ -> true

let get_cost r =
  match r.kind with
    | N -> 0
    | Simple -> 1
    | Argument -> 1

let compare r1 r2 =
  if r1.id < r2.id then -1
  else if r1.id = r2.id then 0
  else 1
