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
  | Simple
  | Argument

type t = {
  id: int;
  kind: kind;
  dependents: int list;
}

let _nextid = ref 0
let _get_next_id () =
  let id = !_nextid in
  _nextid := !_nextid + 1;
  id

let rule_map = ref IMap.empty

let new_rule kind =
  let r = { id = _get_next_id (); kind; dependents = [] } in
  rule_map := IMap.add r.id r !rule_map;
  r.id

let simple_rule () =
  new_rule Simple

let argument_rule () =
  new_rule Argument

let is_split _id v =
  match v with
    | None
    | Some 0 -> false
    | _ -> true

let get_cost id =
  let r = IMap.find_unsafe id !rule_map in
  match r.kind with
    | Simple -> 1
    | Argument -> 1

let get_possible_values _id =
  [0; 1]

let cares_about_children r =
  match r.kind with
    | Simple -> false
    | Argument -> true

let compare r1 r2 =
  if r1.id < r2.id then -1
  else if r1.id = r2.id then 0
  else 1

let to_string id =
  let r = IMap.find_unsafe id !rule_map in
  let kind = match r.kind with
    | Simple -> "Simple"
    | Argument -> "Argument"
  in
  kind
