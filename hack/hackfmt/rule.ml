(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

type kind =
  | Simple
  | Always
  | Argument

type t = {
  id: int;
  kind: kind;
}

let _nextid = ref 0
let _get_next_id () =
  let id = !_nextid in
  _nextid := !_nextid + 1;
  id

let rule_map = ref IMap.empty
let dependency_map = ref IMap.empty (* child_rule -> parent_rule list *)

let get_kind id =
  let r = IMap.find_unsafe id !rule_map in
  r.kind

let get_rule_count () =
  IMap.cardinal !rule_map

let constrain_rules rvm rule_list =
  List.fold_left rule_list ~init:rvm ~f:(fun rvm rule_id ->
    match get_kind rule_id with
      | Argument -> IMap.add rule_id 1 rvm
      | _ -> raise (Failure "unsupported rule type")
  )

let get_initial_rvm () =
  IMap.fold (fun rule_id r acc ->
    match r.kind with
      | Always ->
        let acc = IMap.add rule_id 1 acc in
        if IMap.mem rule_id !dependency_map then begin
          constrain_rules acc (IMap.find_unsafe rule_id !dependency_map)
        end else
          acc
      | _ -> acc
  ) !rule_map IMap.empty

let is_dependency_satisfied par_kind par_val child_val =
  match par_kind, par_val, child_val with
    | Argument, None, 1 -> false
    | Argument, Some 0, 1 -> false
    | _ -> true

let is_rule_value_map_valid rvm =
  let is_valid = IMap.fold (fun rule_id v is_valid ->
    if not (IMap.mem rule_id !dependency_map) then
      is_valid
    else
    let parent_list = IMap.find_unsafe rule_id !dependency_map in
    List.fold_left parent_list ~init:is_valid ~f:(fun is_valid par_id ->
      if not is_valid then
        is_valid
      else
      let par_rule = IMap.find_unsafe par_id !rule_map in
      let par_value = IMap.get par_id rvm in
      is_dependency_satisfied par_rule.kind par_value v
    )
  ) rvm true in
  is_valid

let new_rule kind =
  let r = { id = _get_next_id (); kind; } in
  rule_map := IMap.add r.id r !rule_map;
  r.id

let simple_rule () =
  new_rule Simple

let always_rule () =
  new_rule Always

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
    | Always -> 0
    | Argument -> 1

let get_possible_values _id =
  [0; 1]

let cares_about_children r =
  match r.kind with
    | Simple -> false
    | Always -> false
    | Argument -> true

let mark_dependencies rule_ids child =
  List.iter rule_ids ~f:(fun id ->
    let rule = IMap.find_unsafe id !rule_map in
    let dependency_list = IMap.get child !dependency_map in
    if cares_about_children rule then begin
      dependency_map := IMap.add child (match dependency_list with
        | Some l -> id :: l
        | None -> [id]
      ) !dependency_map
    end;
    ()
  )

let compare r1 r2 =
  if r1.id < r2.id then -1
  else if r1.id = r2.id then 0
  else 1

let to_string id =
  let r = IMap.find_unsafe id !rule_map in
  let kind = match r.kind with
    | Simple -> "Simple"
    | Always -> "Always"
    | Argument -> "Argument"
  in
  (string_of_int id) ^ " - " ^ kind
