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

 type t = {
   rule_map: Rule.t IMap.t;
   dependency_map: (int list) IMap.t;
   next_id: int;
 }

 let make () =
  { rule_map = IMap.empty; dependency_map = IMap.empty; next_id = 0 }

let make_rule t rule_kind =
  let rule = { Rule.id = t.next_id; kind = rule_kind } in
  let t = { t with
    rule_map = IMap.add rule.Rule.id rule t.rule_map;
    next_id = t.next_id + 1;
  } in
  t, rule

let get_rule_count t =
  IMap.cardinal t.rule_map

let get_kind t id =
  let r = IMap.find_unsafe id t.rule_map in
  r.Rule.kind

let mark_dependencies t rule_ids child_id =
  let new_dep_map =
    List.fold_left rule_ids ~init:t.dependency_map ~f:(fun dep_map id ->
      let rule = IMap.find_unsafe id t.rule_map in
      if Rule.cares_about_children rule.Rule.kind then
        let dependency_list = IMap.get child_id dep_map in
        IMap.add child_id (match dependency_list with
          | Some l -> id :: l
          | None -> [id]
        ) dep_map
      else
        dep_map
    )
  in
  {t with dependency_map = new_dep_map }

let constrain_rules t rvm rule_list =
  List.fold_left rule_list ~init:rvm ~f:(fun rvm rule_id ->
    if Rule.cares_about_children (get_kind t rule_id) then
      IMap.add rule_id 1 rvm
    else
      rvm
  )

let get_initial_rvm t =
  IMap.fold (fun rule_id r acc ->
    match r.Rule.kind with
      | Rule.Always ->
        let acc = IMap.add rule_id 1 acc in
        if IMap.mem rule_id t.dependency_map then begin
          constrain_rules t acc (IMap.find_unsafe rule_id t.dependency_map)
        end else
          acc
      | _ -> acc
  ) t.rule_map IMap.empty

let is_dependency_satisfied par_kind par_val child_val =
  match par_kind, par_val, child_val with
    | Rule.Argument, None, 1 -> false
    | Rule.Argument, Some 0, 1 -> false
    | Rule.XHPExpression, None, 1 -> false
    | Rule.XHPExpression, Some 0, 1 -> false
    | _ -> true

let is_rule_value_map_valid t rvm =
  let is_valid = IMap.fold (fun rule_id v is_valid ->
    if not (IMap.mem rule_id t.dependency_map) then
      is_valid
    else
    let parent_list = IMap.find_unsafe rule_id t.dependency_map in
    List.fold_left parent_list ~init:is_valid ~f:(fun is_valid par_id ->
      if not is_valid then
        is_valid
      else
      let par_rule = IMap.find_unsafe par_id t.rule_map in
      let par_value = IMap.get par_id rvm in
      is_dependency_satisfied par_rule.Rule.kind par_value v
    )
  ) rvm true in
  is_valid

let dependency_map_to_string t =
  let kv_list = IMap.elements t.dependency_map in
  let str_list = List.map kv_list ~f:(fun (k, v_list) ->
    let v_strs = "[" ^ String.concat ", " (List.map v_list ~f:string_of_int) ^ "]" in
    string_of_int k ^ ": "  ^ v_strs
  ) in
  "{" ^ String.concat ", " str_list ^ "}"
