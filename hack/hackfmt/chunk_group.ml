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

type print_range =
  | No
  | All
  | Range of int * int
  | StartAt of int
  | EndAt of int

type t = {
  chunks: Chunk.t list;
  rule_map: Rule.t IMap.t;
  rule_dependency_map: (int list) IMap.t;
  block_indentation: int;
  print_range: print_range;
}

let get_rule_count t =
  IMap.cardinal t.rule_map

let get_rules t =
  (* TODO verify or log if there are unused rules *)
  List.map (IMap.bindings t.rule_map) ~f:fst

let get_rule_kind t id =
  let r = IMap.find_unsafe id t.rule_map in
  r.Rule.kind

let get_print_range_indicies t =
  match t.print_range with
    | No -> -1, -1
    | All -> 0, List.length t.chunks
    | Range (s, e) ->  s, e
    | StartAt s -> s, List.length t.chunks
    | EndAt e -> 0, e

let constrain_rules t rvm rule_list =
  let aux rule_id = Rule.cares_about_children (get_rule_kind t rule_id) in
  let rules_that_care = List.filter rule_list ~f:aux in
  List.fold rules_that_care ~init:rvm ~f:(fun acc k -> IMap.add k 1 acc)

let get_initial_rule_value_map t =
  let is_always_rule _k v = v.Rule.kind = Rule.Always in
  let always_rules = IMap.filter is_always_rule t.rule_map in
  let get_dependencies rule_id =
    try IMap.find_unsafe rule_id t.rule_dependency_map with Not_found -> [] in
  let constrain k _v acc = constrain_rules t acc (get_dependencies k) in
  let init_map = IMap.map (fun k -> 1) always_rules in
  IMap.fold constrain always_rules init_map

let is_dependency_satisfied parent_kind parent_val child_val =
  child_val <> 1 ||
  Option.value ~default:0 parent_val <> 0 ||
  (parent_kind <> Rule.Argument && parent_kind <> Rule.XHPExpression)

let is_rule_value_map_valid t rvm =
  let valid_map = IMap.mapi (fun rule_id v ->
    let parent_list = try IMap.find_unsafe rule_id t.rule_dependency_map
      with Not_found -> []
    in
    List.for_all parent_list ~f:(fun parent_id ->
      let parent_rule = IMap.find_unsafe parent_id t.rule_map in
      let parent_value = IMap.get parent_id rvm in
      is_dependency_satisfied parent_rule.Rule.kind parent_value v
    )
  ) rvm in
  List.for_all ~f:(fun x -> x) @@ List.map ~f:snd @@ IMap.bindings valid_map

let dependency_map_to_string t =
  let get_map_values map = List.map ~f:snd @@ IMap.bindings @@ map in
  let str_list = get_map_values @@ IMap.mapi (fun k v_list ->
    let values = List.map v_list ~f:string_of_int in
    string_of_int k ^ ": [" ^ String.concat ", " values ^ "]"
  ) t in
  "{" ^ String.concat ", " str_list ^ "}"
