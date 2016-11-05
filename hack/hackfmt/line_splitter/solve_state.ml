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

let _LINE_WIDTH = 20

type t = {
  chunks: Chunk.t list;
  rvm: int IMap.t;
  nesting_set: ISet.t;
  cost: int;
  overflow: int;
}

let has_split_before_chunk c rvm =
  let rule_id = c.Chunk.rule in
  let value = IMap.get rule_id rvm in
  Rule.is_split rule_id value

let make chunks rvm =
  let len = 0 in
  let cost = 0 in
  let overflow = 0 in
  let acc = len, cost, overflow in

  let get_overflow len =
    if len > _LINE_WIDTH then
      len - _LINE_WIDTH
    else
      0
  in

  let nesting_set, _ =
    List.fold_left chunks ~init:(ISet.empty, ISet.empty)
      (* We only care about the first occurance of each nesting id *)
      ~f:(fun (nset, idset) c ->
        let nid = Chunk.get_nesting_id c in
        if ISet.mem nid idset then
          nset, idset
        else
        if has_split_before_chunk c rvm then
          ISet.add nid nset, ISet.add nid idset
        else
          nset, ISet.add nid idset
      )
  in

  (* keep track of current length, cost of this state, total overflow chars *)
  let (len, cost, overflow)  =
    List.fold_left chunks ~init:acc ~f:(fun (len, cost, overflow) c ->
      let len, cost, overflow = if has_split_before_chunk c rvm then
        let overflow = overflow + (get_overflow len) in
        let len = Nesting.get_indent c.Chunk.nesting nesting_set in
        let cost = cost + Chunk.get_span_split_cost c in
        len, cost, overflow
      else
        let len = if c.Chunk.space_if_not_split then len + 1 else len in
        len, cost, overflow
      in

      let len = len + (String.length c.Chunk.text) in
      len, cost, overflow
    ) in

  (* calculate the overflow of the last chunk *)
  let overflow = overflow + (get_overflow len) in

  (* add to cost the cost of all rules that are split *)
  let cost = cost + (
    IMap.fold (fun r_id v acc ->
      if (Rule.is_split r_id (Some v)) then
        acc + (Rule.get_cost r_id)
      else
        acc
    ) rvm 0
  ) in

  { chunks; rvm; cost; overflow; nesting_set; }

let compare s1 s2 =
  if s1.overflow < s2.overflow then -1
  else if s1.overflow = s2.overflow then begin
    if s1.cost < s2.cost then -1
    else if s1.cost = s2.cost then 0
    else 1
  end
  else 1

let __debug s =
  (* TODO: make a new rule strings string *)
  let rule_strings = IMap.fold (fun k v acc ->
    (string_of_int k ^ ": " ^ string_of_int v) :: acc
  ) s.rvm [] in
  let rule_count = string_of_int (Rule.get_rule_count ()) in
  let rule_str = rule_count ^ " [" ^ (String.concat "," rule_strings) ^ "]" in
  (string_of_int s.overflow) ^ "," ^ (string_of_int s.cost) ^ " " ^ rule_str
