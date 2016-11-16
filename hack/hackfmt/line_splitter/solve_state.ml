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

let _LINE_WIDTH = 80

type t = {
  chunk_group: Chunk_group.t;
  rvm: int IMap.t;
  nesting_set: ISet.t;
  cost: int;
  overflow: int;
}

let has_split_before_chunk c rvm =
  let rule_id = c.Chunk.rule in
  let value = IMap.get rule_id rvm in
  Rule.is_split rule_id value

let make chunk_group rvm =
  let { Chunk_group.chunks; block_indentation; _ } = chunk_group in
  let len = 0 in
  let cost = 0 in
  let overflow = 0 in
  let acc = len, cost, overflow in

  let get_overflow len = max (len - _LINE_WIDTH) 0 in

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
        let len = len + block_indentation in
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
        acc + (Rule.get_cost (Chunk_group.get_rule_kind chunk_group r_id))
      else
        acc
    ) rvm 0
  ) in

  { chunk_group; rvm; cost; overflow; nesting_set; }

let is_rule_bound t rule_id =
  IMap.mem rule_id t.rvm

let pick_best_state s1 s2 =
  let cmp = Pervasives.compare (s1.overflow, s1.cost) (s2.overflow, s2.cost) in
  if cmp < 0 then s1 else s2

let compare s1 s2 =
  Pervasives.compare (s1.cost, s1.overflow) (s2.cost, s2.overflow)

let __debug t =
  (* TODO: make a new rule strings string *)
  let rule_strings = List.map (IMap.bindings t.rvm) (fun (k, v) ->
    string_of_int k ^ ": " ^ string_of_int v
  ) in
  let rule_count = string_of_int (Chunk_group.get_rule_count t.chunk_group) in
  let rule_str = rule_count ^ " [" ^ (String.concat "," rule_strings) ^ "]" in
  (string_of_int t.overflow) ^ "," ^ (string_of_int t.cost) ^ " " ^ rule_str
