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

let _LINE_WIDTH = 16

type t = {
  chunks: Chunk.t list;
  rules: Rule.t list;
  cost: int;
  overflow: int;
}

let make chunks =
  let rules = (List.map chunks ~f:(fun c -> c.Chunk.rule)) in
  let rules = RuleSet.elements (RuleSet.of_list rules) in

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

  (* keep track of current length, cost of this state, total overflow chars *)
  let (len, cost, overflow)  =
    List.fold_left chunks ~init:acc ~f:(fun (len, cost, overflow) c ->
      let len, cost, overflow = if Chunk.has_split_before c then
        let overflow = overflow + (get_overflow len) in
        let len = 0 in (* get_block_indent *)
        let cost = cost + Chunk.get_span_split_cost c in
        len, cost, overflow
      else
        len, cost, overflow
      in

      let len = len + (String.length c.Chunk.text) in
      len, cost, overflow
    ) in

  (* calculate the overflow of the last chunk *)
  let overflow = overflow + (get_overflow len) in

  (* add to cost the cost of all rules that are split *)
  let cost = cost + (
    List.fold_left rules ~init:0 ~f:(fun acc r ->
      if (Rule.is_split r) then
        acc + (Rule.get_cost r)
      else
        acc
    )
  ) in

  { chunks; rules; cost; overflow; }

let compare s1 s2 =
  if s1.overflow < s2.overflow then -1
  else if s1.overflow = s2.overflow then begin
    if s1.cost < s2.cost then -1
    else if s1.cost = s2.cost then 0
    else 1
  end
  else 1

let __debug s =
  let rule_strings = List.map s.rules ~f:Rule.to_string in
  let rule_str = "[" ^ (String.concat "," rule_strings) ^ "]" in
  (string_of_int s.overflow) ^ "," ^ (string_of_int s.cost) ^ " " ^ rule_str
