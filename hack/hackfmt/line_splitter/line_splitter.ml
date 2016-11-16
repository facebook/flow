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

let expand_state state_queue state =
  let rule_ids = List.map state.Solve_state.chunks ~f:(
    fun c -> c.Chunk.rule
  ) in
  let rule_id_set = (ISet.of_list rule_ids) in

  let next_rvms = ISet.fold (fun rule_id acc ->
    if IMap.mem rule_id state.Solve_state.rvm then
      acc
    else
      List.fold_left (Rule.get_possible_values rule_id) ~init:acc ~f:(
        fun acc v ->
          let next_rmv = IMap.add rule_id v state.Solve_state.rvm in
          if Rule_allocator.is_rule_value_map_valid state.Solve_state.ra next_rmv then
            next_rmv :: acc
          else
            acc
      )
  ) rule_id_set [] in

  List.fold_left next_rvms ~init:state_queue ~f:(fun q rvm ->
    let st = (Solve_state.make state.Solve_state.chunks rvm state.Solve_state.ra state.Solve_state.bi) in
    State_queue.add q st
  )

let solve queue =
  let count = ref 0 in
  let rec aux acc queue =
  count := !count + 1;
    if State_queue.is_empty queue then
      acc
    else

    let queue, state = State_queue.get_next queue in
    let best =
      if Solve_state.compare state acc < 0 then state
      else acc
    in
    if best.Solve_state.overflow = 0 || !count > 50 then
      best
    else begin
      let queue = expand_state queue state in
      aux best queue;
    end
  in
  aux (State_queue.peek queue) queue
