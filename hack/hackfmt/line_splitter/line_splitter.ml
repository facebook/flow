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

let expand_state state =
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
          if Rule.is_rule_value_map_valid next_rmv then
            next_rmv :: acc
          else
            acc
      )
  ) rule_id_set [] in

  List.iter next_rvms ~f:(fun rvm ->
    State_queue.add (Solve_state.make state.Solve_state.chunks rvm)
  );
  ()

let solve chunks =
  let count = ref 0 in
  let rec aux acc =
    count := !count + 1;
    if State_queue.is_empty () then
      acc
    else

    let state = State_queue.get_next () in
    let best =
      if Solve_state.compare state acc < 0 then state
      else acc
    in
    if best.Solve_state.overflow = 0 || !count > 50 then
      best
    else begin
      expand_state state;
      aux best;
    end
  in

  aux (State_queue.peek ())
