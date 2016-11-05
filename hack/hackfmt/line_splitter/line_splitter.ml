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
  Printf.printf "%s\n" (Solve_state.__debug state);

  let chunks = state.Solve_state.chunks in
  let rules = (List.map chunks ~f:(fun c -> c.Chunk.rule)) in
  let rules = RuleSet.elements (RuleSet.of_list rules) in

  let rec get_next_rule_states states processed remaining =
    let bind_values r =
      let rec bind_value acc r v =
        if v >= 0 then
          bind_value ({r with Rule.value = Some v;} :: acc) r (v - 1)
        else
          acc
      in
      match r.Rule.value with
        | None -> bind_value [] r r.Rule.max_value
        | Some _ -> []
    in
    match remaining with
      | [] -> states;
      | hd :: tl ->
        let new_rule_states = bind_values hd in
        let pro = List.rev processed in
        let states = List.map new_rule_states ~f:(fun s ->
          pro @ (s :: tl)
        ) @ states in
        get_next_rule_states states (hd :: processed) tl
  in

  let next_rule_states = get_next_rule_states [] [] rules in
  (* for each chunk update it with a new rule *)
  let get_next_states rules =
    let rules_map = List.fold_left rules ~init:IMap.empty ~f:(fun map r ->
      IMap.add r.Rule.id r map
    ) in
    let chunks = List.map chunks ~f:(
      fun c -> {c with Chunk.rule = IMap.find c.Chunk.rule.Rule.id rules_map}
    ) in
    Solve_state.make chunks
  in

  let next_states = List.map next_rule_states ~f:get_next_states in
  List.iter next_states ~f:(fun s ->
    State_queue.add s
  );
  ()

let solve () =
  let rec aux acc =
    if State_queue.is_empty () then
      acc
    else

    let state = State_queue.get_next () in
    let best =
      if Solve_state.compare state acc < 0 then state
      else acc
    in
    if best.Solve_state.overflow = 0 then
      best
    else begin
      expand_state best;
      aux best;
    end
  in

  aux (State_queue.peek ())
