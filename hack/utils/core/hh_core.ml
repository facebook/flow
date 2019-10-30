(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module List = struct
  include Base.List

  let rec fold_left_env env l ~init ~f =
    match l with
    | [] -> (env, init)
    | x :: xs ->
      let (env, init) = f env init x in
      fold_left_env env xs ~init ~f

  let rev_map_env env xs ~f =
    let f2 env init x =
      let (env, x) = f env x in
      (env, x :: init)
    in
    fold_left_env env xs ~init:[] ~f:f2

  let map_env env xs ~f =
    let rec aux env xs counter =
      match xs with
      | [] -> (env, [])
      | [y1] ->
        let (env, z1) = f env y1 in
        (env, [z1])
      | [y1; y2] ->
        let (env, z1) = f env y1 in
        let (env, z2) = f env y2 in
        (env, [z1; z2])
      | [y1; y2; y3] ->
        let (env, z1) = f env y1 in
        let (env, z2) = f env y2 in
        let (env, z3) = f env y3 in
        (env, [z1; z2; z3])
      | [y1; y2; y3; y4] ->
        let (env, z1) = f env y1 in
        let (env, z2) = f env y2 in
        let (env, z3) = f env y3 in
        let (env, z4) = f env y4 in
        (env, [z1; z2; z3; z4])
      | [y1; y2; y3; y4; y5] ->
        let (env, z1) = f env y1 in
        let (env, z2) = f env y2 in
        let (env, z3) = f env y3 in
        let (env, z4) = f env y4 in
        let (env, z5) = f env y5 in
        (env, [z1; z2; z3; z4; z5])
      | y1 :: y2 :: y3 :: y4 :: y5 :: ys ->
        let (env, z1) = f env y1 in
        let (env, z2) = f env y2 in
        let (env, z3) = f env y3 in
        let (env, z4) = f env y4 in
        let (env, z5) = f env y5 in
        let (env, zs) =
          if counter > 1000 then
            let (env, zs) = rev_map_env env ys ~f in
            (env, List.rev zs)
          else
            aux env ys (counter + 1)
        in
        (env, z1 :: z2 :: z3 :: z4 :: z5 :: zs)
    in
    aux env xs 0

  let rec map2_env env l1 l2 ~f =
    match (l1, l2) with
    | ([], []) -> (env, [])
    | ([], _)
    | (_, []) ->
      raise @@ Invalid_argument "map2_env"
    | (x1 :: rl1, x2 :: rl2) ->
      let (env, x) = f env x1 x2 in
      let (env, rl) = map2_env env rl1 rl2 ~f in
      (env, x :: rl)

  let rec map3_env env l1 l2 l3 ~f =
    if length l1 <> length l2 || length l2 <> length l3 then
      raise @@ Invalid_argument "map3_env"
    else
      match (l1, l2, l3) with
      | ([], [], []) -> (env, [])
      | ([], _, _)
      | (_, [], _)
      | (_, _, []) ->
        raise @@ Invalid_argument "map3_env"
      | (x1 :: rl1, x2 :: rl2, x3 :: rl3) ->
        let (env, x) = f env x1 x2 x3 in
        let (env, rl) = map3_env env rl1 rl2 rl3 ~f in
        (env, x :: rl)

  let filter_map_env env xs ~f =
    let (env, l) = rev_map_env env xs ~f in
    (env, rev_filter_map l ~f:(fun x -> x))
end
