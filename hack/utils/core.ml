(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module List = struct
  include Core_list

  let rec fold_left_env env l ~init ~f = match l with
    | [] -> env, init
    | x :: xs ->
      let env, init = f env init x in
      fold_left_env env xs ~init ~f

  let rec map_env env xs ~f = match xs with
    | [] -> env, []
    | x :: xs ->
      let env, x = f env x in
      let env, xs = map_env env xs ~f in
      env, x :: xs

  let rev_map_env env xs ~f =
    let f2 env init x =
      let env, x = f env x in
      env, x :: init
    in
    fold_left_env env xs ~init:[] ~f:f2

  let rec map2_env env l1 l2 ~f = match l1, l2 with
    | [], [] -> env, []
    | [], _ | _, [] -> raise @@ Invalid_argument "map2_env"
    | x1 :: rl1, x2 :: rl2 ->
      let env, x = f env x1 x2 in
      let env, rl = map2_env env rl1 rl2 ~f in
      env, x :: rl
end
