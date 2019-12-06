(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module List = struct
  include Core_kernel.List

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
            (env, rev zs)
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

  let filter_map_env env xs ~f =
    let (env, l) = rev_map_env env xs ~f in
    (env, rev_filter_map l ~f:(fun x -> x))

  let rec replicate ~num x =
    match num with
    | 0 -> []
    | n when n < 0 ->
      raise @@ Invalid_argument (Printf.sprintf "List.replicate was called with %d argument" n)
    | _ -> x :: replicate ~num:(num - 1) x
end

module Option = struct
  include Core_kernel.Option

  let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
   fun pp_x fmt x_opt ->
    match x_opt with
    | None -> Format.pp_print_string fmt "None"
    | Some x ->
      Format.pp_print_string fmt "(Some ";
      pp_x fmt x;
      Format.pp_print_string fmt ")"

  let show : (Format.formatter -> 'a -> unit) -> 'a t -> string =
   (fun pp_x x_opt -> Format.asprintf "%a" (pp pp_x) x_opt)

  let if_none x_opt ~f =
    match x_opt with
    | Some x -> Some x
    | None -> f ()

  let ( >>! ) = if_none
end
