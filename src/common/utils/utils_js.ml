(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Key_not_found of (* message *) string * (* key *) string

let spf = Printf.sprintf

let print_endlinef fmt = Printf.ksprintf print_endline fmt

let prerr_endlinef fmt = Printf.ksprintf prerr_endline fmt

let exe_name = Filename.basename Sys.executable_name

module FilenameSet = struct
  include Flow_set.Make (File_key)

  let pp = make_pp File_key.pp

  let show x = Format.asprintf "%a" pp x
end

module FilenameMap = struct
  include WrappedMap.Make (File_key)

  let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
   (fun pp_data -> make_pp File_key.pp pp_data)

  let show pp_data x = Format.asprintf "%a" (pp pp_data) x
end

module FilenameGraph = Graph.Make (FilenameSet) (FilenameMap)

let debug_string_of_filename_set set =
  set
  |> FilenameSet.elements
  |> Base.List.map ~f:File_key.to_string
  |> String.concat ", "
  |> spf "[%s]"

let debug_string_of_filename_map value_to_string map =
  map
  |> FilenameMap.map value_to_string
  |> FilenameMap.elements
  |> Base.List.map ~f:(fun (file, value) -> spf "  %s: %s" (File_key.to_string file) value)
  |> String.concat "\n"

let assert_false s =
  let callstack = Exception.get_current_callstack_string 10 in
  prerr_endline
    (spf
       "%s%s\n%s:\n%s%s%s"
       (* this clowny shit is to evade hg's conflict marker detection *)
       "<<<<"
       "<<<<"
       s
       callstack
       ">>>>"
       ">>>>"
    );
  failwith s

let ite cond a b =
  if cond then
    a
  else
    b

let map_pair f g (a, b) = (f a, g b)

let map_fst f (a, b) = (f a, b)

let map_snd g (a, b) = (a, g b)

let swap (a, b) = (b, a)

let mk_tuple x y = (x, y)

let mk_tuple_swapped x y = (y, x)

let rec iter2opt f = function
  | (x :: xs, y :: ys) ->
    f (Some x) (Some y);
    iter2opt f (xs, ys)
  | (x :: xs, []) ->
    f (Some x) None;
    iter2opt f (xs, [])
  | ([], y :: ys) ->
    f None (Some y);
    iter2opt f ([], ys)
  | ([], []) -> ()

let uncurry f (x, y) = f x y

let curry f x y = f (x, y)

let ( %> ) f g x = g (f x)

(**
 * Given a list of lazy "option" expressions, evaluate each in the list
 * sequentially until one produces a `Some` (and do not evaluate any remaining).
 *)
let lazy_seq (lst : 'a option Lazy.t list) : 'a option =
  List.fold_left
    (fun acc lazy_expr ->
      match acc with
      | None -> Lazy.force lazy_expr
      | Some _ -> acc)
    None
    lst

(**
 * Useful for various places where a user might have typoed a string and the
 * set of possible intended strings is known (i.e. variable names).
 *)
let typo_suggestions =
  (*
   * Calculates the Levenshtein distance between the two strings, but with a
   * limit. See here for documentation on this algorithm:
   *
   * https://en.wikipedia.org/wiki/Levenshtein_distance
   *)
  let distance a b limit =
    let alen = String.length a in
    let blen = String.length b in
    let limit = min (max alen blen) limit in
    if abs (alen - blen) > limit then
      None
    else
      let matrix = Array.make_matrix (alen + 1) (blen + 1) (limit + 1) in
      matrix.(0).(0) <- 0;
      for i = 1 to max alen blen do
        if i <= alen then matrix.(i).(0) <- i;
        if i <= blen then matrix.(0).(i) <- i
      done;
      for ai = 1 to alen do
        for bi = max 1 (ai - limit - 1) to min blen (ai + limit + 1) do
          let prev_ai = a.[ai - 1] in
          let prev_bi = b.[bi - 1] in
          let cost =
            if prev_ai = prev_bi then
              0
            else
              1
          in
          let closest =
            min
              (min (matrix.(ai - 1).(bi) + 1) (* deletion *) (matrix.(ai).(bi - 1) + 1))
              (* insertion *)
              (matrix.(ai - 1).(bi - 1) + cost)
            (* substitution *)
          in
          let closest =
            if ai > 1 && bi > 1 && prev_ai = b.[bi - 2] && a.[ai - 2] = prev_bi then
              (* transposition *)
              min matrix.(ai).(bi) (matrix.(ai - 2).(bi - 2) + cost)
            else
              closest
          in
          matrix.(ai).(bi) <- closest
        done
      done;
      let result = matrix.(alen).(blen) in
      if result > limit then
        None
      else
        Some result
  in
  let fold_results limit name results poss_name =
    match distance name poss_name limit with
    | None -> results
    | Some distance ->
      let (curr_choice, curr_dist) = results in
      if distance < curr_dist then
        ([poss_name], curr_dist)
      else if distance = curr_dist then
        (poss_name :: curr_choice, curr_dist)
      else
        results
  in
  fun possible_names name ->
    let limit =
      match String.length name with
      | 1
      | 2 ->
        0
      | 3
      | 4 ->
        1
      | 5
      | 6 ->
        2
      | _ -> 3
    in
    fst (List.fold_left (fold_results limit name) ([], max_int) possible_names)

let typo_suggestion possible_names_rev name =
  let suggestions = typo_suggestions possible_names_rev name in
  try Some (List.hd suggestions) with
  | _ -> None

(* util to limit the number of calls to a (usually recursive) function *)
let count_calls ~counter ~default f =
  (* Count number of calls to a function f, decrementing at each call and
     returning default when count reaches 0. **)
  if !counter = 0 then
    default
  else (
    decr counter;
    f ()
  )

let extension_of_filename filename =
  try
    let idx = String.rindex filename '.' in
    Some (String.sub filename idx (String.length filename - idx))
  with
  | Not_found -> None

(* ordinal of a number *)
let ordinal = function
  | 1 -> "first"
  | 2 -> "second"
  | 3 -> "third"
  | 4 -> "fourth"
  | 5 -> "fifth"
  | 6 -> "sixth"
  | 7 -> "seventh"
  | 8 -> "eigth"
  | 9 -> "ninth"
  | n ->
    let n = string_of_int n in
    let th = n.[String.length n - 1] in
    let th =
      match th with
      | '1' -> "st"
      | '2' -> "nd"
      | '3' -> "rd"
      | _ -> "th"
    in
    n ^ th

(* Module implementing the recommended way to augment a map.

   Without this API, we end up using the lower-level Map.union API. But
   Map.union sometimes has unexpected results because the order of the two
   arguments matters when there's overlap. (It's implemented as a Map.fold of
   Map.add, which has the effect of adding the bindings in the first argument to
   the second argument.)

   Instead, Augmentable(Map).augment map ~with_bindings makes the intention
   explicit, and is implemented by simply passing the arguments in the correct
   order to Map.union.
*)
module Augmentable (M : WrappedMap.S) = struct
  let augment map ~with_bindings = M.union with_bindings map
end

module AugmentableSMap = Augmentable (SMap)

(* The problem with Base.Result's >>= is that the function second argument cannot return
 * an Lwt.t. This helper infix operator handles that case *)
let ( %>>= ) (result : ('ok, 'err) result) (f : 'ok -> ('a, 'err) result Lwt.t) :
    ('a, 'err) result Lwt.t =
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok x -> f x

let ( %>>| ) (result : ('ok, 'err) result) (f : 'ok -> 'a Lwt.t) : ('a, 'err) result Lwt.t =
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok x ->
    let%lwt new_x = f x in
    Lwt.return (Ok new_x)

let debug_print_current_stack_trace () =
  Hh_logger.info "Current backtrace:\n%s" (Exception.get_current_callstack_string 200)

let get_next_power_of_two x =
  let rec f y =
    if y >= x then
      y
    else
      f (y * 2)
  in
  f 1

module BoolMap = Flow_map.Make (struct
  type t = bool

  let compare = Base.Bool.compare
end)

module NumberMap = Flow_map.Make (struct
  type t = float

  let compare = Base.Float.compare
end)

module BigIntOptionMap = Flow_map.Make (struct
  type t = int64 option

  let compare x y =
    match (x, y) with
    | (Some x, Some y) -> Int64.compare x y
    | (Some _, None) -> 1
    | (None, Some _) -> -1
    | (None, None) -> 0
end)

let in_flow_test =
  match Sys.getenv_opt "IN_FLOW_TEST" with
  | Some "1"
  | Some "true" ->
    true
  | Some _
  | None ->
    false

let camelize = Parse_error_utils.camelize
