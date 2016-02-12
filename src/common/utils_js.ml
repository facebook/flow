(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

(**
 * There seems to be a bug in OCaml 4.01 that causes `include Utils` in this
 * file to generate an interface for this file that is merged with the interface
 * for Utils -- but is somehow not type compatible with it.
 *
 * The only workaround I could find was to require that downstream users of both
 * utils.ml and utils_js.ml explicitly refer to both :(
 *
 * If at some point we are able to deprecate support for 4.01, we should
 * consider re-instating the following line and using Utils_js as a direct
 * extension of utils.ml.
 *)
(* include Utils *)

open Utils

(* alias stuff from `Loc` so that it can be used by doing `open Utils_js`
   instead of `open Loc`, which pollutes too much. *)
type filename = Loc.filename
let string_of_filename = Loc.string_of_filename

module FilenameSet = struct
  include Set.Make(Loc.FilenameKey)
  let of_list = List.fold_left (fun s f -> add f s) empty
end

module FilenameMap = MyMap.Make (Loc.FilenameKey)

(* ok-or-error type *)
type ('a,'b) ok_or_err = OK of 'a | Err of 'b

let assert_false s =
  let callstack = Printexc.(get_callstack 10 |> raw_backtrace_to_string) in
  prerr_endline (spf "%s%s\n%s:\n%s%s%s"
    (* this clowny shit is to evade hg's conflict marker detection *)
    "<<<<" "<<<<" s callstack ">>>>" ">>>>"
  );
  failwith s

let __DEBUG__ ?(s="") f =
  try f () with _ -> assert_false s

(* Time logging utility. Computes the elapsed time when running some code, and
   if the elapsed time satisfies a given predicate (typically, is more than a
   threshold), prints a message. *)
let time pred msg f =
  let start = Unix.gettimeofday () in
  let ret = f () in
  let elap = (Unix.gettimeofday ()) -. start in
  if not (pred elap) then () else prerr_endline (msg elap);
  ret

let call_succeeds try_function function_input =
  try
    try_function function_input;
    true
  with
  (* print failwith <msg> command's exception message *)
  | Failure msg -> prerr_endline msg;
                   false
  | _ -> false

(* quick exception format *)

let fmt_exc exc = Printexc.((to_string exc) ^ "\n" ^ (get_backtrace ()))

let fmt_file_exc file exc = file ^ ": " ^ (fmt_exc exc)

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

(** like List.fold_left, but f returns an option and so do we.
    f acc v = Some acc proceeds as usual; None stops the fold.
    Eg
      let f x y = if y > 0 then Some (x + y) else None in
      fold_left_opt f 0 [1; 2; 3; 4; 5] => Some 15
      fold_left_opt f 0 [1; 2; -3; 4; 5] => None

    Useful in situations where failure rules out List.fold_left.
  *)
let rec fold_left_opt f acc = function
| [] -> Some acc
| v :: vs ->
  match f acc v with
  | None -> None
  | Some acc -> fold_left_opt f acc vs

(** like List.fold_left, but f returns a stop flag as well as a result.
    f acc v = true, acc proceeds as usual; false, acc stops the fold.
    Eg
      let f x y = if y > 0 then true, (x + y) else false, x in
      fold_left_until f 0 [1; 2; 3; 4; 5] => 15
      fold_left_until f 0 [1; 2; -3; 4; 5] => 3

    Useful in situations where shortcutting makes List.fold_left a bad fit.
  *)
let rec fold_left_until f acc = function
| [] -> acc
| v :: vs ->
  match f acc v with
  | false, acc -> acc
  | true, acc -> fold_left_until f acc vs

(** unique list items, in order of first appearance *)
let rec uniq = function
  | [] -> []
  | [x] -> [x]
  | x :: (y :: _ as l) when x = y -> uniq l
  | x :: rl -> x :: uniq rl

(**
 * Useful for various places where a user might have typoed a string and the
 * set of possible intended strings is known (i.e. variable names).
 *)
let typo_suggestions =
  (**
   * Calculates the Levenshtein distance between the two strings, but with a
   * limit. See here for documentation on this algorithm:
   *
   * https://en.wikipedia.org/wiki/Levenshtein_distance
   *)
  let distance a b limit =
    let alen = String.length a in
    let blen = String.length b in
    let limit = min (max alen blen) limit in
    if abs (alen - blen) > limit then None else (
      let matrix = Array.make_matrix (alen + 1) (blen + 1) (limit + 1) in
      matrix.(0).(0) <- 0;
      for i = 1 to (max alen blen) do
        if i <= alen then matrix.(i).(0) <- i;
        if i <= blen then matrix.(0).(i) <- i;
      done;
      for ai = 1 to alen do
        for bi = max 1 (ai - limit - 1) to min blen (ai + limit + 1) do
          let prev_ai = a.[ai - 1] in
          let prev_bi = b.[bi - 1] in
          let cost = if prev_ai = prev_bi then 0 else 1 in
          let closest =
            min
              (min
                (matrix.(ai - 1).(bi) + 1) (* deletion *)
                (matrix.(ai).(bi - 1) + 1)) (* insertion *)
              (matrix.(ai - 1).(bi - 1) + cost) (* substitution *)
          in
          let closest =
            if ai > 1 && bi > 1 && prev_ai = b.[bi-2] && a.[ai-2] = prev_bi
            then
              (* transposition *)
              min (matrix.(ai).(bi)) (matrix.(ai - 2).(bi - 2) + cost)
            else closest
          in
          matrix.(ai).(bi) <- closest
        done;
      done;
      let result = matrix.(alen).(blen) in
      if result > limit then None else Some result
    )
  in

  let fold_results limit name results poss_name =
    match distance name poss_name limit with
    | None -> results
    | Some distance ->
        let (curr_choice, curr_dist) = results in
        if distance < curr_dist
        then ([poss_name], curr_dist)
        else
          if distance = curr_dist
          then (poss_name::curr_choice, curr_dist)
          else results
  in

  fun possible_names name ->
    let limit =
      match String.length name with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
    in
    fst (List.fold_left (fold_results limit name) ([], max_int) possible_names)

(* Wrapper for command results that carry an optional failure message. Ideally,
   this data format would be used to wrap results of all commands, but currently
   we use it only for autocomplete. *)
(** TODO: This definition is here instead of somewhere closer to server/ or
    commands/ because Flow_js.Autocomplete uses it. **)
type 'a command_result = string option * 'a
let command_result_success r = None, r
let command_result_failure s r = Some s, r
