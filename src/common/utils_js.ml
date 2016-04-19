(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

exception Key_not_found of (* message *) string * (* key *) string

let spf = Printf.sprintf
let print_endlinef fmt = Printf.ksprintf print_endline fmt
let prerr_endlinef fmt = Printf.ksprintf prerr_endline fmt

(* JSON numbers must not end in a `.`, but string_of_float returns things like
   `1.` instead of `1.0`, so we want to truncate the `.` *)
(* TODO: ocaml's string_of_float in general differs from JavaScript's. once
   we fix that (e.g. by pulling in double-conversion or dtoa), we can use that
   when printing JSON. *)
let string_of_float_trunc x =
  let result = string_of_float x in
  if String.get result (String.length result - 1) = '.' then
    String.sub result 0 (String.length result - 1)
  else
    result

let string_before s n = String.sub s 0 n
let string_after s n = String.sub s n (String.length s - n)

(* Returns the index of the first occurrence of string `needle` in string
   `haystack`. If not found, returns -1.

   An implementation of the Knuth-Morris-Pratt (KMP) algorithm. *)
let substring_index needle =
  (* see Wikipedia pseudocode *)
  let needle_len = String.length needle in
  if needle_len = 0 then raise (Invalid_argument needle);
  let table = Array.make needle_len 0 in
  table.(0) <- (-1);
  let pos = ref 2 and cnd = ref 0 in
  while !pos < needle_len do
    if needle.[!pos - 1] = needle.[!cnd] then
      (table.(!pos) <- !cnd + 1; incr pos; incr cnd)
    else if !cnd > 0 then
      cnd := table.(!cnd)
    else
      (table.(!pos) <- 0; incr pos)
  done;
  fun haystack ->
    let len = String.length haystack in
    let p = ref 0 in
    let q = ref 0 in
    while !p < len && !q < needle_len do
      if haystack.[!p] = needle.[!q] then (incr p; incr q)
      else if !q = 0 then incr p
      else q := table.(!q)
    done;
    if !q >= needle_len then !p - needle_len
    else -1

let is_substring needle =
  let substring_index_memo = substring_index needle in
  fun haystack -> (substring_index_memo haystack) >= 0

(* alias stuff from `Loc` so that it can be used by doing `open Utils_js`
   instead of `open Loc`, which pollutes too much. *)
type filename = Loc.filename
let string_of_filename = Loc.string_of_filename

module FilenameSet = struct
  include Set.Make(Loc.FilenameKey)
  let of_list = List.fold_left (fun s f -> add f s) empty
end

module FilenameMap = MyMap.Make (Loc.FilenameKey)

module PathMap : MyMap.S with type key = Path.t = MyMap.Make (struct
  type t = Path.t
  let compare p1 p2 =
    String.compare (Path.to_string p1) (Path.to_string p2)
end)

let set_of_list = List.fold_left (fun acc x -> SSet.add x acc) SSet.empty

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

let opt_map_default f def = function
  | None -> def
  | Some x -> f x

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
