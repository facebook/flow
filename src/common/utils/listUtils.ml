(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

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

    Like a reapeat-until loop, guaranteed to run f at least once.
    Useful in situations where shortcutting makes List.fold_left a bad fit.
  *)
let rec fold_left_until f acc = function
| [] -> acc
| v :: vs ->
  match f acc v with
  | false, acc -> acc
  | true, acc -> fold_left_until f acc vs

(** like List.fold_left, but adds a guard function p.
    p acc v is called before f acc v at each step. false stops the fold.
    Eg
      let p _ y = y > 0 in
      let f xs y = y :: xs in
      fold_left_while p f 0 [1; 2; 3; 4; 5] => [5; 4; 3; 2; 1]
      fold_left_while f 0 [-1; 2; 3; 4; 5] => []

    In effect p functions like a while loop test.
    Useful in situations where failure rules out List.fold_left,
    and test should precede action.
  *)
let rec fold_left_while p f acc = function
| [] -> acc
| v :: vs ->
  match p acc v with
  | false -> acc
  | true -> fold_left_while p f (f acc v) vs

(** Folds f over lst the given number of times, or for every element
    of lst, whichever is less.
    Eg
      let f acc x = x :: acc
      fold_left_for 3 f 0 [1; 2; 3; 4; 5] => [1; 2; 3]
  *)
let fold_left_for n f acc lst =
  snd (fold_left_while
    (fun (i, _) _ -> i < n)
    (fun (i, acc) x -> i + 1, f acc x)
    (0, acc)
    lst)

(* truncate a list to first 0 < n <= len items *)
let first_n n lst =
  List.rev (fold_left_for n (fun rl x -> x :: rl) [] lst)

(* truncate a list to last 0 < n <= len items *)
let last_n n lst =
  fold_left_for n (fun rl x -> x :: rl) [] (List.rev lst)

(* make a list of n copies of a given value *)
let copy_n n v =
  let rec loop acc = function
    | 0 -> acc
    | i -> loop (v :: acc) (i - 1)
  in loop [] n

(** unique list items, in order of first appearance *)
let rec uniq = function
| [] -> []
| [x] -> [x]
| x :: (y :: _ as l) when x = y -> uniq l
| x :: rl -> x :: uniq rl

(** physically unique list items, in order of first appearance *)
let rec phys_uniq = function
| [] -> []
| [x] -> [x]
| x :: (y :: _ as l) when x == y -> phys_uniq l
| x :: rl -> x :: phys_uniq rl
