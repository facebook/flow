(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
    (match f acc v with
    | None -> None
    | Some acc -> fold_left_opt f acc vs)

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
    (match f acc v with
    | (false, acc) -> acc
    | (true, acc) -> fold_left_until f acc vs)

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
    (match p acc v with
    | false -> acc
    | true -> fold_left_while p f (f acc v) vs)

(** Folds f over lst the given number of times, or for every element
    of lst, whichever is less.
    Eg
      let f acc x = x :: acc
      fold_left_for 3 f 0 [1; 2; 3; 4; 5] => [1; 2; 3]
 *)
let fold_left_for n f acc lst =
  snd (fold_left_while (fun (i, _) _ -> i < n) (fun (i, acc) x -> (i + 1, f acc x)) (0, acc) lst)

(** this function takes a list and truncates it if needed to no more than
    the first n elements. If truncation happened, then the callback 'f'
    is used to generated a final element e.g. "shown 5/200" *)
let first_upto_n n f lst =
  let (first, total) =
    Base.List.fold lst ~init:([], 0) ~f:(fun (first, total) s ->
        let first =
          if total < n then
            s :: first
          else
            first
        in
        (first, total + 1))
  in
  let r =
    if total <= n then
      first
    else
      match f total with
      | None -> first
      | Some e -> e :: first
  in
  Base.List.rev r

(* truncate a list to last 0 < n <= len items *)
let last_n n lst = Base.List.rev lst |> fold_left_for n (Base.Fn.flip Base.List.cons) []

(* make a list of n copies of a given value *)
let copy_n n v =
  let rec loop acc = function
    | 0 -> acc
    | i -> loop (v :: acc) (i - 1)
  in
  loop [] n

(** performs a map, but returns the original list if there is no change **)
let ident_map f lst =
  let (rev_lst, changed) =
    Base.List.fold_left
      ~f:(fun (lst_, changed) item ->
        let item_ = f item in
        (item_ :: lst_, changed || item_ != item))
      ~init:([], false)
      lst
  in
  if changed then
    Base.List.rev rev_lst
  else
    lst

let ident_mapi f lst =
  let (_, rev_lst, changed) =
    Base.List.fold_left
      ~f:(fun (index, lst_, changed) item ->
        let item_ = f index item in
        (index + 1, item_ :: lst_, changed || item_ != item))
      ~init:(0, [], false)
      lst
  in
  if changed then
    Base.List.rev rev_lst
  else
    lst

let ident_map_multiple f lst =
  let (rev_lst, changed) =
    Base.List.fold_left
      ~f:(fun (lst_, changed) item ->
        match f item with
        | [] -> (lst_, true)
        | [item_] -> (item_ :: lst_, changed || item != item_)
        | items_ -> (Base.List.rev_append items_ lst_, true))
      ~init:([], false)
      lst
  in
  if changed then
    Base.List.rev rev_lst
  else
    lst

(** performs a filter, but returns the original list if there is no change **)
let ident_filter f lst =
  let (rev_lst, changed) =
    Base.List.fold_left
      ~f:(fun (lst', changed) item ->
        if f item then
          (item :: lst', changed)
        else
          (lst', true))
      ~init:([], false)
      lst
  in
  if changed then
    Base.List.rev rev_lst
  else
    lst

let rec combine3 = function
  | ([], [], []) -> []
  | (a1 :: l1, a2 :: l2, a3 :: l3) -> (a1, a2, a3) :: combine3 (l1, l2, l3)
  | (_, _, _) -> invalid_arg "List.combine3"

let rec split3 = function
  | [] -> ([], [], [])
  | (x, y, z) :: l ->
    let (rx, ry, rz) = split3 l in
    (x :: rx, y :: ry, z :: rz)

let zipi xs ys = Base.List.zip_exn xs ys |> Base.List.mapi ~f:(fun i (x, y) -> (i, x, y))

let range_with f a b =
  if a > b then
    []
  else
    let rec loop j acc =
      if a <= j then
        loop (j - 1) (f j :: acc)
      else
        acc
    in
    loop (b - 1) []

let range = range_with (fun x -> x)

let repeat n a = range_with (fun _ -> a) 0 n

let concat_fold f acc items =
  let (acc, lists) =
    Base.List.fold_left
      ~f:(fun (acc, lists) item ->
        let (acc, list) = f acc item in
        (acc, list :: lists))
      ~init:(acc, [])
      items
  in
  (acc, Base.List.concat lists)

let last_opt l =
  let rec last l v =
    match l with
    | [] -> v
    | x :: xs -> last xs x
  in
  Base.List.nth l 0 |> Base.Option.map ~f:(last l)

(* Stringify a list given a separator and a printer for the element type *)
let to_string separator printer list = String.concat separator @@ Base.List.map ~f:printer list

(* Stringify an association list given a separator, a printer for the key type, a key/value
   separator, and a printer for the value type *)
let assoc_to_string separator key_printer key_value_separator value_printer list =
  to_string
    separator
    (fun (k, v) -> Printf.sprintf "%s%s%s" (key_printer k) key_value_separator (value_printer v))
    list

(* Dedups a list in O(n) time and space. Unlike Base.List.dedup, this
   preserves order. Core's implementation is also O(n log n) *)
let dedup l =
  let tbl = Base.List.length l |> Hashtbl.create in
  let f l e =
    if Hashtbl.mem tbl e then
      l
    else (
      Hashtbl.add tbl e ();
      e :: l
    )
  in
  Base.List.fold_left ~f ~init:[] l |> Base.List.rev
