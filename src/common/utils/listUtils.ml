(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module M_ = Monad

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

let rec first_some_map f = function
  | [] -> None
  | hd::tl -> begin match f hd with
    | Some _ as x -> x
    | None -> first_some_map f tl
  end

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

(** unique list items, in order of first appearance (requires sorted list) *)
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

(** performs a map, but returns the original list if there is no change **)
let ident_map f lst =
  let rev_lst, changed = List.fold_left (fun (lst_, changed) item ->
    let item_ = f item in
    item_::lst_, changed || item_ != item
  ) ([], false) lst in
  if changed then List.rev rev_lst else lst

let rec combine3 = function
  | ([], [], []) -> []
  | (a1::l1, a2::l2, a3::l3) -> (a1, a2, a3) :: combine3 (l1, l2, l3)
  | (_, _, _) -> invalid_arg "List.combine3"

let rec split3 = function
  | [] -> ([], [], [])
  | (x,y,z)::l ->
      let (rx, ry,rz) = split3 l in (x::rx, y::ry, z::rz)

let zipi xs ys =
  List.combine xs ys |> List.mapi (fun i (x, y) -> (i,x,y))

let range_with f a b =
  if a > b then []
  else
    let rec loop j acc =
      if a <= j then loop (j-1) (f j :: acc)
      else acc
    in
    loop (b-1) []

let range = range_with (fun x -> x)

let repeat n a = range_with (fun _ -> a) 0 n

let rec cat_maybes = function
  | [] -> []
  | (Some y) :: ys -> y :: cat_maybes ys
  | None :: ys -> cat_maybes ys

(** fold over the elements of a list while keeping the results of
    each iteration and returning it in the end along with the
    accumulator
  *)
let fold_map f acc xs =
  let acc', ys = List.fold_left (fun (a, ys) x ->
    let (a', y) = f a x in
    (a', y :: ys)
  ) (acc, []) xs in
  (acc', List.rev ys)

let concat_fold f acc items =
  let acc, lists = List.fold_left (fun (acc, lists) item ->
    let acc, list = f acc item in
    acc, list :: lists
  ) (acc, []) items in
  acc, List.concat lists

(** Monadic versions of previous folding operations

    It's unfortunate that we have to replicate the definitions for
    the cases where the monadic module accepts 1 or 2 type paramenters.
  *)

module Monad (M : M_.S) = struct

  include M_.Make(struct
    type 'a t = 'a M.t
    let bind   = M.bind
    let return = M.return
    let map = `Custom M.map
  end)

  let fold_map_m f init xs =
    List.fold_left (fun acc x -> acc >>= (fun (s, ys) ->
      f s x >>| fun (s', y) ->
      (s', y :: ys)
    )) (return (init, [])) xs >>| fun (acc', ys) ->
    (acc', List.rev ys)

  let concat_fold_m f init items =
    List.fold_left (fun a item -> a >>= fun (acc, lists) ->
      f acc item >>| fun (acc, list) ->
      (acc, list :: lists)
    ) (return (init, [])) items >>| fun (acc, lists) ->
    (acc, List.concat lists)
end

module Monad2 (M : M_.S2) = struct

  include M_.Make2(struct
    type ('a, 'b) t = ('a,'b) M.t
    let bind   = M.bind
    let return = M.return
    let map = `Custom M.map
  end)

  let fold_map_m f init xs =
    List.fold_left (fun acc x -> acc >>= (fun (s, ys) ->
      f s x >>| fun (s', y) ->
      (s', y :: ys)
    )) (return (init, [])) xs >>| fun (acc', ys) ->
    (acc', List.rev ys)

  let concat_fold_m f init items =
    List.fold_left (fun a item -> a >>= fun (acc, lists) ->
      f acc item >>| fun (acc, list) ->
      (acc, list :: lists)
    ) (return (init, [])) items >>| fun (acc, lists) ->
    (acc, List.concat lists)
end

(* Stringify a list given a separator and a printer for the element type *)
let to_string separator printer list =
  String.concat separator @@ List.map printer list
