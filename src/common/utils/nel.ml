(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Non-empty list *)

type 'a t = 'a * 'a list

let to_list (x, xs) = x::xs

let one x = (x, [])

let cons x0 (x1, xs) = (x0, x1::xs)

let iter f (x, xs) =
  f x;
  List.iter f xs

let map f (x, xs) = (f x, List.map f xs)

let ident_map f ((x, xs) as original) =
  let x' = f x in
  let xs' = ListUtils.ident_map f xs in
  if x' == x && xs' == xs then original
  else (x', xs')

let concat (xs, xss) =
  let xs = to_list xs in
  let xss = List.map to_list xss in
  match List.concat (xs::xss) with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)

let map_concat f (x, xs) =
  let xss = List.map (fun x -> to_list (f x)) (x::xs) in
  match List.concat xss with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)

let rev (x, xs) =
  match List.rev (x::xs) with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)

let rev_map f (x, xs) =
  match List.rev_map f (x::xs) with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)

let rev_append xs ys =
  match List.rev_append (to_list xs) (to_list ys) with
  | [] -> failwith "impossible"
  | z::zs -> (z, zs)

let length (_, xs) = 1 + List.length xs

let fold_left f acc (x, xs) = List.fold_left f acc (x::xs)

let hd (x, _) = x
