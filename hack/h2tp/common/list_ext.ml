(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

include List
open Utils

let rec first pred = function
  | [] -> None
  | (x::_) when pred x -> Some x
  | (_::xs) -> first pred xs

let rec lst_unsafe = function
  | [] -> raise (Invalid_argument "lst_unsafe got empty list")
  | [e] -> e
  | _::es -> lst_unsafe es

let is_empty xs = List.length xs = 0

let not_empty xs = not (is_empty xs)

let intersperse sep = function
  | [] -> []
  | x::xs -> x :: List.fold_right (fun y ys -> sep :: y :: ys) xs []

let concatMap f xs = List.map f xs |> List.concat
