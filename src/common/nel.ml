(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Non-empty list *)

type 'a t = 'a * 'a list

let to_list (x, xs) = x::xs

let one x = (x, [])

let cons y (x, xs) = (y, x::xs)

let map f (x, xs) =
  (f x, List.map f xs)

let rev (x, xs) =
  match List.rev (x::xs) with
  | x::xs -> (x, xs)
  | _ -> failwith "impossible"

let fold_left f init (x, xs) =
  List.fold_left f (f init x) xs
