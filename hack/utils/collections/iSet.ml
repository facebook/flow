(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

include Set.Make (IntKey)
let to_string iset =
  "{" ^ (String.concat "," (List.map string_of_int (elements iset))) ^ "}"

(* temporary implementations to placate deriving show *)
let show = to_string
let pp : Format.formatter -> t -> unit = fun _ x -> Printf.printf "%s\n" (show x)
