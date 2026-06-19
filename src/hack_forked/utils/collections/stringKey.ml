(* (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary. *)

type t = string

let compare (x : t) (y : t) = String.compare x y

let to_string x = x

let pp fmt x = Format.fprintf fmt "%S" x
