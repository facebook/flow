(* (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary. *)

type t = int

let compare = ( - )

let pp = Format.pp_print_int
