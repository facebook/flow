(* (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary. *)

include Flow_set.Make (IntKey)

let pp = make_pp IntKey.pp

let show iset = Format.asprintf "%a" pp iset

let to_string = show
