(* (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary. *)

include Flow_set.Make (StringKey)

let pp = make_pp StringKey.pp

let show sset = Format.asprintf "%a" pp sset

let to_string = show
