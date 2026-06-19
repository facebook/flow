(* (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary. *)

include WrappedMap.Make (StringKey)

let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
 (fun pp_data -> make_pp StringKey.pp pp_data)

let show pp_data x = Format.asprintf "%a" (pp pp_data) x
