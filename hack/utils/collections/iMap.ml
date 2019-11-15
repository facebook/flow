(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

include WrappedMap.Make (IntKey)

let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
 (fun pp_data -> make_pp Format.pp_print_int pp_data)

let show pp_data x = Format.asprintf "%a" (pp pp_data) x
