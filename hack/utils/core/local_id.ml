(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module S = struct
  type t = int * string

  let equal_ref : (t -> t -> bool) ref = ref Caml.( = )

  let equal x y = !equal_ref x y

  let compare = Pervasives.compare
end

include S

let ctr = ref 1

let next () =
  incr ctr;
  !ctr

let to_string x = snd x

let pp_ref : (Format.formatter -> int * string -> unit) ref =
  ref (fun fmt x -> Format.pp_print_string fmt (to_string x))

let pp fmt x = !pp_ref fmt x

let to_int x = fst x

let get_name x = to_string x

let make_scoped x = (next (), x)

let make_unscoped x = (0, x)

let make i x = (i, x)

let tmp () =
  let res = next () in
  (res, "__tmp" ^ string_of_int res)

module Set = Set.Make (S)
module Map = WrappedMap.Make (S)
