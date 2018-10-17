(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module S = struct
  type t = int * string
  let compare x y = compare x y
end

include S

let ctr = ref 1

let next () =
  incr ctr;
  !ctr

let to_string x = snd x

let pp fmt x = Format.pp_print_string fmt (to_string x)

let to_int x = fst x

let get_name x = to_string x

let make x = (next (), x)

let without_ident x = (0, x)

(* `make` always returns a positive value. By multiplying the hash by -1 we
 * ensure that the value returned by `get` never overlaps with those returned
 * by `make` *)
let get x = (-(Hashtbl.hash x), x)

let tmp () =
  let res = next () in
  (res, ("__tmp"^string_of_int res))

module Set = Set.Make(S)
module Map = MyMap.Make(S)
