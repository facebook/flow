(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = Loc.t

let of_loc loc = loc
let to_loc loc = loc

(* We'll still store the source concretely even when we move to abstract locations. *)
let source = Loc.source

let compare = Loc.compare
