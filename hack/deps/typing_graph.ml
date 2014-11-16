(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
(*****************************************************************************)
(* Module for a compact graph representation. *)
(*****************************************************************************)
open Utils

external hh_add_dep: int -> unit     = "hh_add_dep"
external hh_get_dep: int -> int list = "hh_get_dep"

(*****************************************************************************)
(* Please consult hh_shared.c for the underlying representation. *)
(*****************************************************************************)

let add x y = hh_add_dep ((x lsl 31) lor y)

let get x =
  let l = hh_get_dep x in
  List.fold_left begin fun acc node ->
    ISet.add node acc
  end ISet.empty l
