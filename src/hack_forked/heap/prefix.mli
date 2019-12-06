(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* The prefix is used to guarantee that we are not mixing different kind of 
 * keys in the heap.
 * It just creates a new prefix every time its called.
 *)
(*****************************************************************************)

type t (* Better make the type abstract *)

val make : unit -> t

(* Given a prefix and a key make me a prefixed key *)
val make_key : t -> string -> string

(* Removes the prefix from a key *)
val remove : t -> string -> string
