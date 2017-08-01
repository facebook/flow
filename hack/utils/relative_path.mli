(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reordered_argument_collections

type prefix =
  | Root
  | Hhi
  | Dummy

val set_path_prefix : prefix -> Path.t -> unit
val path_of_prefix : prefix -> string

module S : sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
end

type t = S.t

val pp : Format.formatter -> t -> unit

val default : t
(* Checks that string indeed has the given prefix before constructing path *)
val create : prefix -> string -> t
(* Creates a Relative_path.t relative to the root *)
val from_root : string -> t
val prefix : t -> prefix
val suffix : t -> string
val to_absolute : t -> string

module Set : module type of Reordered_argument_set(Set.Make(S))
module Map : module type of Reordered_argument_map(MyMap.Make(S))

val relativize_set : prefix -> SSet.t -> Set.t
val set_of_list : t list -> Set.t
