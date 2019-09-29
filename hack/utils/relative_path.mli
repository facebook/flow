(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Reordered_argument_collections

type prefix =
  | Root
  | Hhi
  | Dummy
  | Tmp
[@@deriving show, enum]

val set_path_prefix : prefix -> Path.t -> unit

val path_of_prefix : prefix -> string

module S : sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

type t = S.t [@@deriving show]

val default : t

(* Checks that string indeed has the given prefix before constructing path *)
val create : prefix -> string -> t

(* Creates a new path, inferring the prefix. Will default to Dummy. *)
val create_detect_prefix : string -> t

(* Creates a Relative_path.t relative to the root *)
val from_root : string -> t

val prefix : t -> prefix

val suffix : t -> string

val to_absolute : t -> string

val to_tmp : t -> t

val to_root : t -> t

val strip_root_if_possible : string -> string option

module Set : sig
  include module type of Reordered_argument_set (Set.Make (S))

  val pp : Format.formatter -> t -> unit

  val show : t -> string
end

module Map : sig
  include module type of Reordered_argument_map (MyMap.Make (S))

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
end

val relativize_set : prefix -> SSet.t -> Set.t

val set_of_list : t list -> Set.t

val storage_to_string : t -> string

val storage_of_string : string -> t
