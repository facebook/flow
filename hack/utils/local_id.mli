(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(** Used to represent local variables in the named AST. *)

module S : sig
  type t
  val compare : t -> t -> int
end

type t = S.t

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int

val to_string : t -> string

val to_int : t -> int

val get_name : t -> string

(** Make an id for a scoped variable. Return a fresh id every time.
This is used to enforce that two locals with the same name but with
different scopes have different ids. *)
val make_scoped : string -> t

(** Make an id for an unscoped variable. Two calls with the same input
 * string will return the same id. *)
val make_unscoped : string -> t

(** Returns the same id every time for a given string argument. Used for
 * function / method parameters.
 * The ids returned here are guaranteed not to overlap with those returned by
 * Local_id.make_scoped, which is used for naming local variables. *)
val get : string -> t

val tmp : unit -> t

module Set : module type of Set.Make(S)
module Map : module type of MyMap.Make(S)
