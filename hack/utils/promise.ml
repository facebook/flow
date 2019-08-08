(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module type S = sig
  type 'a t

  val return : 'a -> 'a t
  (** Creates a promise that returns the given value immediately. *)

  val map : 'a t -> ('a -> 'b) -> 'b t
  (** Returns a new promise that will map the result of the given one. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** Returns a new promise generated from the results of the given one. *)
end
