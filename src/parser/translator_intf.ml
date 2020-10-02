(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type t

  val string : string -> t

  val bool : bool -> t

  val obj : (string * t) list -> t

  val array : t list -> t

  val number : float -> t

  val int : int -> t

  val null : t

  val regexp : Loc.t -> string -> string -> t
end
