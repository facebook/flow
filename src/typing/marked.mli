(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
module type S = sig
  type t

  type key

  val empty : t

  val add : key -> Polarity.t -> t -> (Polarity.t * t) option

  val get : key -> t -> Polarity.t option

  val mem : key -> Polarity.t -> t -> bool

  val exclude : key -> t -> t
end

module Make (Key : Map.OrderedType) : S with type key = Key.t

module IdMarked : S with type key = int
