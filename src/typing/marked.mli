(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
module type S = sig
  type t
  type key
  val empty: t
  val add: key -> Type.polarity -> t -> (Type.polarity * t) option
  val get: key -> t -> Type.polarity option
  val mem: key -> Type.polarity -> t -> bool
  val exclude: key -> t -> t
end

module Make(Key: Map.OrderedType): S with type key = Key.t

module IdMarked: S with type key = int
