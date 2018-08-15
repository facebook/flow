(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t
val empty: t
val add: int -> Type.polarity -> t -> (Type.polarity * t) option
val get: int -> t -> Type.polarity option
val mem: int -> Type.polarity -> t -> bool
val exclude: int -> t -> t
