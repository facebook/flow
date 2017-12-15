(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val create: unit -> t
val set: t -> Loc.t -> Type.t -> unit
val iter: (Loc.t -> Type.t -> unit) -> t -> unit
val fold: (Loc.t -> Type.t -> 'a -> 'a) -> t -> 'a -> 'a
val find_unsafe: t -> Loc.t -> Type.t
val reset: t -> unit
val copy: t -> t
