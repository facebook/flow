(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t

val create: unit -> t
val set: t -> Loc.t -> Type.t -> unit
val iter: (Loc.t -> Type.t -> unit) -> t -> unit
val fold: (Loc.t -> Type.t -> 'a -> 'a) -> t -> 'a -> 'a
val find_unsafe: t -> Loc.t -> Type.t
val reset: t -> unit
