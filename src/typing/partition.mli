(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type 'a t
val empty : ('a -> Type.t) -> 'a t
val cell : 'a -> 'a t -> 'a list
val mem : 'a -> 'a t -> 'a
val add : 'a -> 'a t -> 'a t
val is_discrete : 'a t -> bool
val from : ('a -> Type.t) -> 'a list -> 'a t
