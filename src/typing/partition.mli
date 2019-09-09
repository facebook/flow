(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a t

val empty : ('a -> Type.t) -> 'a t

val cell : 'a -> 'a t -> 'a list

val mem : 'a -> 'a t -> 'a

val add : 'a -> 'a t -> 'a t

val is_discrete : 'a t -> bool

val from : ('a -> Type.t) -> 'a list -> 'a t
