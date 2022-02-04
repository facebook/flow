(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Hg
  | Git

val find_root : ?recursion_limit:int -> Path.t -> (t * Path.t) option
val find : ?recursion_limit:int -> Path.t -> t option
val name : t -> string
