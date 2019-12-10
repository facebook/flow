(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Negative
  | Neutral
  | Positive
[@@deriving show]

val compat : t * t -> bool

val inv : t -> t

val mult : t * t -> t

val string : t -> string

val sigil : t -> string
