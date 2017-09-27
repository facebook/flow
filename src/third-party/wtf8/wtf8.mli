(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type codepoint =
  | Point of int
  | Malformed

type 'a folder = 'a -> int -> codepoint -> 'a

val fold_wtf_8 : ?pos:int -> ?len:int -> 'a folder -> 'a -> string -> 'a
val add_wtf_8 : Buffer.t -> int -> unit
