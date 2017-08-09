(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type codepoint =
  | Point of int
  | Malformed

type 'a folder = 'a -> int -> codepoint -> 'a

val fold_wtf_8 : ?pos:int -> ?len:int -> 'a folder -> 'a -> string -> 'a
val add_wtf_8 : Buffer.t -> int -> unit
