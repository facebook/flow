(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type position = { line : int; column : int; offset : int; }
type t = { source : File_key.t option; start : position; _end : position; }
val none : t
val btwn : t -> t -> t
val btwn_exclusive : t -> t -> t
val char_before : t -> t
val first_char: t -> t
val contains : t -> t -> bool
val pos_cmp : position -> position -> int
val span_compare : t -> t -> int
val compare : t -> t -> int
val to_string : ?include_source:bool -> t -> string
val source : t -> File_key.t option
module LocSet : Set.S with type elt = t
