(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = int

val compare : t -> t -> int

val track_names : bool ref

val tmp : unit -> t

val to_string : t -> string

val debug : ?normalize:(int -> int) -> t -> string

val get_name : t -> string

val set_name : t -> string -> unit

val make : string -> t

val pp : Format.formatter -> t -> unit
