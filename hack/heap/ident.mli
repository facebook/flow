(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
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
