(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module S : sig
  type t
  val compare : t -> t -> int
end

type t = S.t

val track_names : bool ref

val compare : t -> t -> int

val to_string : t -> string

val to_int : t -> int

val get_name : t -> string

val make : string -> t

val make_fake : string -> t

val tmp : unit -> t

module Set : module type of Set.Make(S)
module Map : module type of MyMap.Make(S)
