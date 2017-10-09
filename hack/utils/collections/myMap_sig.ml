(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module type S = sig
  include Map.S

  val add: ?combine: ('a -> 'a -> 'a) -> key -> 'a -> 'a t -> 'a t
  val get: key -> 'a t -> 'a option
  val find_unsafe: key -> 'a t -> 'a
  val union: ?combine:(key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare: 'a t -> 'a t -> int
  val equal: 'a t -> 'a t -> bool
  val keys: 'a t -> key list
  val ordered_keys: 'a t -> key list
  val values: 'a t -> 'a list
  val elements: 'a t -> (key * 'a) list
  val map_env: ('c -> 'a -> 'c * 'b) -> 'c -> 'a t -> 'c * 'b t
  val choose: 'a t -> (key * 'a) option
  val from_keys: key list -> f:(key -> 'a) -> 'a t
  val ident_map: ('a -> 'a) -> 'a t -> 'a t
  val ident_map_key: ?combine: ('a -> 'a -> 'a) -> (key -> key) -> 'a t -> 'a t
end
