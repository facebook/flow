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

  val get: key -> 'a t -> 'a option
  val find_unsafe: key -> 'a t -> 'a
  val union: 'a t -> 'a t -> 'a t
  val compare: 'a t -> 'a t -> int
  val equal: 'a t -> 'a t -> bool
  val keys: 'a t -> key list
  val values: 'a t -> 'a list
  val elements: 'a t -> (key * 'a) list
  val map_env: ('c -> 'a -> 'c * 'b) -> 'c -> 'a t -> 'c * 'b t
  val choose: 'a t -> (key * 'a) option
  val from_keys: key list -> (key -> 'a) -> 'a t
end
