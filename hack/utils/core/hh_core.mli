(*
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* This `.mli` file was generated automatically. It may include extra
definitions that should not actually be exposed to the caller. If you notice
that this interface file is a poor interface, please take a few minutes to
clean it up manually, and then delete this comment once the interface is in
shape. *)

module List : sig
  include module type of Core_list

  val fold_left_env :
    'a -> 'b list -> init:'c -> f:('a -> 'c -> 'b -> 'a * 'c) -> 'a * 'c

  val rev_map_env : 'a -> 'b list -> f:('a -> 'b -> 'a * 'c) -> 'a * 'c list

  val map_env : 'a -> 'b list -> f:('a -> 'b -> 'a * 'c) -> 'a * 'c list

  val map2_env :
    'a -> 'b list -> 'c list -> f:('a -> 'b -> 'c -> 'a * 'd) -> 'a * 'd list

  val map3_env :
    'a ->
    'b list ->
    'c list ->
    'd list ->
    f:('a -> 'b -> 'c -> 'd -> 'a * 'e) ->
    'a * 'e list

  val filter_map_env :
    'a -> 'b list -> f:('a -> 'b -> 'a * 'c option) -> 'a * 'c list

  val for_all2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool

  val same_length_and_for_all2 :
    f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
end
