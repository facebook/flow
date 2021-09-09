(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  include Flow_map.S

  val add : ?combine:('a -> 'a -> 'a) -> key -> 'a -> 'a t -> 'a t

  val union : ?combine:(key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val union_env :
    'a -> 'b t -> 'b t -> combine:('a -> key -> 'b -> 'b -> 'a * 'b option) -> 'a * 'b t

  val merge_env :
    'a ->
    'b t ->
    'c t ->
    combine:('a -> key -> 'b option -> 'c option -> 'a * 'd option) ->
    'a * 'd t

  val keys : 'a t -> key list

  val ordered_keys : 'a t -> key list

  val values : 'a t -> 'a list

  val fold_env : 'a -> ('a -> key -> 'b -> 'c -> 'a * 'c) -> 'b t -> 'c -> 'a * 'c

  val map_env : ('c -> key -> 'a -> 'c * 'b) -> 'c -> 'a t -> 'c * 'b t

  val of_list : (key * 'a) list -> 'a t

  val of_function : key list -> (key -> 'a) -> 'a t

  val elements : 'a t -> (key * 'a) list

  val ident_map : ('a -> 'a) -> 'a t -> 'a t

  val ident_map_key : ?combine:('a -> 'a -> 'a) -> (key -> key) -> 'a t -> 'a t

  val for_all2 : f:(key -> 'a option -> 'b option -> bool) -> 'a t -> 'b t -> bool

  val make_pp :
    (Format.formatter -> key -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit
end
