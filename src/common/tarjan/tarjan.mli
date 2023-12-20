(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type NODE = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module type Map = sig
  type key

  type value

  type t

  val find : key -> t -> value
end

module Make
    (N : NODE)
    (NSet : Flow_set.S with type elt = N.t)
    (NMap : Map with type key = N.t and type value = NSet.t) : sig
  (* given a map from keys to dependencies, returns whether the dependencies are
     cyclic, as well as a topologically sorted list of key lists where any keys in
     a list only depend on keys in a subsequent list
  *)
  val topsort : roots:NSet.t -> NMap.t -> N.t Nel.t list
end
