(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type NODE = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module Make (N : NODE) (NMap : MyMap.S with type key = N.t) (NSet : Set.S with type elt = N.t) : sig
  (* given a map from keys to dependencies, returns whether the dependencies are
     cyclic, as well as a topologically sorted list of key lists where any keys in
     a list only depend on keys in a subsequent list
  *)
  val topsort : roots:NSet.t -> NSet.t NMap.t -> N.t Nel.t list

  val log : N.t Nel.t list -> unit
end
