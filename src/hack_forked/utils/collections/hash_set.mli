(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This is a small module to implement Hash_set for maximal sharing.

    Why we don't re-use the (elt,unit)Hashtbl.t.

    The stdlib Hashtbl.t is overly complicated and 
    - it allows duplicate keys
    - not memory efficient
    - does not provide a way to match our [add] semantics in one pass
    
 *)

module type S = sig
  type t

  type elt

  val create : int -> t

  (** [clear h] clear the collection but not reclaiming the allocated space *)
  val clear : t -> unit

  (** [reset h] clear the collection and also reclaim the allocated space *)
  val reset : t -> unit

  (** [add h elt] 
    if [elt in h] then return the element already in the collection 
    else add [elt] to the collection and return [elt]
  *)
  val add : t -> elt -> elt
end
module Make (H : Hashtbl.HashedType) : S with type elt = H.t