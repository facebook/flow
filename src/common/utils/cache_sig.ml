(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* WARNING: Eviction is O(n) in the number of entries contained in the cache. If a high enough
 * `max_size` is needed so that linear eviction times matter, consider using Core's cache module.
 *)
module type S = sig
  type 'a t

  type key

  val make : max_size:int -> 'a t

  val clear : 'a t -> unit

  val remove_entry : key -> 'a t -> unit

  (* Returns the value as well as a boolean which is true if the cache was hit and false if it
   * missed. *)
  val with_cache : key -> 'a Lwt.t Lazy.t -> 'a t -> ('a * bool) Lwt.t

  val with_cache_sync : key -> 'a Lazy.t -> 'a t -> 'a * bool
end
