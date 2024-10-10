(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type key

  type value

  module KeySet : Flow_set.S with type elt = key

  val add : key -> value -> unit

  val get_committed : key -> value option

  val get_latest : key -> value option

  val remove : key -> unit

  val remove_batch : KeySet.t -> unit

  val mem : key -> bool
end

(* A heap that allows versioned reads of `value` types. Underneath the hood,
 * it follows the `entity` semantics described in the NewAPI module above. OCaml values
 * are serialized and compressed.
 *)
module Make (Key : SharedMem.Key) (Value : SharedMem.Value) :
  S with type key = Key.t and type value = Value.t and module KeySet = Flow_set.Make(Key)
