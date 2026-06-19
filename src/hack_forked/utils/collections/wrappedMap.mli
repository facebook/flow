(* (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary. *)

module type S = WrappedMap_sig.S

module Make (Ord : Map.OrderedType) : S with type key = Ord.t
