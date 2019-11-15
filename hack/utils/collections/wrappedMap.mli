(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module type S = WrappedMap_sig.S

module Make (Ord : Map.OrderedType) : S with type key = Ord.t
