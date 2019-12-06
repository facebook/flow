(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = WrappedMap_sig.S

module Make (Ord : Map.OrderedType) : S with type key = Ord.t
