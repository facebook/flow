(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Map : WrappedMap.S with type key = Reason.name

module Set : Flow_set.S with type elt = Reason.name

val display_smap_of_namemap : 'a Map.t -> 'a SMap.t

val namemap_of_smap : 'a SMap.t -> 'a Map.t
