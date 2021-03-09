(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Map : WrappedMap.S with type key = Reason.name

module Set : Set.S with type elt = Reason.name

val smap_mem : Reason.name -> 'a SMap.t -> bool

val smap_find_opt : Reason.name -> 'a SMap.t -> 'a option

val display_smap_of_namemap : 'a Map.t -> 'a SMap.t

val namemap_of_smap : 'a SMap.t -> 'a Map.t
