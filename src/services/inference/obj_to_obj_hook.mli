(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val with_obj_to_obj_hook :
  enabled:bool ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  f:(unit -> 'a) ->
  'a * Type.Properties.Set.t Loc_collections.LocMap.t
