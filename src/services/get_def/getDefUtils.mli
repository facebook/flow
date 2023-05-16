(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type single_property_def_info =
  | ClassProperty of Loc.t
  | ObjectProperty of Loc.t

type property_def_info = single_property_def_info Nel.t

type def_info = property_def_info * string

val get_object_literal_loc : Type.t -> ALoc.t option

val all_locs_of_property_def_info : property_def_info -> Loc.t Nel.t

val all_locs_of_def_info : def_info -> Loc.t Nel.t

type def_loc =
  | FoundClass of Loc.t Nel.t
  | FoundObject of Loc.t
  | FoundUnion of def_loc Nel.t
  | NoDefFound
  | UnsupportedType
  | AnyType

val extract_def_loc :
  loc_of_aloc:(ALoc.t -> Loc.t) -> Context.t -> Type.t -> string -> (def_loc, string) result

val get_property_def_info :
  loc_of_aloc:(ALoc.t -> Loc_collections.LocMap.key) ->
  Types_js_types.typecheck_artifacts ->
  Loc.t ->
  (def_info option, string) result
