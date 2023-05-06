(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type single_def_info =
  | Class of Loc.t
  | Object of Loc.t

type property_def_info = single_def_info Nel.t

type def_info = property_def_info * string

module ObjectKeyAtLoc : sig
  val get : (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (Loc.t * Loc.t * string) option
end

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

val add_literal_properties :
  (string * Loc.t * string) option -> def_info option -> (def_info option, string) result

val get_def_info :
  loc_of_aloc:(ALoc.t -> Loc_collections.LocMap.key) ->
  Context.t
  * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t
  * Type.Properties.Set.t Loc_collections.LocMap.t ->
  Loc.t ->
  (def_info option, string) result
