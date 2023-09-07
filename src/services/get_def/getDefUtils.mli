(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Get_def_types

val get_object_literal_loc : Type.t -> ALoc.t option

val all_locs_of_property_def_info : property_def_info -> Loc.t Nel.t

val all_locs_of_def_info : def_info -> Loc.t list

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
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  Types_js_types.typecheck_artifacts ->
  Loc.t ->
  (property_def_info option, string) result

val get_def_info :
  options:Options.t ->
  reader:Parsing_heaps.Reader.reader ->
  purpose:Purpose.t ->
  FindRefsUtils.ast_info ->
  Types_js_types.typecheck_artifacts ->
  Loc.t ->
  (def_info, string) result
