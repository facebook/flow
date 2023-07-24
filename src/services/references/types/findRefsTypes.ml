(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ref_kind =
  | PropertyDefinition
  | PropertyAccess
  | Local

let string_of_ref_kind = function
  | PropertyDefinition -> "PropertyDefinition"
  | PropertyAccess -> "PropertyAccess"
  | Local -> "Local"

type single_ref = ref_kind * Loc.t

type find_refs_found = single_ref list

type find_refs_ok = find_refs_found option

type kind =
  | FindReferences
  | Rename

type request = {
  def_info: Get_def_types.def_info;
  kind: kind;
}

let empty_request = { def_info = Get_def_types.NoDefinition; kind = FindReferences }
