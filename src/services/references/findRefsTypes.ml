(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ref_kind =
  | PropertyDefinition
  | PropertyAccess
  | Local
  | Other

let string_of_ref_kind = function
  | PropertyDefinition -> "PropertyDefinition"
  | PropertyAccess -> "PropertyAccess"
  | Local -> "Local"
  | Other -> "Other"

type single_ref = ref_kind * Loc.t

type find_refs_found = string * single_ref list

type find_refs_ok = find_refs_found option
