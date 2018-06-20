(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ref_kind =
  | Other

type single_ref = ref_kind * Loc.t
type find_refs_found = (string * single_ref list)
type find_refs_ok = find_refs_found option
type find_refs_result = (find_refs_ok, string) result
