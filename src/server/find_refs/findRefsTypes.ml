(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type find_refs_found = (string * Loc.t list)
type find_refs_ok = find_refs_found option
type find_refs_result = (find_refs_ok, string) result
