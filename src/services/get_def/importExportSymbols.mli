(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Given a list of locations in this file, return the list of symbols which are related due to
 * imports or exports. This is used to determine the local bindings for exported/imported values and
 * vice-versa. In some cases, a returned location may be identical to a given location.
 * Some examples:
 *
 * Given the code: `export default function foo() {}`
 * - If given the location for `default`, will return the location for `foo`.
 * - If given the location for `foo`, will return the location for `default`.
 *
 * Given the code: `import {foo} from 'bar'`
 * - If given the location for `foo`, will return that same location.
 *)
val find_related_symbols : File_sig.With_Loc.t -> Loc.t list -> Loc.t list

(* As above but operates only on a single location *)
val find_related_symbol : File_sig.With_Loc.t -> Loc.t -> Loc.t option
