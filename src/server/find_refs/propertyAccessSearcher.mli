(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Returns true iff the given AST contains an access to a property with the given name *)
val search : string (* name *) -> Loc.t Ast.program -> bool
