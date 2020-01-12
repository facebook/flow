(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Returns true iff the given AST contains an access to a property with the given name *)
val search : string (* name *) -> (Loc.t, Loc.t) Flow_ast.program -> bool
