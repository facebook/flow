(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val provide_available_refactors :
  (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (string * (Loc.t, Loc.t) Flow_ast.Program.t) list
