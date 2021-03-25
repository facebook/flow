(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type type_contents_artifacts =
  Context.t
  * Docblock.t
  * File_sig.With_Loc.t
  * File_sig.With_Loc.tolerable_error list
  * (Loc.t, Loc.t) Flow_ast.Program.t
  * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t
  * (Loc.t * Parse_error.t) list
