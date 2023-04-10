(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ast_info = (Loc.t, Loc.t) Flow_ast.Program.t * File_sig.t * Docblock.t

type type_info =
  Context.t
  * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t
  * Type.Properties.Set.t Loc_collections.LocMap.t
