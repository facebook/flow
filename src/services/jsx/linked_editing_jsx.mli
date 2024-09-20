(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val get_linked_locs : (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t -> Loc.t -> Loc.t list option
