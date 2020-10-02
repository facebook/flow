(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Converts a (Loc.t, Loc.t) AST into a (ALoc.t, ALoc.t) AST. Leaves the underlying representation
 * of the contained ALoc.ts concrete. *)
val loc_to_aloc_mapper : (Loc.t, Loc.t, ALoc.t, ALoc.t) Flow_polymorphic_ast_mapper.mapper

(* Converts an (ALoc.t, ALoc.t) AST where the ALoc.ts have a concrete underlying representation to
 * one where the ALoc.ts have a keyed underlying representation. *)
val keyify_alocs :
  File_key.t ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  ALoc.table * (ALoc.t, ALoc.t) Flow_ast.Program.t
