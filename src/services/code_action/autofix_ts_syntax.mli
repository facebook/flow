(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val convert_never_type :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val convert_undefined_type :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val convert_type_param_colon :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val remove_in_out_variance :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val convert_plus_sigil_to_readonly :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val convert_plus_sigil_to_out :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val convert_minus_sigil_to_writeonly :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val convert_minus_sigil_to_in :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val convert_as_expression :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val convert_satisfies_expression :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val convert_readonly_array_type :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val convert_readonly_tuple_type :
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t
