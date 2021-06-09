(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val extract_statements :
  (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (Loc.t, Loc.t) Flow_ast.Statement.t list

val allow_refactor_extraction :
  (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (Loc.t, Loc.t) Flow_ast.Statement.t list -> bool

val create_extracted_function :
  (Loc.t, Loc.t) Flow_ast.Statement.t list -> (Loc.t, Loc.t) Flow_ast.Function.t

val replace_statements_with_new_function_call :
  (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t list -> (Loc.t, Loc.t) Flow_ast.Program.t option
