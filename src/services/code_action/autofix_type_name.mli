(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val convert_type :
  incorrect_name:string ->
  replacement_name:string ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t

val convert_incorrect_type :
  Flow_intermediate_error_types.IncorrectType.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t
