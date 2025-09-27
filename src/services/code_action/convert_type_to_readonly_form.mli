(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type conversion_kind =
  | ConversionToReadOnlyArray
  | ConversionToReadOnlyObject
  | ConversionToReadOnlyMap
  | ConversionToReadOnlySet

val convert :
  ts_readonly_name:bool ->
  (Loc.t, Loc.t) Ast.Program.t ->
  Loc.t ->
  ((Loc.t, Loc.t) Ast.Program.t * conversion_kind) option
