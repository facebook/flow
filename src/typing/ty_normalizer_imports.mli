(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val extract_schemes :
  Context.t ->
  File_sig.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t option ->
  (string * ALoc.t * Ty.import_mode * Type.TypeScheme.t) list
