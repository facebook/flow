(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val check_type_guard :
  Context.t -> (ALoc.t, ALoc.t) Flow_ast.Function.Params.t -> Type.type_guard -> unit

val infer_type_guard :
  Context.t -> (ALoc.t, ALoc.t) Flow_ast.Function.Params.t -> Type.type_guard option
