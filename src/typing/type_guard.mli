(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val predicate_checks :
  Context.t -> Type.type_guard -> (ALoc.t, ALoc.t) Flow_ast.Function.Params.t -> unit
