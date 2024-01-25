(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val visit_program : Context.t -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t -> unit
