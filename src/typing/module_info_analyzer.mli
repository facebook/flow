(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Returns the signature location of the module and the module type *)
val analyze_program : Context.t -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t -> ALoc.t * Type.t

val analyze_declare_namespace :
  Context.t -> Reason.t -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t list -> Type.t
