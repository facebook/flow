(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module type S = sig
  module Env : Env_sig.S

  module Abnormal : Abnormal_sig.S with module Env := Env

  module Import_export : module type of Import_export.Make (Env)

  module Toplevels : module type of Toplevels.DependencyToplevels (Env) (Abnormal)

  val expression :
    ?cond:Type.cond_context ->
    Context.t ->
    annot:unit option ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

  val statement :
    Context.t -> (ALoc.t, ALoc.t) Ast.Statement.t -> (ALoc.t, ALoc.t * Type.t) Ast.Statement.t

  val toplevel_decls : Context.t -> (ALoc.t, ALoc.t) Ast.Statement.t list -> unit
end
