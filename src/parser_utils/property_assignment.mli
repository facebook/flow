(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* NOTE: This is a WIP and should not be used for anything yet *)

module Ast = Flow_ast

val public_property : 'loc -> ('loc, 'loc) Ast.Identifier.t -> ('loc, 'loc) Ast.Identifier.t
val private_property : 'loc -> 'loc Ast.PrivateName.t -> ('loc, 'loc) Ast.Identifier.t

val eval_property_assignment :
  (ALoc.t, ALoc.t) Ast.Identifier.t list ->
  (ALoc.t, ALoc.t) Ast.Statement.Block.t ->
  (
    (ALoc.t, ALoc.t) Ast.Identifier.t list *
    ALoc.t list
  )
