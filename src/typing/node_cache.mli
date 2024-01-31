(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type cache

type t = cache ref

val mk_empty : unit -> t

val set_annotation : t -> ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.Type.t -> unit

val set_expression : t -> (ALoc.t, ALoc.t * Type.t) Ast.Expression.t -> unit

val set_statement : t -> (ALoc.t, ALoc.t * Type.t) Ast.Statement.t -> unit

val set_jsx_children :
  t -> Type.unresolved_param list * (ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.child list) -> unit

val set_function : t -> ALoc.t -> Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.t -> unit

val set_function_sig :
  t ->
  ALoc.t ->
  Func_class_sig_types.Func_stmt_sig_types.t
  * ((ALoc.t, ALoc.t * Type.t) Ast.Function.Params.t ->
    (ALoc.t, ALoc.t * Type.t) Ast.Function.body ->
    Type.t ->
    (ALoc.t, ALoc.t * Type.t) Ast.Function.t
    ) ->
  unit

val set_alias : t -> ALoc.t -> Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.TypeAlias.t -> unit

val set_opaque :
  t -> ALoc.t -> Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.OpaqueType.t -> unit

val set_interface :
  t -> ALoc.t -> Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.Interface.t -> unit

val set_declared_class :
  t -> ALoc.t -> Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareClass.t -> unit

val set_declared_component :
  t -> ALoc.t -> Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareComponent.t -> unit

val set_declared_namespace :
  t -> ALoc.t -> Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareNamespace.t -> unit

val set_class : t -> ALoc.t -> Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Class.t -> unit

val set_class_sig :
  t ->
  ALoc.t ->
  Type.t
  * Type.t
  * Func_class_sig_types.Class_stmt_sig_types.t
  * (Type.t -> (ALoc.t, ALoc.t * Type.t) Ast.Class.t) ->
  unit

val set_tparam :
  t -> (ALoc.t, ALoc.t * Type.t) Ast.Type.TypeParam.t * Type.typeparam * Type.t -> unit

val set_component_sig :
  t ->
  ALoc.t ->
  Component_sig_types.Component_declaration_sig_types.t
  * ((ALoc.t, ALoc.t * Type.t) Ast.Statement.ComponentDeclaration.Params.t ->
    ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.Block.t ->
    Type.t ->
    (ALoc.t, ALoc.t * Type.t) Ast.Statement.ComponentDeclaration.t
    ) ->
  unit

val get_annotation : t -> ALoc.t -> (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation option

val get_expression : t -> ALoc.t -> (ALoc.t, ALoc.t * Type.t) Ast.Expression.t option

val get_statement : t -> ALoc.t -> (ALoc.t, ALoc.t * Type.t) Ast.Statement.t option

val get_jsx_children :
  t ->
  ALoc.t ->
  (Type.unresolved_param list * (ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.child list)) option

val get_function_sig :
  t ->
  ALoc.t ->
  ( Func_class_sig_types.Func_stmt_sig_types.t
  * ((ALoc.t, ALoc.t * Type.t) Ast.Function.Params.t ->
    (ALoc.t, ALoc.t * Type.t) Ast.Function.body ->
    Type.t ->
    (ALoc.t, ALoc.t * Type.t) Ast.Function.t
    )
  )
  option

val get_function : t -> ALoc.t -> (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.t) option

val get_alias : t -> ALoc.t -> (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.TypeAlias.t) option

val get_opaque :
  t -> ALoc.t -> (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.OpaqueType.t) option

val get_interface :
  t -> ALoc.t -> (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.Interface.t) option

val get_declared_class :
  t -> ALoc.t -> (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareClass.t) option

val get_declared_component :
  t -> ALoc.t -> (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareComponent.t) option

val get_declared_namespace :
  t -> ALoc.t -> (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareNamespace.t) option

val get_class : t -> ALoc.t -> (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Class.t) option

val get_class_sig :
  t ->
  ALoc.t ->
  ( Type.t
  * Type.t
  * Func_class_sig_types.Class_stmt_sig_types.t
  * (Type.t -> (ALoc.t, ALoc.t * Type.t) Ast.Class.t)
  )
  option

val get_tparam :
  t -> ALoc.t -> ((ALoc.t, ALoc.t * Type.t) Ast.Type.TypeParam.t * Type.typeparam * Type.t) option

val get_component_sig :
  t ->
  ALoc.t ->
  ( Component_sig_types.Component_declaration_sig_types.t
  * ((ALoc.t, ALoc.t * Type.t) Ast.Statement.ComponentDeclaration.Params.t ->
    ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.Block.t ->
    Type.t ->
    (ALoc.t, ALoc.t * Type.t) Ast.Statement.ComponentDeclaration.t
    )
  )
  option
