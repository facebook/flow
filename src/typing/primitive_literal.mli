(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type syntactic_flags = {
  encl_ctx: Enclosing_context.enclosing_context;
  decl: Ast.Variable.kind option;
  as_const: bool;
  frozen: Type.frozen_kind;
  has_hint: bool Lazy.t;
}

val empty_syntactic_flags : syntactic_flags

val mk_syntactic_flags :
  ?encl_ctx:Enclosing_context.enclosing_context ->
  ?decl:Ast.Variable.kind ->
  ?as_const:bool ->
  ?frozen:Type.frozen_kind ->
  ?has_hint:bool Lazy.t ->
  unit ->
  syntactic_flags

val generalize_singletons : Context.t -> force_general:bool -> Type.t -> Type.t

val loc_has_hint : Context.t -> ALoc.t -> bool

val primitive_literal :
  Context.t ->
  Reason.t ->
  syntactic_flags ->
  legacy:(unit -> Type.t) ->
  precise:(unit -> Type.t) ->
  general:(unit -> Type.t) ->
  ALoc.t ->
  Type.t

val try_generalize : Context.t -> syntactic_flags -> ALoc.t -> Type.t -> Type.t
