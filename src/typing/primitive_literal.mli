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

val is_generalization_candidate : Context.t -> Type.t -> bool

type singleton_action =
  | DoNotKeep of { use_sound_type: bool }  (** Generalize singleton type *)
  | KeepAsIs
      (** Keep singleton type and `from_annot` value to `false`. This is used to
          avoid generalizing singleton types that are checked against annotations
          where the added precision is useful. *)

(** Walk a literal type and replaces singleton types that originate from literals
    according to `singleton_action`. *)
val convert_literal_type :
  Context.t -> singleton_action:(ALoc.t -> singleton_action) -> Type.t -> Type.t

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
