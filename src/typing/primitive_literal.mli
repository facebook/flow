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

val enclosing_context_needs_precise : Enclosing_context.enclosing_context -> bool

type singleton_action =
  | DoNotKeep of { use_sound_type: bool }  (** Generalize singleton type *)
  | KeepAsIs
      (** Keep singleton type and `from_annot` value to `false`. This is used to
          avoid generalizing singleton types that are checked against annotations
          where the added precision is useful. *)
  | KeepAsConst
      (** Keep singleton type, but change `from_annot` to `true`.
          Used for 'const' type parameter conversion. *)

(** Walk a literal type and replaces singleton types that originate from literals
    according to `singleton_action`. *)
val convert_literal_type :
  Context.t -> singleton_action:(ALoc.t -> singleton_action) -> Type.t -> Type.t

val convert_implicit_instantiation_literal_type :
  Context.t -> singleton_action:(ALoc.t -> singleton_action) -> Type.t -> Type.t

(** [convert_literal_type_to_const ~loc_range cx t] converts a type `t` inferred
    for a literal expression to its 'const' form. This is the form expected for
    const type parameters (for example). It only descends down literal array and
    object containers that are defined within the `loc_range` parameter, and
    converts them to readonly array and object types. For primitive values it
    applies the `KeepAsConst` action. *)
val convert_literal_type_to_const : loc_range:ALoc.t -> Context.t -> Type.t -> Type.t

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
