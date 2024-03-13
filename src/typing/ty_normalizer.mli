(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Env = Ty_normalizer_env
module T = Type

(* Error reporting *)

type error_kind =
  | BadMethodType
  | BadCallProp
  | BadClassT
  | BadMappedType
  | BadThisClassT
  | BadPoly
  | BadTypeAlias
  | BadTypeApp
  | BadInlineInterfaceExtends
  | BadInternalT
  | BadInstanceT
  | BadEvalT
  | BadUse
  | ShadowTypeParam
  | SyntheticBoundT
  | UnexpectedTypeCtor of string
  | UnsupportedTypeCtor
  | UnsupportedUseCtor
  | RecursionLimit

type error = error_kind * string

val error_kind_to_string : error_kind -> string

val error_to_string : error -> string

module type S = sig
  module State : sig
    type t

    val empty : t

    val found_computed_type : t -> bool
  end

  val run_type : genv:Env.genv -> State.t -> Type.t -> (Ty.elt, error) result * State.t

  val normalize_imports :
    Context.t ->
    File_sig.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t option ->
    Env.options ->
    (string * ALoc.t * Ty.import_mode * Type.t) list ->
    Ty.imported_ident Loc_collections.ALocMap.t

  val run_expand_members :
    force_instance:bool -> genv:Env.genv -> State.t -> Type.t -> (Ty.t, error) result * State.t

  val run_expand_literal_union :
    genv:Env.genv -> State.t -> Type.t -> (Ty.t, error) result * State.t
end

module type INPUT = sig
  val eval :
    Context.t ->
    should_eval:bool ->
    cont:(Type.t -> 'a) ->
    default:(Type.t -> 'a) ->
    non_eval:(Type.t -> Type.destructor -> 'a) ->
    Type.t * Type.defer_use_t * Type.Eval.id ->
    'a

  val keys :
    Context.t ->
    should_evaluate:bool ->
    cont:(Type.t -> 'a) ->
    default:(unit -> 'a) ->
    Reason.t ->
    Type.t ->
    'a

  val typeapp :
    Context.t ->
    cont:(Type.t -> 'a) ->
    type_:(Type.t -> 'a) ->
    app:('a -> 'a list -> 'a) ->
    from_value:bool ->
    Reason.t ->
    Type.t ->
    Type.t list ->
    'a

  val builtin_type : Context.t -> cont:(Type.t -> 'a) -> Reason.t -> string -> 'a

  val builtin_typeapp :
    Context.t ->
    cont:(Type.t -> 'a) ->
    type_:(Type.t -> 'a) ->
    app:('a -> 'a list -> 'a) ->
    Reason.t ->
    string ->
    Type.t list ->
    'a
end

module Make (_ : INPUT) : S

module Lookahead : sig
  type t =
    | Recursive
    | LowerBounds of Type.t list

  exception RecursiveExn

  val peek : Context.t -> Type.t -> t
end
