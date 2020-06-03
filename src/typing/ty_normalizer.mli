(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty_normalizer_env

type error_kind =
  | BadMethodType
  | BadBoundT
  | BadCallProp
  | BadClassT
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
  | UnexpectedTypeCtor of string
  | UnsupportedTypeCtor
  | UnsupportedUseCtor
  | TypeTooBig
  | RecursionLimit

type error = error_kind * string

val error_to_string : error -> string

val from_type : options:options -> genv:genv -> Type.t -> (Ty.elt, error) result

val from_scheme : options:options -> genv:genv -> Type.TypeScheme.t -> (Ty.elt, error) result

(* The following differ from mapping `from_type` on each input as it folds over
   the input elements of the input propagating the state (caches) after each
   transformation to the next element. *)
val from_types :
  options:options -> genv:genv -> ('a * Type.t) list -> ('a * (Ty.elt, error) result) list

val from_schemes :
  options:options ->
  genv:genv ->
  ('a * Type.TypeScheme.t) list ->
  ('a * (Ty.elt, error) result) list

val fold_hashtbl :
  options:options ->
  genv:genv ->
  f:('a -> 'loc * (Ty.elt, error) result -> 'a) ->
  g:('b -> Type.TypeScheme.t) ->
  htbl:('loc, 'b) Hashtbl.t ->
  'a ->
  'a

(* `include_proto_members` controls whether to include prototypes' members.
   `idx_hook` is a function that will be called if the given type is in an IdxWrapper;
   Feels hacky, but idx isn't super important now that we have optional chaining. *)
val expand_members :
  include_proto_members:bool ->
  idx_hook:(unit -> unit) ->
  options:options ->
  genv:genv ->
  Type.TypeScheme.t ->
  (Ty.t, error) result
