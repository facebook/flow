(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty_normalizer_env

val from_scheme :
  options:options -> genv:genv -> Type.TypeScheme.t -> (Ty.elt, Ty_normalizer.error) result

val from_scheme_with_found_computed_type :
  options:options -> genv:genv -> Type.TypeScheme.t -> (Ty.elt, Ty_normalizer.error) result * bool

(* The following differ from mapping `from_type` on each input as it folds over
   the input elements of the input propagating the state (caches) after each
   transformation to the next element. *)
val from_types :
  options:options ->
  genv:genv ->
  ('a * Type.t) list ->
  ('a * (Ty.elt, Ty_normalizer.error) result) list

val from_schemes :
  options:options ->
  genv:genv ->
  ('a * Type.TypeScheme.t) list ->
  ('a * (Ty.elt, Ty_normalizer.error) result) list

val expand_members :
  force_instance:bool ->
  options:options ->
  genv:genv ->
  Type.TypeScheme.t ->
  (Ty.t, Ty_normalizer.error) result

val expand_literal_union :
  options:options -> genv:genv -> Type.TypeScheme.t -> (Ty.t, Ty_normalizer.error) result

(* A debugging facility for getting quick string representations of Type.t *)
val debug_string_of_t : Context.t -> Type.t -> string
