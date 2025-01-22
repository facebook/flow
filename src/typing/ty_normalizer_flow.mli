(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty_normalizer_env

val from_type : genv -> Type.t -> (Ty.elt, Ty_normalizer.error) result

val from_module_type : genv -> Type.moduletype -> (Ty.decl, Ty_normalizer.error) result

val mk_genv :
  options:options ->
  cx:Context.t ->
  typed_ast_opt:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t option ->
  file_sig:File_sig.t ->
  genv

val from_type_with_found_computed_type :
  genv -> Type.t -> (Ty.elt, Ty_normalizer.error) result * bool

(* The following differ from mapping `from_type` on each input as it folds over
   the input elements of the input propagating the state (caches) after each
   transformation to the next element. *)
val from_types :
  ?f:('a -> unit) -> genv -> ('a * Type.t) list -> ('a * (Ty.elt, Ty_normalizer.error) result) list

val expand_members :
  force_instance:bool ->
  ?allowed_prop_names:Reason.name list ->
  genv ->
  Type.t ->
  (Ty.t, Ty_normalizer.error) result

val expand_literal_union : genv -> Type.t -> (Ty.t, Ty_normalizer.error) result

(* A debugging facility for getting quick string representations of Type.t *)
val debug_string_of_t : Context.t -> Type.t -> string
