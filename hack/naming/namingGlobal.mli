(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(** Module "naming" a program.
 * Get all the global names
 *)
open Utils

(* The environment needed to do the naming. The key of the SMap corresponds
 * to the original name of the entity, e.g. "foo", that is then
 * mapped here in ifuns to a freshly created unique integer identifier.
 *)
module GEnv: sig
  val class_id: string -> (Pos.t * Ident.t) option
  val class_canon_name: string -> string option

  val fun_id: string -> (Pos.t * Ident.t) option
  val fun_canon_name: string -> string option

  val typedef_id: string -> (Pos.t * Ident.t) option

  val gconst_id: string -> (Pos.t * Ident.t) option
end

(* Canonicalizes a key *)
val canon_key: String.t -> String.t

(* Function building the original naming environment.
 * This pass "declares" all the global names. The only checks done
 * here are whether an entity name was already bound (e.g. when
 * two files define the same function).
 * It takes an old environment and add all the entities
 * passed as parameters to this old environment.
*)
val make_env:
      funs:Ast.id list ->
      classes:Ast.id list ->
      typedefs:Ast.id list ->
      consts:Ast.id list -> unit

(* Removing declarations *)
val remove_decls:
      funs: SSet.t ->
      classes: SSet.t ->
      typedefs: SSet.t ->
      consts: SSet.t -> unit

val ndecl_file:
  Relative_path.t -> FileInfo.t ->
  Errors.t * Relative_path.Set.t
