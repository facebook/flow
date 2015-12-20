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
type env = {
  iclasses  : ((Pos.t * Ident.t) SMap.t) * (String.t SMap.t);
  ifuns     : ((Pos.t * Ident.t) SMap.t) * (String.t SMap.t);
  itypedefs : (Pos.t * Ident.t) SMap.t;
  iconsts   : (Pos.t * Ident.t) SMap.t;
}

(* The empty naming environment *)
val empty: env

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
    env ->
      funs:Ast.id list ->
      classes:Ast.id list ->
      typedefs:Ast.id list ->
      consts:Ast.id list -> env

type fun_set = SSet.t
type class_set = SSet.t
type typedef_set = SSet.t
type const_set = SSet.t
type decl_set = fun_set * class_set * typedef_set * const_set

(* Removing declarations *)
val remove_decls: env -> decl_set -> env

val ndecl_file:
  Relative_path.t -> FileInfo.t -> env ->
  Errors.t * Relative_path.Set.t * env
