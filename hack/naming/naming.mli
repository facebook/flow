(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(** Module "naming" a program.
 * The naming phase consists in several things
 * 1- get all the global names
 * 2- transform all the local names into a unique identifier
 *
*)
open Utils

(* The environment needed to do the naming. The key of the SMap corresponds
 * to the original name of the entity, e.g. "foo", that is then
 * mapped here in ifuns to a freshly created unique integer identifier.
 *)
type env = {
  itcopt    : TypecheckerOptions.t;
  iclasses  : ((Pos.t * Ident.t) SMap.t) * (String.t SMap.t);
  ifuns     : ((Pos.t * Ident.t) SMap.t) * (String.t SMap.t);
  itypedefs : (Pos.t * Ident.t) SMap.t;
  iconsts   : (Pos.t * Ident.t) SMap.t;
}

(* Canonicalizes a key *)
val canon_key: String.t -> String.t

(* The empty naming environment *)
val empty: TypecheckerOptions.t -> env

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

(* Access the typechecker options from the env *)
val typechecker_options: env -> TypecheckerOptions.t

(* Solves the local names within a function *)
val fun_: env -> Ast.fun_ -> Nast.fun_

(* Uses a default empty environment to extract the use list
  of a lambda expression. This exists only for the sake of
  the dehackificator and is not meant for general use. *)
val uselist_lambda: Ast.fun_ -> string list

(* Solves the local names of a class *)
val class_: env -> Ast.class_ -> Nast.class_

(* Solves the local names in a function body *)
val func_body: env -> Nast.fun_ -> Nast.func_named_body

(* Solves the local names in class method bodies *)
val class_meth_bodies: env -> Nast.class_ -> Nast.class_

(* Solves the local names in an typedef *)
val typedef: env -> Ast.typedef -> Nast.typedef

(* Solves the local names in a global constant definition *)
val global_const: env -> Ast.gconst -> Nast.gconst

type fun_set = SSet.t
type class_set = SSet.t
type typedef_set = SSet.t
type const_set = SSet.t
type decl_set = fun_set * class_set * typedef_set * const_set

(* Removing declarations *)
val remove_decls: env -> decl_set -> env

val get_classes: env -> string list

(* Contains the set of test functions built-in PHP
 * Relevant to typing (is_int, is_null etc ...)
*)
val predef_tests: SSet.t

(* Bunch of predefined functions *)
val is_int:    string
val is_bool:   string
val is_array:  string
val is_float:  string
val is_string: string
val is_null:   string
val is_resource:  string

val ndecl_file:
  Relative_path.t -> FileInfo.t ->
  Errors.t * Relative_path.Set.t * env ->
  Errors.t * Relative_path.Set.t * env
