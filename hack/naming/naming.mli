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
 * Transform all the local names into a unique identifier
 *)

type env = TypecheckerOptions.t * NamingGlobal.GEnv.t

(* The empty naming environment *)
val empty: TypecheckerOptions.t -> env

(* Access the typechecker options from the env *)
val typechecker_options: env -> TypecheckerOptions.t

val make_env:
    env ->
      funs:Ast.id list ->
      classes:Ast.id list ->
      typedefs:Ast.id list ->
      consts:Ast.id list -> env

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
