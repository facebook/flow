(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* collection of helpers to manipulate the AST *)
open Ast
(*
  Given a position, a function name and arguments, creates
  a call to the function.
*)
val call_func_expr_ : Pos.t -> string -> expr list -> expr_
val call_func : Pos.t -> string -> expr list -> expr

(*
  Given a position, an expression, a method name and arguments creates a
  call to an instance method on the expression
*)
val call_inst_func_expr_ : Pos.t -> expr -> string -> expr list -> expr_
val call_inst_func : Pos.t -> expr -> string -> expr list -> expr
(* variant of above, calls the function on $this *)
val call_this_func : Pos.t -> string -> expr list -> expr
(*
  Given a position, class name, method name and arguments creates a call
  to a static method on the class.
*)
val call_static_func_expr_ : Pos.t -> string -> string -> expr list -> expr_
val call_static_func : Pos.t -> string -> string -> expr list -> expr
(* variant of above, calls the function on self *)
val call_self_func : Pos.t -> string -> expr list -> expr

(* Expression pointing to var of name on this *)
val this_var : Pos.t -> string -> expr

(*
  Given a position, variable name and expression, assigns the variable
  to the expression.
*)
val assign : Pos.t -> string -> expr -> expr

(*
  Given a position, an object, property name and expression, assigns the
  property to the expression.
*)
val assign_inst : Pos.t -> expr -> string -> expr -> expr
val assign_this : Pos.t -> string -> expr -> expr

(*
  Given a position, a class, static property name and expression, assigns the
  property to the expression.
*)
val assign_static : Pos.t -> string -> string -> expr -> expr
val assign_self : Pos.t -> string -> expr -> expr

(*
  Create the unary not of any expression
*)
val negate : expr -> expr

val default_method : method_

(*
  if something is a hack collection, returns a standard name with which
  the collection can be addressed everywhere.
*)
val base_collection_str : string -> string option

(*
  returns true if a name is a hack collection
*)
val is_collection_str : string -> bool


(*
  evaluates if a given expression might be a hack collection. This
  result is a tribool, represented as a bool option.
  None indicates that its unknown if the expression is a collection.
*)

val is_collection_expr_ : expr_ -> bool option
