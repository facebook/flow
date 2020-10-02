(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Decide if a type variable appears free inside a type. This is useful for:
 *
 * - Deciding well-formedness: a type variable should not appear free in a
 *   top-level type.
 *
 * - Computing recursive types: we decide if a type is recursive, we will need
 *   to know if it appears free in its expansion. (More can be found in the type
 *   normalizer module.)
 *
 * The reason we require the is_toplevel parameter is to determine if the TypeAlias
 * body will be walked over. Typically the body is only useful when TypeAlias
 * appears as the top-level constructor, and is ignored otherwise.
 *)
val tvar_appears_in_type : is_toplevel:bool -> Ty.tvar -> Ty.t -> bool

(* Returns the number of nodes in a type. Will return None if the number of nodes
 * exceeds the max parameter.
 *)
val size_of_type : ?max:int -> Ty.t -> int option

val simplify_type : merge_kinds:bool -> ?sort:bool -> Ty.t -> Ty.t

val simplify_elt : merge_kinds:bool -> ?sort:bool -> Ty.elt -> Ty.elt

val symbols_of_type : Ty.t -> Ty_symbol.symbol list
