(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Returns the number of nodes in a type. Will return None if the number of nodes
    exceeds the max parameter. *)
val size_of_type : ?max:int -> Ty.t -> int option

val size_of_elt : ?max:int -> Ty.elt -> int option

val simplify_type : merge_kinds:bool -> ?sort:bool -> Ty.t -> Ty.t

val simplify_decl : merge_kinds:bool -> ?sort:bool -> Ty.decl -> Ty.decl

val simplify_elt : merge_kinds:bool -> ?sort:bool -> Ty.elt -> Ty.elt

val unmaybe_ty : Ty.t -> Ty.t

val elt_equal : Ty.elt -> Ty.elt -> bool

(** Utility useful for codemods/type insertion. When the element we inferred is a
    declaration we can't directly print/insert in code. This utility helps convert
    it to an equivalent type. For example it will convert `class C` to `typeof C`,
    `enum E` to `typeof E`. *)
val typify_elt : Ty.elt -> Ty.t option

val reinterpret_elt_as_type_identifier : Ty.elt -> Ty.elt
