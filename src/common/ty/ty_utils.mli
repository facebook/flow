(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Returns the number of nodes in a type. Will return None if the number of nodes
 * exceeds the max parameter.
 *)
val size_of_type : ?max:int -> Ty.t -> int option

val simplify_type : merge_kinds:bool -> ?sort:bool -> Ty.t -> Ty.t

val simplify_elt : merge_kinds:bool -> ?sort:bool -> Ty.elt -> Ty.elt

val symbols_of_type : Ty.t -> Ty_symbol.symbol list
