(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id

val compare_id : id -> id -> int

val equal_id : id -> id -> bool

val id_of_int : int -> id

val id_as_int : id -> int option

val id_of_aloc_id : ALoc.id -> id

val string_of_id : id -> string

val generate_id : unit -> id
