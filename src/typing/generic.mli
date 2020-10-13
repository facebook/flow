(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id

type sat_result =
  | Satisfied
  | Lower of id
  | Upper of id

val to_string : id -> string

val equal_id : id -> id -> bool

val collapse : id -> id -> id option

val make_bound_id : ALoc.id -> string -> id

val aloc_of_id : id -> ALoc.id

val satisfies : id -> id -> sat_result
