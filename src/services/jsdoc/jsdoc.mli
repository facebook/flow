(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

(*************)
(* accessors *)
(*************)

val description : t -> string

(***********)
(* parsing *)
(***********)

val of_comments : ('M, 'T) Flow_ast.Syntax.t option -> t option
