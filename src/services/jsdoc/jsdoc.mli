(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Param : sig
  type optionality =
    | NotOptional
    | Optional
    | OptionalWithDefault of string
  [@@deriving ord, show, eq]

  type info = {
    description: string option;
    optional: optionality;
  }
  [@@deriving ord, show, eq]

  type path =
    | Name
    | Element of path
    | Member of path * string
  [@@deriving ord, show, eq]
end

type t

(*************)
(* accessors *)
(*************)

val description : t -> string option

val param : t -> string -> Param.path -> Param.info option

(***********)
(* parsing *)
(***********)

val of_comments : ('M, 'T) Flow_ast.Syntax.t option -> t option
