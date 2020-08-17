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
  [@@deriving show, eq]

  type info = {
    description: string option;
    optional: optionality;
  }
  [@@deriving show, eq]

  type path =
    | Name
    | Element of path
    | Member of path * string
  [@@deriving show, eq]

  type t = (path * info) list [@@deriving show, eq]
end

module Params : sig
  type t = (string * Param.t) list [@@deriving show, eq]
end

type t

(*************)
(* accessors *)
(*************)

val description : t -> string option

val params : t -> Params.t

(***********)
(* parsing *)
(***********)

val of_comments : ('M, 'T) Flow_ast.Syntax.t option -> t option
