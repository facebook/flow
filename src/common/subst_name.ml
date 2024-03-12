(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type op_kind =
  | Spread
  | ReadOnly
  | Mapped
  | Partial
  | Required
  | CheckConfig
  | CreateElement
  | ReactConfig
  | Conditional
[@@deriving eq, ord, show]

module Name = struct
  (* Subst names are used to represent the names of type parameters.
     - A `Name` is just a typical name, originating directly from the source.
     - An `Id` is used when we perform a substitution where a variable needs
       to be renamed to avoid capture--this is the classic capture avoiding
       substitution problem in the lambda calculus. When we rename a variable.
       we rename it to an `Id` with the original name but also an integer,
       to remove it from free variables.
     - A `Synthetic` name is used when converting from more complex generic
       encodings such as spreads, where a single GenericT can represent more
       than one source-level generic. Such GenericTs should never be the targets
       of substitution, because they only arise through constraint solving (rather
       than in signatures where they can be replaced in substitutions). We enforce
       this by erroring if a Synthetic name is substituted.*)
  type t =
    | Name of string
    | Id of int * string
    | Synthetic of {
        name: string;
        op_kind: op_kind option;
        ts: t list;
      }
  [@@deriving eq, ord, show]
end

module Map = WrappedMap.Make (Name)
module Set = Flow_set.Make (Name)
include Name

let string_of_subst_name n =
  match n with
  | Synthetic { name = n; _ }
  | Name n
  | Id (_, n) ->
    n

(** Similar to [string_of_subst_name] but uses backticks (code formatting) for
 * non-synthetic names. *)
let formatted_string_of_subst_name n =
  match n with
  | Synthetic { name = n; _ } -> n
  | Name n
  | Id (_, n) ->
    Utils_js.spf "`%s`" n
