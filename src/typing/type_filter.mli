(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type filter_result =
  | TypeFilterResult of {
      type_: Type.t;
      changed: bool;
    }

val exists : Context.t -> Type.t -> filter_result

val not_exists : Context.t -> Type.t -> filter_result

val maybe : Context.t -> Type.t -> filter_result

val not_maybe : Context.t -> Type.t -> filter_result

val null : Type.t -> filter_result

val not_null : Context.t -> Type.t -> filter_result

val undefined : Type.t -> filter_result

val not_undefined : Context.t -> Type.t -> filter_result

val string_literal : ALoc.t -> bool -> Reason.name -> Type.t -> filter_result

val not_string_literal : Reason.name -> Type.t -> filter_result

val number_literal : ALoc.t -> bool -> Type.number_literal -> Type.t -> filter_result

val not_number_literal : Type.number_literal -> Type.t -> filter_result

val bigint_literal : ALoc.t -> bool -> Type.bigint_literal -> Type.t -> filter_result

val not_bigint_literal : Type.bigint_literal -> Type.t -> filter_result

val true_ : Type.t -> filter_result

val not_true : Type.t -> filter_result

val false_ : Type.t -> filter_result

val not_false : Type.t -> filter_result

val boolean : ALoc.t -> Type.t -> filter_result

val not_boolean : Type.t -> filter_result

val string : ALoc.t -> Type.t -> filter_result

val not_string : Type.t -> filter_result

val symbol : ALoc.t -> Type.t -> filter_result

val not_symbol : Type.t -> filter_result

val number : ALoc.t -> Type.t -> filter_result

val not_number : Type.t -> filter_result

val bigint : ALoc.t -> Type.t -> filter_result

val not_bigint : Type.t -> filter_result

val object_ : Context.t -> Type.t -> filter_result

val not_object : Type.t -> filter_result

val function_ : Type.t -> filter_result

val not_function : Type.t -> filter_result

val array : Type.t -> filter_result

val not_array : Type.t -> filter_result

val sentinel_refinement :
  Type.t -> Reason.t -> Type.t -> bool -> Type.UnionEnum.star -> filter_result

module TypeTag : sig
  type t
end

module TypeTagSet : Flow_set.S with type elt = TypeTag.t

val tag_of_t : Context.t -> Type.t -> TypeTagSet.t option

val tags_overlap : TypeTagSet.t -> TypeTagSet.t -> bool
