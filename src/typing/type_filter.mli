(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val exists : Context.t -> Type.t -> Type.t

val not_exists : Context.t -> Type.t -> Type.t

val maybe : Context.t -> Type.t -> Type.t

val not_maybe : Context.t -> Type.t -> Type.t

val null : Type.t -> Type.t

val not_null : Context.t -> Type.t -> Type.t

val undefined : Type.t -> Type.t

val not_undefined : Context.t -> Type.t -> Type.t

val string_literal : ALoc.t -> bool -> Reason.name -> Type.t -> Type.t

val not_string_literal : Reason.name -> Type.t -> Type.t

val number_literal : ALoc.t -> bool -> Type.number_literal -> Type.t -> Type.t

val not_number_literal : Type.number_literal -> Type.t -> Type.t

val bigint_literal : ALoc.t -> bool -> Type.bigint_literal -> Type.t -> Type.t

val not_bigint_literal : Type.bigint_literal -> Type.t -> Type.t

val true_ : Type.t -> Type.t

val not_true : Type.t -> Type.t

val false_ : Type.t -> Type.t

val not_false : Type.t -> Type.t

val boolean : ALoc.t -> Type.t -> Type.t

val not_boolean : Type.t -> Type.t

val string : ALoc.t -> Type.t -> Type.t

val not_string : Type.t -> Type.t

val symbol : ALoc.t -> Type.t -> Type.t

val not_symbol : Type.t -> Type.t

val number : ALoc.t -> Type.t -> Type.t

val not_number : Type.t -> Type.t

val bigint : ALoc.t -> Type.t -> Type.t

val not_bigint : Type.t -> Type.t

val object_ : Context.t -> Type.t -> Type.t

val not_object : Type.t -> Type.t

val function_ : Type.t -> Type.t

val not_function : Type.t -> Type.t

val array : Type.t -> Type.t

val not_array : Type.t -> Type.t

val sentinel_refinement : Type.t -> Reason.t -> Type.t -> bool -> Type.UnionEnum.star -> Type.t

module TypeTag : sig
  type t
end

module TypeTagSet : Flow_set.S with type elt = TypeTag.t

val tag_of_t : Context.t -> Type.t -> TypeTagSet.t option

val tags_overlap : TypeTagSet.t -> TypeTagSet.t -> bool
