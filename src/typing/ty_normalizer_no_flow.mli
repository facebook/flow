(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty_normalizer_env

val from_type : genv -> Type.t -> (Ty.elt, Ty_normalizer.error) result

val mk_default_genv : ?options:Ty_normalizer_env.options -> Context.t -> genv

(**
 * A debugging facility for getting quick string representations of Type.t.
 * Should not be used in any user visible code.
 *)
val debug_string_of_t : Context.t -> Type.t -> string

val type_to_desc_for_errors : genv:genv -> Type.t -> (Ty.t, Reason.reason_desc) result
