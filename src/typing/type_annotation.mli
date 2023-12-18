(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

module type C = sig
  val mk_typeof_annotation : Context.t -> Reason.t -> Type.t -> Type.t

  val mk_instance :
    Context.t -> ?type_t_kind:Type.type_t_kind -> reason -> ?use_desc:bool -> Type.t -> Type.t

  val cjs_require : Context.t -> Type.t -> Reason.t -> bool -> bool -> Type.t

  val get_prop :
    Context.t -> Type.use_op -> Reason.t -> ?op_reason:Reason.t -> Reason.name -> Type.t -> Type.t

  val reposition : Context.t -> ALoc.t -> ?annot_loc:ALoc.t -> Type.t -> Type.t

  val get_builtin : Context.t -> name -> reason -> Type.t

  val obj_test_proto : Context.t -> Reason.t -> Type.t -> Type.t

  val mixin : Context.t -> Reason.t -> Type.t -> Type.t

  val subtype_check : Context.t -> Type.t -> Type.t -> unit
end

module FlowJS : C

module Annot : C

module Make (_ : C) (_ : Statement_sig.S) : Type_annotation_sig.S
