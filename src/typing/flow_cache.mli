(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FlowConstraint : sig
  val get : Context.t -> Type.t * Type.use_t -> bool
end

module Eval : sig
  val id : Context.t -> Type.t -> Type.defer_use_t -> Type.t

  val find_repos : Context.t -> Type.t -> Type.defer_use_t -> Type.Eval.id -> Type.t option

  val add_repos : Context.t -> Type.t -> Type.defer_use_t -> Type.Eval.id -> Type.t -> unit
end

module Fix : sig
  val find : Context.t -> bool -> Type.t -> Type.t option

  val add : Context.t -> bool -> Type.t -> Type.t -> unit
end

val summarize_flow_constraint : Context.t -> (string * int) list
