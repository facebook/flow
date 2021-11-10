(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Binding of Type.tvar
  | OpenResolved
  | OpenUnresolved of int option * Reason.reason * Type.ident

val collect_of_types : ?log_unresolved:int -> Context.t -> t IMap.t -> Type.t list -> t IMap.t

val collect_of_type : ?log_unresolved:int -> Context.t -> t IMap.t -> Type.t -> t IMap.t

val collect_of_use : ?log_unresolved:int -> Context.t -> t IMap.t -> Type.use_t -> t IMap.t
