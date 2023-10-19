(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val builtin_set : t -> NameUtils.Set.t

val get_builtin_opt : t -> Reason.name -> Type.t option

val get_builtin :
  t -> Reason.name -> on_missing:(unit -> (Type.t, 'a) result) -> (Type.t, 'a) result

val set_builtin : t -> Reason.name -> Type.t lazy_t -> unit

val empty : unit -> t

val optimize_entries : t -> optimize:(Type.t -> Type.t) -> unit
