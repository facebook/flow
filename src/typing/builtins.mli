(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val add_not_yet_seen_builtin : t -> Reason.name -> Type.t -> unit

val get_builtin : t -> Reason.name -> on_missing:(unit -> Type.t) -> Type.t

val set_builtin : flow_t:(Type.t * Type.t -> unit) -> t -> Reason.name -> Type.t -> unit

val empty : unit -> t

val map_entries : t -> f:(Type.t -> Type.t) -> unit
