(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val builtin_ordinary_name_set : t -> SSet.t

val get_builtin_value_opt : t -> string -> Type.t option

val get_builtin_type_opt : t -> string -> Type.t option

val get_builtin_module_opt : t -> string -> Type.t option

val of_name_map :
  mapper:(Type.t_out -> Type.t_out) ->
  values:Type.t_out lazy_t SMap.t ->
  types:Type.t_out lazy_t SMap.t ->
  modules:Type.t_out lazy_t SMap.t ->
  t

val empty : unit -> t
