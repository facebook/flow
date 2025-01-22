(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val builtin_ordinary_name_set : t -> SSet.t

val get_builtin_value_opt : t -> string -> (ALoc.t * Type.t) option

val get_builtin_type_opt : t -> string -> (ALoc.t * Type.t) option

val get_builtin_module_opt : t -> string -> (Reason.t * Type.moduletype Lazy.t) option

val of_name_map :
  type_mapper:(Type.t -> Type.t) ->
  module_type_mapper:(Type.moduletype -> Type.moduletype) ->
  values:(ALoc.t * Type.t) lazy_t SMap.t ->
  types:(ALoc.t * Type.t) lazy_t SMap.t ->
  modules:(Reason.t * Type.moduletype Lazy.t) Lazy.t SMap.t ->
  t

val empty : unit -> t
