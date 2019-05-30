(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id_kind =
  | PropertyAccess of Type.t (* receiver type *)
  | Import of string (* remote name *) * Type.t (* module type *)
  | Exists
  | Other

type name = string
type scheme_entry = name * Type.TypeScheme.t * id_kind
type type_entry = name * Type.t * id_kind

type t

val create: unit -> t
val set_targ: t -> ALoc.t -> Type.t -> unit
val set_info: ?extra_tparams:Type.typeparam list -> ALoc.t -> type_entry -> t -> unit
val find_unsafe_targ: t -> ALoc.t -> Type.TypeScheme.t
val reset: t -> unit
val copy: t -> t
val with_typeparams: Type.typeparam list -> t -> (unit -> 'a) -> 'a
val find_type_info: t -> ALoc.t -> scheme_entry option
val find_type_info_with_pred: t -> (ALoc.t -> bool) -> (ALoc.t * scheme_entry) option
val targs_hashtbl: t -> (ALoc.t, Type.TypeScheme.t) Hashtbl.t
val targs_to_list: t -> (ALoc.t * Type.TypeScheme.t) list
val type_info_hashtbl: t -> (ALoc.t, scheme_entry) Hashtbl.t
