(**
 * Copyright (c) 2013-present, Facebook, Inc.
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
val set: t -> Loc.t -> Type.t -> unit
val set_targ: t -> Loc.t -> Type.t -> unit
val set_info: ?extra_tparams:Type.typeparam list -> Loc.t -> type_entry -> t -> unit
val fold_coverage: (Loc.t -> Type.TypeScheme.t -> 'a -> 'a) -> t -> 'a -> 'a
val find_unsafe_coverage: t -> Loc.t -> Type.TypeScheme.t
val find_unsafe_coverage_type: t -> Loc.t -> Type.t
val find_unsafe_targ: t -> Loc.t -> Type.TypeScheme.t
val reset: t -> unit
val copy: t -> t
val with_typeparams: Type.typeparam list -> t -> (unit -> 'a) -> 'a
val find_type_info_with_pred: t -> (Loc.t -> bool) -> (Loc.t * scheme_entry) option
val function_decl_loc : (Loc.t * 'a) option -> Loc.t -> Loc.t
val targs_hashtbl: t -> (Loc.t, Type.TypeScheme.t) Hashtbl.t
val targs_to_list: t -> (Loc.t * Type.TypeScheme.t) list
val coverage_to_list: t -> (Loc.t * Type.TypeScheme.t) list
val coverage_hashtbl: t -> (Loc.t, Type.TypeScheme.t) Hashtbl.t
val type_info_hashtbl: t -> (Loc.t, scheme_entry) Hashtbl.t
