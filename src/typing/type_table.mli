(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id_kind =
  | PropertyAccess of Type.t (* receiver type *)
  | Other

type id_info = string (* name *) * Type.t * id_kind

type t

val create: unit -> t
val set: t -> Loc.t -> Type.t -> unit
val set_info: t -> Loc.t -> id_info -> unit
val fold_coverage: (Loc.t -> Type.t -> 'a -> 'a) -> t -> 'a -> 'a
val find_unsafe_coverage: t -> Loc.t -> Type.t
val reset: t -> unit
val copy: t -> t
val find_type_info: pred:(Loc.t -> bool) -> t -> (Loc.t * id_info) option
val coverage_to_list: t -> (Loc.t * Type.t) list
