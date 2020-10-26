(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val new_gcx : unit -> t

val mark_ids_of_type : Context.t -> t -> Type.t -> Scope_id.t -> t

val blame_ids_of_type : Context.t -> t -> Type.t -> Scope_id.t -> Reason.t -> t

val add_scope_map : t -> Scope_id.t -> Loc_collections.ALocIDSet.t -> t

val in_scope : t -> Scope_id.t -> ALoc.id -> bool

val get_id_annotation_reason : t -> Type.ident -> Reason.t option

val get_marked_ids : t -> Reason.t option IMap.t Scope_id.ScopeMap.t
