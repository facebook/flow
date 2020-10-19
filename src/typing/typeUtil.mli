(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

val reason_of_t : Type.t -> reason

val reason_of_defer_use_t : Type.defer_use_t -> reason

val reason_of_use_t : Type.use_t -> reason

val reason_of_t_add_id : Type.t -> reason

val reason_of_use_t_add_id : Type.use_t -> reason

val desc_of_t : Type.t -> reason_desc

val loc_of_t : Type.t -> ALoc.t

val def_loc_of_t : Type.t -> ALoc.t

val mod_reason_of_t : (reason -> reason) -> Type.t -> Type.t

val mod_reason_of_defer_use_t : (reason -> reason) -> Type.defer_use_t -> Type.defer_use_t

val mod_reason_of_use_t : (reason -> reason) -> Type.use_t -> Type.use_t

val mod_reason_of_opt_use_t : (reason -> reason) -> Type.opt_use_t -> Type.opt_use_t

val use_op_of_use_t : Type.use_t -> Type.use_op option

val mod_use_op_of_use_t : (Type.use_op -> Type.use_op) -> Type.use_t -> Type.use_t

val mod_root_of_use_op : (Type.root_use_op -> Type.root_use_op) -> Type.use_op -> Type.use_op

val mod_loc_of_virtual_use_op : ('a -> 'b) -> 'a Type.virtual_use_op -> 'b Type.virtual_use_op

val reasonless_compare : Type.t -> Type.t -> int

val reasonless_eq : Type.t -> Type.t -> bool

val literal_eq : string -> string Type.literal -> bool

val number_literal_eq : Type.number_literal -> Type.number_literal Type.literal -> bool

val boolean_literal_eq : bool -> bool option -> bool

val quick_subtype : bool -> Type.t -> Type.t -> bool

val reason_of_propref : Type.propref -> reason

val tuple_length : reason -> Trust.trust_rep -> Type.t list -> Type.t

val optional : ?annot_loc:ALoc.t -> ?use_desc:bool -> Type.t -> Type.t

val maybe : Type.t -> Type.t

val exact : Type.t -> Type.t

val class_type : ?structural:bool -> ?annot_loc:ALoc.t -> Type.t -> Type.t

val this_class_type : Type.t -> bool -> Type.t

val extends_type : reason -> Type.t -> Type.t -> Type.t

val extends_use_type : Type.use_op -> Type.t -> Type.t -> Type.use_t

val poly_type : Type.Poly.id -> ALoc.t -> Type.typeparam Nel.t -> Type.t -> Type.t

val poly_type_of_tparam_list : Type.Poly.id -> ALoc.t -> Type.typeparam list -> Type.t -> Type.t

val poly_type_of_tparams : Type.Poly.id -> Type.typeparams -> Type.t -> Type.t

val typeapp : reason -> Type.t -> Type.t list -> Type.t

val typeapp_annot : ALoc.t -> Type.t -> Type.t list -> Type.t

val implicit_typeapp : ?annot_loc:ALoc.t -> Type.t -> Type.t list -> Type.t

val this_typeapp : ?annot_loc:ALoc.t -> Type.t -> Type.t -> Type.t list option -> Type.t

val push_type_alias_reason : reason -> Type.t -> Type.t

val pred_map_implies : Type.predicate Key_map.t -> Type.predicate Key_map.t -> bool
