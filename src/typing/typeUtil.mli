(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

module TypeExSet : Flow_set.S with type elt = Type.t

val literal_eq : 'a -> 'a Type.literal -> bool

val number_literal_eq : Type.number_literal -> Type.number_literal Type.literal -> bool

val bigint_literal_eq : Type.bigint_literal -> Type.bigint_literal Type.literal -> bool

val boolean_literal_eq : bool -> bool option -> bool

(* Under a multiplatform world, we might have two nominal constructs under two different
 * files, but they are logically the same thing. e.g. class Foo under A.ios.js and A.js.flow.
 *
 * [nominal_id_have_same_logical_module ~file_options a b] tells us whether in the multiplatform world,
 * [a] and [b] are the same logical module. *)
val nominal_id_have_same_logical_module :
  file_options:Files.options -> ALoc.id * string option -> ALoc.id * string option -> bool

val quick_subtype : Type.t -> Type.t -> bool

val reason_of_propref : Type.propref -> reason

val mk_named_prop : reason:reason -> ?from_indexed_access:bool -> name -> Type.propref

val optional : ?annot_loc:ALoc.t -> ?use_desc:bool -> Type.t -> Type.t

val maybe : Type.t -> Type.t

val make_exact_object : reason_obj:reason -> Type.objtype -> reason_op:reason -> Type.t

val class_type : ?structural:bool -> ?annot_loc:ALoc.t -> Type.t -> Type.t

val extends_type : reason -> Type.t -> Type.t -> Type.t

val extends_use_type : Type.use_op -> Type.t -> Type.t -> Type.use_t

val poly_type : Type.Poly.id -> ALoc.t -> Type.typeparam Nel.t -> Type.t -> Type.t

val poly_type_of_tparam_list : Type.Poly.id -> ALoc.t -> Type.typeparam list -> Type.t -> Type.t

val poly_type_of_tparams : Type.Poly.id -> Type.typeparams -> Type.t -> Type.t

val typeapp_with_use_op :
  from_value:bool -> use_desc:bool -> reason -> Type.use_op -> Type.t -> Type.t list -> Type.t

val typeapp : from_value:bool -> use_desc:bool -> reason -> Type.t -> Type.t list -> Type.t

val typeapp_annot : from_value:bool -> use_desc:bool -> ALoc.t -> Type.t -> Type.t list -> Type.t

val implicit_typeapp : ?annot_loc:ALoc.t -> Type.t -> Type.t list -> Type.t

val this_typeapp : ?annot_loc:ALoc.t -> Type.t -> Type.t -> Type.t list option -> Type.t

val typeof_annotation : reason -> Type.t -> Type.t list option -> Type.t

val push_type_alias_reason : reason -> Type.t -> Type.t

val pred_map_implies : Type.predicate Key_map.t -> Type.predicate Key_map.t -> bool

val type_t_of_annotated_or_inferred : Type.annotated_or_inferred -> Type.t

val map_annotated_or_inferred :
  (Type.t -> Type.t) -> Type.annotated_or_inferred -> Type.annotated_or_inferred

val union_of_ts : reason -> Type.t list -> Type.t

val union_of_ts_opt : reason -> Type.t list -> Type.t option

val annotated_or_inferred_of_option : default:Type.t -> Type.t option -> Type.annotated_or_inferred

val subtype_this_of_function : Type.funtype -> Type.t

val all_explicit_targs : Type.targ Base.List.t option -> Type.targ list option

val all_explicit_targ_ts : Type.targ Base.List.t option -> Type.t list option

val tuple_length : reason -> int * int -> Type.t

val tuple_ts_of_elements : Type.tuple_element list -> Type.t list

val mk_tuple_element :
  ?name:string -> ?optional:bool -> ?polarity:Polarity.t -> reason -> Type.t -> Type.tuple_element

val type_guard_of_predicate : Type.fun_predicate -> Type.t option

val type_guard_of_funtype : Type.funtype -> Type.t option

val mk_renders_type : reason -> Type.renders_variant -> Type.t -> Type.t
